(ns com.gfredericks.org-agenda
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [print-table]]
   [clojure.stacktrace :refer [print-stack-trace]]
   [clojure.string :as string]
   [clojure.tools.logging :as log]
   [com.gfredericks.compare :as compare]
   [com.gfredericks.org-editor :as org]
   [me.raynes.fs :as fs]
   [juxt.dirwatch :refer [close-watcher watch-dir]])
  (:import
   (java.io File FileNotFoundException)
   (java.time Duration LocalDate LocalDateTime LocalTime Period ZonedDateTime ZoneId)
   (java.time.format DateTimeFormatter)
   (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(def CHICAGO (ZoneId/of "America/Chicago"))
(def DEFAULT-WARNING-PERIOD
  "same as org-deadline-warning-days"
  (Period/ofDays 14))

(def org-timestamp-regex
  #"([<\[])([-\d]{10})(?: \w{3})?(?: (\d\d:\d\d)(?:-(\d\d:\d\d))?)?(?: (\+|\+\+|\.\+)(\d+)([ymwdh]))?(?: -(\d+)([ymwdh]))?(?: [^>\]]*?)?([>\]])")

(def TODO-states #{"TODO" "BLOCKED" "DONE"})
(def DONE-states #{"DONE"})
(def TODO-state-pattern (re-pattern (format "(?:%s)" (string/join "|" TODO-states))))

;; pasted from org-editor
(def ^:private header-with-tags-regex
  #"(.*?)\s*(:(?:[^\s:]+:)+\s*)?$")

(let [formatter (DateTimeFormatter/ofPattern "'['yyyy-MM-dd EEE']'")]
  (defn format-org-local-date
    [^LocalDate local-date]
    (.format local-date formatter)))

(defn parse-org-datetime
  [s]
  (if-let [[_ _ d t] (re-matches org-timestamp-regex s)]
    (LocalDateTime/of (LocalDate/parse d) (LocalTime/parse t))
    (throw (ex-info "Bad arg to parse-org-datetime" {:arg s}))))

(defn parse-org-date-or-datetime
  [s]
  (let [[_ _ d t] (re-matches org-timestamp-regex s)
        ld (LocalDate/parse d)]
    (if t
      (LocalDateTime/of ld (LocalTime/parse t))
      ld)))

(defprotocol IToLocalDate
  (to-local-date [x]))

(extend-protocol IToLocalDate
  LocalDate
  (to-local-date [x] x)
  ZonedDateTime
  (to-local-date [x] (.toLocalDate x))
  LocalDateTime
  (to-local-date [x] (.toLocalDate x)))

(defn format-effort
  [^Duration duration]
  (let [tot-minutes (.toMinutes duration)]
    (format "%d:%02d" (quot tot-minutes 60) (mod tot-minutes 60))))

(defn all-org-files
  [dir]
  (fs/find-files dir #".*\.org$"))

(defn all-sections
  [^File file]
  (with-open [r (io/reader file)]
    (let [{::org/keys [prelude] :as parsed} (org/parse-file r)
          filewide-category (->> prelude
                                 (keep #(re-matches #"#\+CATEGORY: (.*?)\s*" %))
                                 (map second)
                                 (first))]
      (-> parsed
          (->> (tree-seq #(contains? % ::org/sections)
                         (fn [{::org/keys [header sections]
                               ::keys [tags-with-ancestors props-with-ancestors
                                       ancestor-headers]}]
                           (map (fn [section]
                                  (assoc section
                                         ::tags-with-ancestors
                                         (cons (org/read-tags (::org/header section))
                                               tags-with-ancestors)
                                         ::props-with-ancestors
                                         (cons (org/read-properties section)
                                               props-with-ancestors)
                                         ::ancestor-headers
                                         ((fnil conj []) ancestor-headers header)))
                                sections)))
               (filter ::org/header)
               (map #(assoc %
                            ::org/line-number (::org/line-number (meta %))
                            ::filewide-category filewide-category)))))))

(defn timestamp-finder
  [context-regex-format-string]
  (let [pattern (re-pattern (format context-regex-format-string org-timestamp-regex))]
    (fn [line]
      (if-let [[_ left-bracket date time time-end r-type r-num r-unit warning-period warning-period-unit right-bracket]
               (re-matches pattern line)]
        (let [base (if time
                     (let [t1 (LocalDateTime/parse (str date "T" time))]
                       (if time-end
                         (let [t2 (LocalDateTime/parse (str date "T" time-end))]
                           [t1 t2])
                         t1))
                     (if date
                       (LocalDate/parse date)))]
          (if (not (#{["<" ">"] ["[" "]"]} [left-bracket right-bracket]))
            ;; alternately we could just return nil
            (throw (ex-info "Mismatched timestamp brackets" {:line line})))
          (cond->
              {:base base
               :active? (= "<" left-bracket)}
              r-type
              (assoc :repeater [r-type (Long/parseLong r-num) r-unit])

              warning-period
              (assoc :warning-period
                     (let [wp (Long/parseLong warning-period)]
                       (case warning-period-unit
                         "y" (Period/ofYears wp)
                         "m" (Period/ofMonths wp)
                         "w" (Period/ofWeeks wp)
                         "d" (Period/ofDays wp)
                         "h" (throw (ex-info "Bad warning period" {:line line})))))))))))

(defn apply-repeater
  [[r-type r-num r-unit] base today max-date]
  (when (vector? base)
    ;; do I actually use this?
    (throw (ex-info "TODO implement combo of time ranges and repeaters" {})))
  (let [bump (case r-unit
               ;; would take a bit of effort to eliminate the
               ;; reflection here
               "d" #(.plusDays % r-num)
               "w" #(.plusWeeks % r-num)
               "m" #(.plusMonths % r-num)
               "y" #(.plusYears % r-num))
        next (case r-type
               "+" (bump base)
               "++" (->> (iterate bump base)
                         (next)
                         (drop-while #(compare/<= (to-local-date %) today))
                         (first))
               ".+" (bump
                     (let [completion-day (compare/max today (to-local-date base))]
                       (if (instance? LocalDateTime base)
                         (LocalDateTime/of completion-day (.toLocalTime base))
                         completion-day))))]
    (cons base
          (->> (iterate bump next)
               (take-while #(compare/<= (to-local-date %) max-date))))))

(defn remove-tags
  [header]
  (second (re-matches header-with-tags-regex header)))

(defn dedent
  [lines]
  (let [min-indention (->> lines
                           (map #(count (re-find #"^\s+" %)))
                           (apply min))]
    (cond->> lines
      (pos? min-indention)
      (map #(subs % min-indention)))))

(let [scheduled-finder         (timestamp-finder "\\s*SCHEDULED: %s(?: DEADLINE: .*)?")
      deadline-finder          (timestamp-finder "\\s*DEADLINE: %s(?: SCHEDULED: .*)?")
      closed-finder            (timestamp-finder "\\s*CLOSED: %s.*")
      created-at-finder        (timestamp-finder "\\s*Created at %s")
      agenda-timestamp-finder  (timestamp-finder ".*(?<!(?:SCHEDULED|DEADLINE): )(?=<)%s(?<=>).*")

      agenda-date-range-pattern
      #"<(\d{4}-\d\d-\d\d)(?: \w\w\w)?>--<(\d{4}-\d\d-\d\d)(?: \w\w\w)?>"

      header-pattern
      (re-pattern (format (str #"(\*+) (?:(%s) )?(?:(\[#[A-Z0-9]+\]) )?(.*)")
                          TODO-state-pattern))]
  (defn parse-section-for-agenda
    [{::org/keys           [header prelude sections line-number]
      ::keys [tags-with-ancestors props-with-ancestors ancestor-headers filewide-category]
      :as                  section}
     file-str
     today]
    (try
      (when (not-any? #(re-matches #"(?i)archive" %) (apply concat tags-with-ancestors))
        (let [get-timestamp (fn [finder allow-repeater? include-header?]
                              (let [ret (some->> (cond->> prelude include-header? (cons header))
                                                 (keep finder)
                                                 (first))]
                                (if (and (:repeater ret)
                                         (not allow-repeater?))
                                  (throw (ex-info "Sorry man can't have a repeater there" {}))
                                  ret)))
              max-repeater-date (.plusDays today 60)
              [_ stars todo priority-cookie rest] (re-matches header-pattern header)
              raw-header header
              header (remove-tags rest)
              effort (when-let [s (-> props-with-ancestors first (get "Effort"))]
                       (when-let [[_ hours minutes] (re-matches #"(\d+):(\d+)" s)]
                         (Duration/ofMinutes (+ (Long/parseLong minutes)
                                                (* 60 (Long/parseLong hours))))))
              scheduled (get-timestamp scheduled-finder true false)
              deadline  (get-timestamp deadline-finder true false)
              tags (reduce into #{} tags-with-ancestors)
              properties (first props-with-ancestors)
              agenda-notes (->> prelude
                                (drop-while #(not (re-matches #"\s*:AGENDA_NOTES:\s*" %)))
                                (next)
                                (take-while #(not (re-matches #"\s*:END:\s*" %)))
                                ((fn [lines]
                                   (when (seq lines)
                                     (dedent lines)))))
              clock-logs (->> prelude
                              ;; this isn't the exact logic used by
                              ;; org-mode I'm sure, but it's a good
                              ;; enough hack for now
                              (drop-while #(not (re-matches #"\s*:LOGBOOK:\s*" %)))
                              (clojure.core/rest)
                              (take-while #(not (re-matches #"\s*:END:\s*" %)))
                              (keep #(re-matches #"\s*CLOCK:\s+(\[.*?\])(?:--(\[.*?\]) =>\s+\d+:\d+)?" %))
                              (map (fn [[_ start end]]
                                     [(parse-org-datetime start)
                                      (some-> end parse-org-datetime)])))
              created-at (:base (get-timestamp created-at-finder false false))
              scheduled-repeater? (:repeater scheduled)
              deadline-repeater? (:repeater deadline)
              base (with-meta
                     {:todo               (if (contains? DONE-states todo) nil todo)
                      :done               (if (contains? DONE-states todo) todo)
                      :raw-header         raw-header
                      :header             header
                      :ancestor-headers   ancestor-headers
                      :scheduled          (:base scheduled)
                      :created-at         created-at
                      :updated-at         (or (some-> (get properties "UPDATED_AT")
                                                      parse-org-date-or-datetime)
                                              (if scheduled
                                                (or (:base scheduled) scheduled))
                                              created-at)
                      :deadline           deadline
                      :agenda-timestamp   (:base (get-timestamp agenda-timestamp-finder false true))
                      :closed-at          (:base (get-timestamp closed-finder false false))

                      :last-repeat        (some-> props-with-ancestors
                                                  first
                                                  (get "LAST_REPEAT")
                                                  parse-org-datetime)
                      :category           (-> props-with-ancestors
                                              first
                                              (get "CATEGORY")
                                              (or filewide-category))
                      :file               file-str
                      :line-number        line-number
                      :priority-cookie    priority-cookie
                      :clock-logs         clock-logs
                      :clocked-in?        (->> clock-logs
                                               (some #(nil? (second %)))
                                               (boolean))
                      :agenda-notes       agenda-notes
                      :parent-is-ordered? (-> props-with-ancestors second (get "ORDERED") (= "t"))
                      :backlog-section     (or (get properties "BACKLOG_SECTION")
                                               ;; legacy name
                                               (get properties "AGENDA_SECTION")
                                               (if (contains? tags "backlog")
                                                 "legacy_backlog_tag"))
                      :properties         properties
                      :tags               tags
                      :own-tags           (set (first tags-with-ancestors))
                      :effort             effort
                      :repeat?            (or scheduled-repeater? deadline-repeater?)}
                     {:raw-section section})
              date-range (->> prelude
                              (keep (fn [line]
                                      (re-find agenda-date-range-pattern line)))
                              (first))]
          (when (< 1 (count (filter identity [date-range scheduled-repeater? deadline-repeater?])))
            (throw (ex-info "Can't have more than one of [date-range scheduled-repeater? deadline-repeater?]"
                            {})))
          (cond date-range
                (let [[_ d1 d2] date-range
                      d1 (LocalDate/parse d1)
                      d2 (LocalDate/parse d2)
                      all-dates (->> (iterate #(.plusDays % 1) d1)
                                     (take-while #(compare/<= % d2)))]
                  (->> all-dates
                       (map-indexed (fn [idx date]
                                      (-> base
                                          (assoc :agenda-timestamp date)
                                          (update :header #(format "DAY %d/%d: %s"
                                                                   (inc idx)
                                                                   (count all-dates)
                                                                   %)))))))

                scheduled-repeater?
                (->> (apply-repeater (:repeater scheduled) (:base scheduled) today max-repeater-date)
                     (map-indexed (fn [idx scheduled]
                                    (-> base
                                        (assoc :scheduled scheduled)
                                        (cond-> (pos? idx)
                                          (assoc :last-repeat nil))))))

                deadline-repeater?
                (->> (apply-repeater (:repeater deadline) (:base deadline) today max-repeater-date)
                     (map-indexed (fn [idx new-deadline]
                                    (-> base
                                        (assoc :deadline (assoc deadline :base new-deadline))
                                        (cond-> (pos? idx)
                                          (assoc :last-repeat nil))))))

                :else [base])))
      (catch Exception e
        (let [f (format "/tmp/agenda-err-%02d.org" (mod (hash section) 100))
              header (format "AGENDA ERROR[%s]: %s" f (.getMessage e))]
          (with-open [w (io/writer f)
                      pw (java.io.PrintWriter. w)]
            (.write w (format "* TODO [#A] %s\n" header))
            (.write w (format "  FILE: %s\n" file-str))
            (.write w (format "  LINE: %d\n" line-number))
            (binding [*out* pw] (print-stack-trace e)))
          [{:todo "TODO"
            :raw-header (str "* TODO [#A] " header)
            :header header
            :ancestor-headers []
            :priority-cookie "[#A]"
            :file f
            :line-number 1}])))))

(defn priority
  "Returns a long, where a higher number corresponds to higher priority."
  [item]
  (case (:priority-cookie item)
    nil 5
    "[#A]" 10
    "[#B]" 4
    "[#C]" 1))

(defn scheduled-date
  [item]
  (try
    (some-> (let [s (:scheduled item)]
              (cond-> s (vector? s) first))
            to-local-date)
    (catch Exception e
      (throw (ex-info "Error getting relevant date" {:item item} e)))))

(defn timetable-slot
  "Returns nil or [t1 t2] where both are LocalDateTimes. For items with
  no indicated end time, a duration of one minute is applied."
  [item]
  (if-let [agts (:agenda-timestamp item)]
    (cond (vector? agts)
          agts

          (instance? LocalDate agts)
          [(LocalDateTime/of agts (LocalTime/of 0 1))
           (LocalDateTime/of agts (LocalTime/of 23 59))]

          (instance? LocalDateTime agts)
          [agts (.plusHours agts 1)]

          :else
          (throw (ex-info "Unexpected agenda-timestamp" {:agts agts})))
    (if-let [s (:scheduled item)]
      (cond (instance? LocalDate s)
            nil

            (instance? LocalDateTime s)
            [s (.plusMinutes s 1)]

            (vector? s)
            s

            :else
            (throw (ex-info "Can't timetable-slot" {:item item}))))))

(defn all-day?
  [item]
  (let [[t1 t2] (timetable-slot item)]
    (and (-> t1 .toLocalTime str (= "00:01"))
         (-> t2 .toLocalTime str (= "23:59")))))

(defn calendar-event?
  [item]
  (and (:agenda-timestamp item)
       (not (or (:todo item) (:done item)))))

(defn all-headers [item] (conj (:ancestor-headers item) (:raw-header item)))

(defn agenda-data-for-file
  [file today]
  (try
    (let [file-str (str file)
          all-items (->> (all-sections file)
                         (mapcat #(parse-section-for-agenda % file-str today)))
          calendar-events (->> all-items
                               (filter calendar-event?))
          ;; maybe up here we can precalculate each sibling that is
          ;; shadowed, and then do a lookup?
          shadowed-siblings (->> all-items
                                 (filter :todo)
                                 (filter :parent-is-ordered?)
                                 (group-by :ancestor-headers)
                                 (mapcat (fn [[_ items]]
                                           (->> items
                                                (sort-by :line-number)
                                                (rest))))
                                 (map all-headers))

          todos (->> all-items
                     (filter :todo)
                     (map (fn [todo]
                            (let [search (conj (:ancestor-headers todo) (:raw-header todo))]
                              (assoc todo
                                     :descendent-TODOs?
                                     (->> all-items
                                          (filter :todo)
                                          (some (fn [todo]
                                                  (= search (take (count search)
                                                                  (:ancestor-headers todo)))))
                                          (boolean))
                                     :shadowed-by-sibling?
                                     ;; figure out if its parent has "ORDERED" set and
                                     ;; a prior sibling has "TODO"

                                     ;; actually we need to figure out if this applies
                                     ;; to any of its ancestors also...
                                     (->> shadowed-siblings
                                          (some (fn [headers]
                                                  (= headers (take (count headers)
                                                                   search))))
                                          (boolean)))))))]
      {:todos   todos
       :todones (->> all-items
                     (filter :done))
       :calendar-events calendar-events
       :clocked-in (->> all-items
                        (filter :clocked-in?))
       :by-category (->> all-items
                         (filter :category)
                         (group-by :category))
       :by-own-tag (->> all-items
                        (mapcat (fn [{:keys [own-tags] :as item}]
                                  (for [tag own-tags]
                                    {tag #{item}})))
                        (apply merge-with into))})
    (catch FileNotFoundException e
      (log/warn "FileNotFoundException")
      {:todos [] :todones [] :calendar-events []})))

(defn org-blocked?
  [item]
  (or (:descendent-TODOs? item)
      (:shadowed-by-sibling? item)))

(defn synthesize-agenda
  [deets-by-file today]
  (let [today+10 (.plusDays today 10)
        today-7 (.plusDays today -7)
        m (->> deets-by-file
               vals
               (mapcat :todos)
               (remove #(some->> %
                                 :priority-cookie
                                 (re-find #"\d")))
               (keep (fn [{:keys [todo header deadline backlog-section] :as item}]
                       (cond-> (when-not (org-blocked? item)
                                 (if backlog-section
                                   {:backlog #{item}}
                                   (if-let [d (scheduled-date item)]
                                     (if-not (compare/< today+10 d)
                                       {(compare/max today d) #{item}})
                                     (if (:backlog-section item)
                                       {today #{item}}
                                       {:triage #{item}}))))
                         deadline
                         (assoc :deadlines #{item}))))
               (apply merge-with into))
        calendar-events (->> deets-by-file
                             vals
                             (mapcat :calendar-events)
                             (map (juxt #(-> % timetable-slot first .toLocalDate) identity))
                             (filter (fn [[d _]] (compare/<= today-7 d today+10)))
                             (group-by first)
                             (map (fn [[k v]] [k (map second v)]))
                             (into {}))
        past-log (merge-with concat
                             (->> deets-by-file
                                  vals
                                  (mapcat :todones)
                                  (filter :closed-at)
                                  (remove #(compare/< (.toLocalDate (:closed-at %))
                                                      today-7))
                                  (group-by #(.toLocalDate (:closed-at %))))
                             (->> deets-by-file
                                  vals
                                  (mapcat :todos)
                                  (filter :last-repeat)
                                  (remove #(compare/< (.toLocalDate (:last-repeat %))
                                                      today-7))
                                  (group-by #(.toLocalDate (:last-repeat %)))))]
    ;; maybe we stop making deadlines a special section and just add
    ;; them in each day?
    {:triage (:triage m)
     :deadlines (:deadlines m)
     :by-day (->> (iterate #(.plusDays % 1) today-7)
                  (take-while #(compare/<= % today+10))
                  (map (fn [day]
                         [day {:todos (get m day [])
                               :calendar-events (get calendar-events day [])
                               :past-log (get past-log day [])}]))
                  (into {}))
     :backlog (:backlog m)}))

(let [p (re-pattern (format (str #"(?:%s )?(.*)")
                            (str TODO-state-pattern)))]
  (defn make-org-link
    [item link-text]
    (let [{:keys [properties file header]} item
          escaped-link-text (-> link-text
                                ;; don't think we can do any better
                                ;; than this, org-mode seems to not
                                ;; have a way to put square brackets
                                ;; in link text
                                (string/replace "[" "&#x5b;")
                                (string/replace "]" "&#x5d;"))]
      (when (nil? header)
        (throw (ex-info "Bad item" {:item item})))
      (if-let [id (get (:properties item) "CUSTOM_ID")]
        (format "[[file:%s::#%s][%s]]"
                file id escaped-link-text)
        (let [header-without-tags (remove-tags header)
              header-for-link (string/replace (str "*" header-without-tags)
                                              #"[\[\]\\]"
                                              (fn [c] (str "\\" c)))]
          (format "[[file:%s::%s][%s]]"
                  file header-for-link escaped-link-text))))))

(defn summarize-calendar-events
  "Returns [total-duration count-without-duration]"
  [events]
  (let [durations (for [event events
                        :when (not (all-day? event))
                        :when (not (= "t" (get (:properties event) "NO_DURATION")))
                        :let [[t1 t2] (timetable-slot event)]]
                    (if t2
                      (Duration/ofSeconds (- (.toSecondOfDay (.toLocalTime t2))
                                             (.toSecondOfDay (.toLocalTime t1))))
                      :nope))]
    [(->> durations
          (remove #{:nope})
          (reduce #(.plus %1 %2) Duration/ZERO))
     (count (filter #{:nope} durations))]))

(defmacro with-atomic-write-to
  [filename & body]
  `(let [tmp-file# (File/createTempFile "org-agenda-" ".tmp")
         out-file# ~filename]
     (try
       (with-open [w# (io/writer tmp-file#)
                   pw# (java.io.PrintWriter. w#)]
         (binding [*out* pw#]
           (println (format "Written at %s" (java.time.Instant/now)))
           (println)
           ~@body)
         (clojure.java.shell/sh "mv" (str tmp-file#) (str out-file#)))
       (finally
         (.delete tmp-file#)))))

(defmacro with-atomic-write-with-postamble-to
  [cfg & body]
  `(let [cfg# ~cfg]
     (with-atomic-write-to
       (:agenda-file cfg#)
       (let [cfg# ~cfg
             tmp-file# (File/createTempFile "org-agenda-" ".tmp")]
         ~@body
         (print "\n\n")
         (println (apply str (repeat 80 \;)))
         (println ";; Postamble\n")
         (println ";; Local Variables:")
         (println ";; eval: (gfredericks-agenda-mode 1)")
         (if-let [rf# (:refresh-file cfg#)]
           (printf ";; gfredericks-agenda-mode-refresh-file: \"%s\"\n" rf#))
         (println ";; End:")))))

(defn later-today-time
  "If the item is scheduled for today, but still in the future, then returns
  the LocalTime that it's scheduled at. Else nil."
  [item now]
  (if-let [s (:scheduled item)]
    (if (instance? LocalDateTime s)
      (if (= (.toLocalDate s) (.toLocalDate now))
        (let [lt (.toLocalTime s)]
          (if (compare/< (.toLocalTime now) lt)
            lt))))))

(defn write-to-agenda-file
  [agenda cfg]
  (let [{:keys [today-date]} agenda
        format-todo (fn [{:keys [done todo priority-cookie header] :as item}]
                      (format "%s%s %s%s%s"
                              (cond-> (or done todo)
                                (= "[#C]" priority-cookie)
                                (->> .toLowerCase (format "(%s)")))
                              (if priority-cookie
                                (str " " priority-cookie)
                                "")
                              (if-let [prefix (:custom-prefix item)]
                                (str prefix " ")
                                "")
                              (if (:repeat? item)
                                "[rep] "
                                "")
                              (if (= "t" (get (:properties item) "AGENDA_NO_LINK"))
                                header
                                (make-org-link item header))))
        print-todo-line (fn [{:keys [effort deadline properties] :as item}
                             {:keys [omit-effort?]}]
                          (println (format "-%s %s%s%s%s%s"
                                           (if omit-effort?
                                             ""
                                             (str " "
                                                  (if effort
                                                    (format-effort effort)
                                                    (if (= "t" (get properties "EFFORT_EXEMPT"))
                                                      "    "
                                                      "?:??"))))
                                           (if (= 1 (priority item))
                                             "  "
                                             "")
                                           (if deadline
                                             (let [days-left (- (-> deadline
                                                                    :base
                                                                    to-local-date
                                                                    .toEpochDay)
                                                                (.toEpochDay today-date))]
                                               (format "*%s* "
                                                       (cond (zero? days-left)
                                                             "!due today!"

                                                             (= 1 days-left)
                                                             "!due tomorrow!"

                                                             (pos? days-left)
                                                             (format "(in %d days)" days-left)

                                                             (= -1 days-left)
                                                             "!1 day overdue!"

                                                             :else
                                                             (format "!%d days overdue!" (- days-left)))))
                                             "")
                                           (if-let [lt (:later-today-time item)]
                                             (format "(not until %s) " lt)
                                             "")
                                           (let [{:keys [tag->decoration]} cfg
                                                 s (->> tag->decoration
                                                        (map (fn [[tag decoration]]
                                                               (if (contains? (:own-tags item) tag)
                                                                 decoration)))
                                                        (apply str))]
                                             (if (empty? s) s (str s " ")))
                                           (format-todo item)))
                          (when (= "t" (get properties "DEBUG"))
                            (clojure.pprint/pprint item)))
        print-calendar (fn [items]
                         (->> items
                              (map (juxt timetable-slot identity))
                              (filter first)
                              (sort-by first)
                              (partition-all 2 1)
                              (run! (fn [[[[t1 t2] item] [[t3 t4] _item2]]]
                                      (let [past? (:past? item)
                                            overlap? (and t3 (compare/< t3 t2))
                                            lt1 (.toLocalTime t1)
                                            lt2 (some-> t2 .toLocalTime)]
                                        (printf "    %s%s%s%s: %s %s\n"
                                                (if (and (not past?) overlap?)
                                                  "!!"
                                                  "  ")
                                                (if (= "t" (get (:properties item) "NO_DURATION")) "*" " ")
                                                (if past? "# " "")
                                                (if t2
                                                  (if (:all-day? item)
                                                    "(ALL DAY)"
                                                    (str lt1 "-" lt2))
                                                  lt1)
                                                (if (or (:todo item) (:done item))
                                                  (format-todo item)
                                                  (:header item))
                                                ;; once we figure out enough elisp to stop using links this
                                                ;; can just be implicit
                                                (if (:free-time? item)
                                                  ""
                                                  (format "(%s)"
                                                          (make-org-link item "link")))))))))
        {:keys [deadlines triage today future past backlog]} agenda]
    (with-atomic-write-with-postamble-to cfg
      (when-let [preamble (:preamble cfg)]
        (let [preamble-string (cond (string? preamble)
                                    preamble

                                    (fn? preamble)
                                    (preamble)

                                    :else
                                    (throw (ex-info "Bad preamble"
                                                    {:preamble preamble})))
              ;; strip out any -*- variables;
              ;; e.g. org-link-file-path-type: absolute may be useful
              preamble-string (if-let [[_ s] (re-matches #"(?s)-\*-.*-\*-\n+(.*)" preamble-string)]
                                s
                                preamble-string)]
          (println preamble-string)))

      (println (format "Things to do right now: %d" (:todo-now-count agenda)))
      (println (format "Items in the backlog: %d"
                       (count backlog)))
      (println)

      (when (seq deadlines)
        (println "== DEADLINES ==")
        (doseq [item deadlines
                :let [warning-period (-> item :deadline (get :warning-period DEFAULT-WARNING-PERIOD))
                      first-warning-day (-> item :deadline :base to-local-date (.minus warning-period))]
                :when (compare/>= today-date first-warning-day)]
          (print-todo-line item {}))
        (println))
      (when (seq triage)
        (println "== TRIAGE ==")
        (doseq [item triage]
          (print-todo-line item {}))
        (println))
      (let [print-stats
            (fn [{:keys [total-effort total-cal-time unefforted-count count-without-duration net-free-time gross-free-time] :as stats}]
              (if (.isNegative net-free-time)
                (printf "  !! time shortage of %s (out of %s)\n"
                        (format-effort net-free-time)
                        (format-effort gross-free-time))
                (printf "  Free time surplus of %s out of %s\n"
                        (format-effort net-free-time)
                        (format-effort gross-free-time)))
              (when-not (.isZero total-effort)
                (println (format "  Total effort: %s" (format-effort total-effort))))
              (when-not (.isZero total-cal-time)
                (println (format "  Total calendar time: %s" (format-effort total-cal-time))))
              (when (pos? unefforted-count)
                (println (format "# (%d items with no effort estimate)" unefforted-count)))
              (when (pos? count-without-duration)
                (println (format "# (%d calendar items with no duration)" count-without-duration))))]
        (let [{:keys [stats todos calendar-events]} today
              {past false today true} (group-by #(boolean (or (= today-date (-> % :scheduled to-local-date))
                                                              (:repeat? %)))
                                                todos)]
          (when (seq past)
            (println "== PRIOR DAYS ==")
            (run! #(print-todo-line % {}) past))
          (println "== TODAY ==")
          (when-not stats
            (throw (ex-info "Dang 1" {})))
          (println "Today's todos")
          (let [{repeating true one-off false} (group-by (comp boolean :repeat?) today)]
            (run! #(print-todo-line % {}) one-off)
            (run! #(print-todo-line % {}) repeating))
          (print-stats stats)
          (print-calendar calendar-events))
        (println "\n\n--------------------------------------------------------------------------------")
        (println "|                                  future                                      |")
        (println "--------------------------------------------------------------------------------")
        (print-table ["date" "tot" "-e" "-d" "tot-e" "tot-d"]
                     (for [[date {:keys [stats]}] future
                           :let [{:keys [unefforted-count total-effort total-cal-time count-without-duration]} stats]]
                       {"date"  (format-org-local-date date)
                        "tot"   (format-effort (.plus total-effort total-cal-time))
                        "-e"    unefforted-count
                        "-d"    count-without-duration
                        "tot-e" (format-effort total-effort)
                        "tot-d" (format-effort total-cal-time)}))
        (println)
        (doseq [[date {:keys [todos calendar-events stats]}] future]
          (println (format "== %s ==" (format-org-local-date date)))
          (when-not stats
            (throw (ex-info "Dang" {})))
          (print-stats stats)
          (print-calendar calendar-events)
          (run! #(print-todo-line % {}) todos)
          (println)))
      (println "\n\n--------------------------------------------------------------------------------")
      (println "|                                   past                                       |")
      (println "--------------------------------------------------------------------------------")
      (doseq [[date {:keys [calendar-events past-log]}] past]
        (println)
        (println (format "== LOG: %s ==" (format-org-local-date date)))
        (when (not= date (:today-date agenda))
          (print-calendar calendar-events))
        (doseq [item past-log]
          (print-todo-line item {:omit-effort? true})))
      (when (seq backlog)
        (println "\n\n--------------------------------------------------------------------------------")
        (println "|                                   backlog                                    |")
        (println "--------------------------------------------------------------------------------")
        (when-let [todo (:stalest-backlog-item agenda)]
          (println "== STALEST BACKLOG ITEM ==")
          (print-todo-line todo {})
          (println))
        (->> backlog
             (group-by :backlog-section)
             (sort)
             (run! (fn [[section items]]
                     (printf "== %s ==\n" section)
                     (run! #(print-todo-line % {}) items))))))))

(defn add-free-time
  [calendar-events date now]
  (->> calendar-events
       (map (juxt timetable-slot identity))
       (filter first)
       ((fn [events]
          (let [free-slots
                (->> events
                     (map first)
                     (reduce (fn [slots [t1 t2]]
                               (mapcat
                                (fn [[t3 t4 :as slot]]
                                  (if (or (compare/<= t2 t3)
                                          (compare/<= t4 t1))
                                    [slot]
                                    (filter identity
                                            [(when (compare/< t3 t1)
                                               [t3 t1])
                                             (when (compare/< t2 t4)
                                               [t2 t4])])))
                                slots))
                             [ ;; TODO: configurable
                              [(LocalDateTime/of date (LocalTime/of 8 0))
                               (LocalDateTime/of date (LocalTime/of 17 30))]]))]
            (concat events (for [[t1 t2 :as slot] free-slots
                                 :let [header
                                       (let [free-seconds (- (.toSecondOfDay (.toLocalTime t2))
                                                             (.toSecondOfDay (.toLocalTime t1)))
                                             free-minutes (quot free-seconds 60)
                                             [h m] ((juxt quot mod) free-minutes 60)]
                                         (format "== %02d:%02d free! ==" h m))]]
                             [slot {:header header
                                    :agenda-timestamp slot
                                    :scheduled slot
                                    :free-time? true}])))))
       (sort-by first)
       (map (fn [[[t1 t2] item]]
              (let [past? (compare/< (.atZone (or t2 t1) CHICAGO) now)
                    t1 (.toLocalTime t1)
                    t2 (some-> t2 .toLocalTime)]
                (assoc item :past? past?))))))

(defn calculate-agenda
  [deets-by-file cfg now]
  (let [today (.toLocalDate now)
        {:keys [triage deadlines by-day past-log backlog]} (synthesize-agenda deets-by-file today)
        custom-prefix (:custom-prefix cfg (constantly nil))
        stats-by-date (into {}
                            (for [[date {:keys [todos calendar-events]}] (sort by-day)
                                  :let [[total-cal-time count-without-duration] (summarize-calendar-events calendar-events)
                                        total-effort (->> todos
                                                          (keep :effort)
                                                          (reduce #(.plus %1 %2) Duration/ZERO))
                                        gross-free-time (->> (add-free-time calendar-events date now)
                                                             (filter :free-time?)
                                                             (map :scheduled)
                                                             (map (fn [[t1 t2]]
                                                                    (Duration/ofSeconds
                                                                     (- (-> t2 .toLocalTime .toSecondOfDay)
                                                                        (-> t1 .toLocalTime .toSecondOfDay)))))
                                                             (reduce #(.plus %1 %2) Duration/ZERO))
                                        net-free-time (.minus gross-free-time total-effort)]]
                              [date
                               {:unefforted-count (->> todos
                                                       (remove :effort)
                                                       ;; TODO: this is calculated in two places; either make a
                                                       ;; function or add a :needs-effort? attr to the map
                                                       (remove #(= "t" (get (:properties %) "EFFORT_EXEMPT")))
                                                       (count))
                                :total-effort total-effort
                                :gross-free-time gross-free-time
                                :net-free-time net-free-time
                                :total-cal-time total-cal-time
                                :count-without-duration count-without-duration}]))
        not-past (for [[date {:keys [todos calendar-events]}] (sort by-day)
                       :when (compare/<= today date)]
                   [date {:stats          (stats-by-date date)
                          :todos          (->> todos
                                               (map #(assoc %
                                                            :later-today-time (later-today-time % now)
                                                            :custom-prefix (custom-prefix today %)))
                                               (sort-by (juxt :later-today-time ;; put future things at the bottom
                                                              (comp - priority)
                                                              ;; move rep entries to the bottom for the future blocks, so that
                                                              ;; nonrepeating items stick out
                                                              (if (= date today) (constantly nil) :repeat?)
                                                              #(or (scheduled-date %) today)
                                                              :file
                                                              :line-number)))
                          :calendar-events (add-free-time calendar-events date now)}])
        sort-key (juxt (comp - priority) :created-at :file :line-number)
        today-stuff (second (first not-past))]
    {:deadlines      (sort-by sort-key deadlines)
     :triage         (sort-by sort-key triage)
     :backlog        (sort-by sort-key backlog)
     :today          today-stuff
     :today-date     today
     :now            now
     :stalest-backlog-item (if-let [todos (->> backlog
                                               (remove :descendent-TODOs?)
                                               (remove :shadowed-by-sibling?)
                                               (seq))]
                             (->> todos
                                  (apply min-key
                                         (fn [item]
                                           (if-let [updated-at (:updated-at item)]
                                             (-> updated-at
                                                 to-local-date
                                                 .toEpochDay)
                                             0)))))
     :future         (rest not-past)
     :past           (for [[date :as date+item] (reverse (sort by-day))
                           :when (compare/<= date today)]
                       date+item)
     :todo-now-count (->> (concat (:todos today-stuff) triage deadlines)
                          (remove #(= "[#C]" (:priority-cookie %)))
                          (count))}))

(defn all-agenda-data
  [directory now]
  (into {} (for [file (all-org-files directory)]
             [file (agenda-data-for-file file (.toLocalDate now))])))

(defn do-once
  [{:keys [directory] :as cfg} now]
  (let [by-file (all-agenda-data directory now)]
    (write-to-agenda-file (calculate-agenda by-file cfg now) cfg)))

(defn watch-abstract
  "cfg:

  :directory        the directory containing org files
  :map-fn           fn of file, today
  :synthesize-fn    fn of {filename, map-fn-output}, today
  :output-fn        fn of synthesize-fn-output
  :max-staleness    java.time.Duration, optional, default 5 minutes"
  [{:keys [directory map-fn synthesize-fn output-fn reset-all-file max-staleness]
    :or {max-staleness (Duration/ofMinutes 5)}}]
  (let [q (LinkedBlockingQueue.)
        max-staleness-ms (.toMillis max-staleness)]
    (run! #(.add q %) (all-org-files directory))
    (let [do-refresh-all (fn []
                           (log/info "Refreshing all agenda input files")
                           (run! #(.add q %) (all-org-files directory)))
          watcher (watch-dir (bound-fn [{:keys [file] :as event}]
                               (if (= reset-all-file (str file))
                                 (do-refresh-all)
                                 (when (re-matches #".*\.org" (str file))
                                   (.add q file))))
                             (io/file directory)
                             (fs/parent (io/file reset-all-file)))]
      (try
        (loop [by-file {}
               today (LocalDate/now CHICAGO)]
          (let [x (.poll q max-staleness-ms TimeUnit/MILLISECONDS)
                old-today today
                today (LocalDate/now CHICAGO)]
            (when (not= today old-today) (do-refresh-all))
            (let [start (System/currentTimeMillis)
                  now (ZonedDateTime/now CHICAGO)
                  all (->> (repeatedly #(.poll q 50 TimeUnit/MILLISECONDS))
                           (take-while identity)
                           ((fn [xs] (cond->> xs x (cons x))))
                           (distinct)
                           (doall))
                  by-file (into by-file
                                (for [file all]
                                  [file (map-fn file today)]))]
              (output-fn (synthesize-fn by-file today))
              (log/infof "Updated at %s with %d changed files in %dms"
                         (pr-str (java.util.Date.))
                         (count all)
                         (- (System/currentTimeMillis) start))
              (recur by-file today))))
        (finally
          (close-watcher watcher))))))

(defn send-notifications
  [agenda cfg]
  (when-let [notify (:notify cfg)]
    (let [{:keys [now today]} agenda]
      (->> today
           :todos
           (run! (fn [item]
                   (when-let [scheduled (:scheduled item)]
                     (let [ldt (if (vector? scheduled) (first scheduled) scheduled)
                           ldt-now (.toLocalDateTime (:now agenda))]
                       (when (and (instance? LocalDateTime ldt)
                                  (compare/<= ldt ldt-now)
                                  (compare/<= ldt-now (.plusMinutes ldt 30)))
                         (notify item))))))))))

(defn watch
  "cfg:

  :directory        the directory containing org files
  :agenda-file      the file the agenda will be written to
  :reset-all-file   a file which, when touched, causes *all* org files
                    to be reloaded
  :refresh-file     a file which, when touched, triggers a regeneration
                    of the agenda
  :callback         (optional) function of the agenda data that is called
                    each time the agenda changes"
  [{:keys [directory reset-all-file callback] :as cfg}]
  (try
    (watch-abstract
     {:directory directory
      :map-fn #'agenda-data-for-file
      :synthesize-fn (fn [by-file today]
                       (calculate-agenda by-file cfg (ZonedDateTime/now CHICAGO)))
      :reset-all-file reset-all-file
      :output-fn (fn [agenda]
                   (write-to-agenda-file agenda cfg)
                   (log/infof "Wrote to %s" (:agenda-file cfg))
                   (send-notifications agenda cfg)
                   (when callback
                     (callback agenda)))})
    (catch Exception e
      (with-atomic-write-with-postamble-to cfg
        (println e))
      (throw e))))
