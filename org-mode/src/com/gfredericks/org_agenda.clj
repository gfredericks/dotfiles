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

(defn colorize
  [s ansi-flags]
  (format "\u001b[%sm%s\u001b[0m" ansi-flags s))

;; 1 bold
;; 3 italic
;; 4 underline
(defn bold [s] (colorize s "1"))
(defn bold-underline [s] (colorize s "1;4"))
(defn bold-italic [s] (colorize s "1;3"))
(defn red [s] (colorize s "31"))
(defn blue [s] (colorize s "34"))
(defn red-bold [s] (colorize s "31;1"))
(defn blue-bold [s] (colorize s "34;1"))
(defn green-bold [s] (colorize s "32;1"))
(defn yellow-italic [s] (colorize s "33;3"))
(defn green [s] (colorize s "32;1"))

(defn header
  [s]
  (format "═════════✦ %s ✦═════════" (blue-bold s)))

(defn done-header
  [s]
  (str (green "════════ ❇ ")
       (blue-bold s)
       (green " ❇ ════════")))

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
    (yellow-italic
     (format "%d:%02d" (quot tot-minutes 60) (mod tot-minutes 60)))))

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

(defn updated-at
  [{:keys [properties scheduled created-at]} logbook-timestamps]
  (let [all (concat logbook-timestamps
                    (remove nil? [(some-> (get properties "UPDATED_AT")
                                          parse-org-date-or-datetime)
                                  scheduled
                                  created-at]))]
    (when (seq all)
      (->> all
           (map (fn [x] (cond-> x (vector? x) first)))
           (map to-local-date)
           (apply compare/max)))))

(let [header-pattern
      (re-pattern (format (str #"(\*+) (?:(%s) )?(?:(\[#[A-Z0-9]+\]) )?(.*)")
                          TODO-state-pattern))]
  (defn parse-header
    [header]
    (if-let [[_ stars todo priority-cookie rest] (re-matches header-pattern header)]
      {:stars stars
       :todo todo
       :priority-cookie priority-cookie
       :rest rest}
      (throw (ex-info "Bad header" {:header header})))))


(let [scheduled-finder         (timestamp-finder "\\s*(?: CLOSED: .*?)?(?: DEADLINE: .*?)?SCHEDULED: %s(?: DEADLINE: .*)?")
      deadline-finder          (timestamp-finder "\\s*(?: CLOSED: .*?)?(?: SCHEDULED: .*?)?DEADLINE: %s(?: SCHEDULED: .*)?")
      closed-finder            (timestamp-finder "\\s*CLOSED: %s.*")
      created-at-finder        (timestamp-finder "\\s*Created at %s")
      agenda-timestamp-finder  (timestamp-finder ".*(?<!(?:SCHEDULED|DEADLINE): )(?=<)%s(?<=>).*")

      agenda-date-range-pattern
      #"<(\d{4}-\d\d-\d\d)(?: \w\w\w)?>--<(\d{4}-\d\d-\d\d)(?: \w\w\w)?>"

]
  (defn parse-section-for-agenda*
    [{::org/keys           [header prelude sections line-number]
      ::keys [tags-with-ancestors props-with-ancestors ancestor-headers filewide-category]
      :as                  section}
     file-str
     today]
    (when (not-any? #(re-matches #"(?i)archive" %) (apply concat tags-with-ancestors))
      (let [get-timestamp (fn [finder allow-repeater? include-header?]
                            (let [ret (some->> (cond->> prelude include-header? (cons header))
                                               (keep finder)
                                               (first))]
                              (if (and (:repeater ret)
                                       (not allow-repeater?))
                                (throw (ex-info "Sorry man can't have a repeater there" {}))
                                ret)))
            logbook-timestamps (->> prelude
                                    (drop-while #(not (re-matches #" +:LOGBOOK: *" %)))
                                    (next)
                                    (take-while #(not (re-matches #" +:END: *" %)))
                                    (keep (fn [line]
                                            (if-let [[_ odt] (re-matches #" *- Note taken on (\[.*?\]) \\\\ *" line)]
                                              (parse-org-datetime odt)))))
            max-repeater-date (.plusDays today 60)
            {:keys [stars todo priority-cookie rest]} (parse-header header)
            raw-header header
            header (remove-tags rest)
            properties (first props-with-ancestors)
            effort (when-let [s (get properties "Effort")]
                     (when-let [[_ hours minutes] (re-matches #"(\d+):(\d+)" s)]
                       (Duration/ofMinutes (+ (Long/parseLong minutes)
                                              (* 60 (Long/parseLong hours))))))
            agenda-frontlog-section (->> props-with-ancestors
                                         (keep #(get % "AGENDA_FRONTLOG_SECTION"))
                                         (first))
            scheduled (get-timestamp scheduled-finder true false)
            deadline  (get-timestamp deadline-finder true false)
            tags (reduce into #{} tags-with-ancestors)
            agenda-notes (->> prelude
                              (drop-while #(not (re-matches #"\s*:AGENDA_NOTES:\s*" %)))
                              (next)
                              (take-while #(not (re-matches #"\s*:END:\s*" %)))
                              ((fn [lines]
                                 (when (seq lines)
                                   (dedent lines)))))
            followup-note (->> prelude
                               (keep #(re-matches #"\s*FOLLOWUP_NOTE:(?: (.+))?" %))
                               (map second)
                               ;; logbook items go in reverse chronological order,
                               ;; so if that's where we're putting them then first
                               ;; is the correct choice
                               (first))
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
                    :agenda-header      (if (contains? properties "DISPLAY_PARENT_HEADER")
                                          (if-let [ah (last ancestor-headers)]
                                            (format "%s ~ %s"
                                                    header
                                                    (-> ah parse-header :rest remove-tags))
                                            header)
                                          header)
                    :ancestor-headers   ancestor-headers
                    :scheduled          (:base scheduled)
                    :created-at         created-at
                    :deadline           deadline
                    :agenda-timestamp   (:base (get-timestamp agenda-timestamp-finder false true))
                    :closed-at          (:base (get-timestamp closed-finder false false))

                    :last-repeat        (some-> props-with-ancestors
                                                first
                                                (get "LAST_REPEAT")
                                                parse-org-datetime)
                    ;; properties in general aren't inherited, but the
                    ;; org-mode docs explicitly say: "If you would
                    ;; like to have a special category for a single
                    ;; entry or a (sub)tree, give the entry a
                    ;; ‘CATEGORY’ property with the special category
                    ;; you want to apply as the value." I haven't
                    ;; verified that this is how it actually behaves,
                    ;; but this namespace doesn't actually use the
                    ;; category in any critical way
                    :category           (as-> props-with-ancestors <>
                                          (some #(get % "CATEGORY") <>)
                                          (or <> filewide-category))
                    :file               file-str
                    :line-number        line-number
                    :priority-cookie    priority-cookie
                    :blocked-by         (some-> (get properties "BLOCKED_BY")
                                                (string/split #","))
                    :clock-logs         clock-logs
                    :clocked-in?        (->> clock-logs
                                             (some #(nil? (second %)))
                                             (boolean))
                    :agenda-frontlog-section agenda-frontlog-section
                    :agenda-notes       agenda-notes
                    :followup-note      followup-note
                    :parent-is-ordered? (-> props-with-ancestors second (get "ORDERED") (= "t"))
                    :backlog-priority   (if-not (or scheduled deadline)
                                          (if-let [bp (get properties "BACKLOG_PRIORITY")]
                                            (Long/parseLong bp)
                                            (if (or (get properties "BACKLOG_SECTION")
                                                    (get properties "AGENDA_SECTION")
                                                    (contains? tags "backlog"))
                                              100)))
                    :properties         properties
                    :tags               tags
                    :own-tags           (set (first tags-with-ancestors))
                    :own-properties     (first props-with-ancestors)
                    :effort             effort
                    :repeat?            (or scheduled-repeater? deadline-repeater?)}
                   {:raw-section section})
            base (assoc base :updated-at (updated-at base logbook-timestamps))
            date-range (->> prelude
                            (keep (fn [line]
                                    (re-find agenda-date-range-pattern line)))
                            (first))]
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

              :else [base])))))

(defn parse-section-for-agenda
  [section file-str today]
  (try
    (parse-section-for-agenda* section file-str today)
    (catch Exception e
      (let [f (format "/tmp/agenda-err-%02d.org" (mod (hash section) 100))
            header (format "AGENDA ERROR[%s]: %s" f (.getMessage e))]
        (with-open [w (io/writer f)
                    pw (java.io.PrintWriter. w)]
          (.write w (format "* TODO [#A] %s\n" header))
          (.write w (format "  FILE: %s\n" file-str))
          (.write w (format "  LINE: %d\n" (:line-number section)))
          (binding [*out* pw] (print-stack-trace e)))
        [{:todo "TODO"
          :raw-header (str "* TODO [#A] " header)
          :header header
          :ancestor-headers []
          :priority-cookie "[#A]"
          :file f
          :line-number 1}]))))

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

(defn last-scheduled-date
  [item]
  (if-let [ls (get-in item [:properties "LAST_SCHEDULED"])]
    (if-let [m (re-find #"\d{4}-\d{2}-\d{2}" ls)]
      (LocalDate/parse m))))

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
  (some? (:agenda-timestamp item)))

(defn all-headers [item] (conj (:ancestor-headers item) (:raw-header item)))

(defn agenda-data-for-file
  [file today]
  (try
    (let [file-str (str file)
          all-items (->> (all-sections file)
                         (mapcat #(parse-section-for-agenda % file-str today)))
          ambiguous-line-numbers (->> all-items
                                      (map (juxt :header :line-number))
                                      (distinct)
                                      (group-by first)
                                      (vals)
                                      (remove #(= 1 (count %)))
                                      (apply concat)
                                      (map second)
                                      (set))
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
                              (-> todo
                                  (assoc :descendent-TODOs?
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
                                              (boolean)))
                                  (cond-> (and (ambiguous-line-numbers (:line-number todo))
                                               (nil? (get-in todo [:properties "CUSTOM_ID"])))
                                    (assoc :notification "ambiguous link!")))))))]
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
        blocking-ids (->> deets-by-file
                          vals
                          (mapcat :todos)
                          (keep (fn [todo]
                                  (get-in todo [:properties "CUSTOM_ID"])))
                          (set))
        base-todos (->> deets-by-file
                        vals
                        (mapcat :todos)
                        (remove #(some->> %
                                          :priority-cookie
                                          (re-find #"\d"))))
        active-blocking (->> base-todos
                             (mapcat :blocked-by)
                             (filter blocking-ids)
                             (frequencies))
        m (->> base-todos
               (remove #(let [{:keys [blocked-by]} %]
                          (some (fn [id]
                                  (contains? blocking-ids id))
                                blocked-by)))
               (map (fn [todo]
                      (let [items-blocked (active-blocking (get-in todo [:properties "CUSTOM_ID"]))]
                        (cond-> todo
                          items-blocked
                          (assoc :items-blocked items-blocked)))))
               (keep (fn [{:keys [todo header deadline backlog-priority] :as item}]
                       (cond-> (when-not (org-blocked? item)
                                 (if backlog-priority
                                   {:backlog #{item}}
                                   {:todos #{item}}))
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
                                  (map (fn [todo]
                                         (assoc todo
                                                :todo nil
                                                :done "DONE"
                                                :scheduled (or (last-scheduled-date todo)
                                                               (:last-repeat todo)))))
                                  (group-by #(.toLocalDate (:last-repeat %)))))]
    ;; maybe we stop making deadlines a special section and just add
    ;; them in each day?
    {:deadlines (:deadlines m)
     :todos (:todos m)
     :backlog (:backlog m)
     :calendar-events (get calendar-events today [])
     :past-log (get past-log today [])}))

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
           ~@body)
         (clojure.java.shell/sh "mv" (str tmp-file#) (str out-file#)))
       (finally
         (.delete tmp-file#)))))

(defmacro with-atomic-write-with-postamble-to
  [cfg & body]
  `(let [cfg# ~cfg]
     (with-atomic-write-to
       (:agenda-file cfg#)
       ~@body
       (print "\n\n")
       (println (apply str (repeat 80 \;)))
       (println ";; Postamble\n")
       (println (format ";; Written at %s\n" (java.time.Instant/now)))
       (println ";; Local Variables:")
       (println ";; eval: (gfredericks-agenda-mode 1)")
       (if-let [rf# (:refresh-file cfg#)]
         (printf ";; gfredericks-agenda-mode-refresh-file: \"%s\"\n" rf#))
       (println ";; End:"))))

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
  {:pre [(map? agenda)]}
  (let [{:keys [today-date deadlines past backlog futurelog]} agenda

        format-todo (fn [{:keys [done todo priority-cookie agenda-header items-blocked] :as item}]
                      (format "%s %s%s%s%s%s"
                              (->> [(if (not= "t" (get (:properties item) "DO_NOT_RENDER_AS_TODO"))
                                      (cond-> (or done (red-bold todo))
                                        (= "[#C]" priority-cookie)
                                        (->> .toLowerCase (format "(%s)"))))
                                    priority-cookie]
                                   (remove nil?)
                                   (string/join " "))
                              (case items-blocked
                                nil ""
                                1 "[blocks 1 item] "
                                (format "[blocks %d items] " items-blocked))
                              (if-let [prefix (:custom-prefix item)]
                                (str prefix " ")
                                "")
                              (if-let [notification (:notification item)]
                                (str notification " ")
                                "")
                              (if (:repeat? item)
                                (blue "® ")
                                "  ")
                              (if (= "t" (get (:properties item) "AGENDA_NO_LINK"))
                                agenda-header
                                (make-org-link item agenda-header))))
        print-todo-line (fn [{:keys [effort deadline properties backlog-priority] :as item}
                             {:keys [omit-effort? show-scheduled-date?]}]
                          (println (format "-%s%s %s%s%s%s"
                                           (if omit-effort?
                                             ""
                                             (str " "
                                                  (if effort
                                                    (format-effort effort)
                                                    (if (= "t" (get properties "EFFORT_EXEMPT"))
                                                      "    "
                                                      (yellow-italic "?:??")))))
                                           (if show-scheduled-date?
                                             (format " %s" (scheduled-date item))
                                             "")
                                           (if backlog-priority
                                             (format "P%04d " backlog-priority)
                                             "")
                                           (if-let [lt (:later-today-time item)]
                                             (format "(not until %s) " lt)
                                             (if (:scheduled-time-for-today? item)
                                               "# "
                                               ""))
                                           (str
                                            (if-let [bumps (some-> (get properties "BUMPS")
                                                                   (Long/parseLong))]
                                              (bold (format "(%d) " bumps))
                                              "")
                                            (let [{:keys [tag->decoration]} cfg
                                                  s (->> tag->decoration
                                                         (map (fn [[tag decoration]]
                                                                (if (contains? (:own-tags item) tag)
                                                                  decoration)))
                                                         (apply str))]
                                              (if (empty? s) s (str s " "))))
                                           (format-todo item)))
                          (when deadline
                            (let [days-left (- (-> deadline
                                                   :base
                                                   to-local-date
                                                   .toEpochDay)
                                               (.toEpochDay today-date))]
                              (printf "       %s   %s\n"
                                      (red ">>")
                                      (bold-underline
                                       (cond (zero? days-left)
                                             "due today!"

                                             (= 1 days-left)
                                             "due tomorrow!"

                                             (pos? days-left)
                                             (format "(due in %d days)" days-left)

                                             (= -1 days-left)
                                             "1 day overdue!"

                                             :else
                                             (format "%d days overdue!" (- days-left)))))))
                          (when-let [fn (:followup-note item)]
                            (printf "       %s   %s\n"
                                    (green ">>")
                                    fn))
                          (when (= "t" (get properties "DEBUG"))
                            (clojure.pprint/pprint item)))
        print-todos-with-header (fn [todos header-text show-scheduled-date?]
                                  (if (seq todos)
                                    (do
                                      (println (header header-text))
                                      (when (every? :effort todos)
                                        (let [^Duration total (->> todos
                                                                   (map :effort)
                                                                   (reduce #(.plus ^Duration %1 ^Duration %2)))]
                                          (printf "(Total effort: %d:%02d)\n"
                                                  (.toHours total)
                                                  (mod (.toMinutes total) 60))))
                                      (run! #(print-todo-line %
                                                              {:show-scheduled-date?
                                                               show-scheduled-date?})
                                            todos))
                                    (println (done-header header-text)))
                                  (println))
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
                                        (printf "%s%s%s%s: %s %s\n"
                                                (if (:nearest-start-time item)
                                                  " -->"
                                                  "    ")
                                                (if (and (not past?) overlap?)
                                                  "!!"
                                                  "  ")
                                                (if (= "t" (get (:properties item) "NO_DURATION")) "*" " ")
                                                (cond->
                                                    (if t2
                                                      (if (:all-day? item)
                                                        "(ALL DAY)"
                                                        (str lt1 "-" lt2))
                                                      lt1)
                                                    past?
                                                    yellow-italic)
                                                (let [header (cond-> (:header item)
                                                               past?
                                                               yellow-italic)]
                                                  (if-let [x (or (:todo item) (:done item))]
                                                    (str x " " header)
                                                    header))
                                                ;; once we figure out enough elisp to stop using links this
                                                ;; can just be implicit
                                                (if (:free-time? item)
                                                  ""
                                                  (format "(%s)"
                                                          (make-org-link item "link")))))))))]
    (with-atomic-write-with-postamble-to cfg
      (when-let [preamble (:preamble cfg)]
        (let [preamble-string (cond (string? preamble)
                                    preamble

                                    (ifn? preamble)
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

      (println (format "Items in the backlog: %d"
                       (count backlog)))
      (println (format "Items in the frontlog: %d"
                       (->> [:frontlog-free
                             :frontlog-today
                             :frontlog-later]
                            (mapcat agenda)
                            (remove :done)
                            (count))))
      (println)

      (let [deadlines (for [item deadlines
                            :let [warning-period (-> item :deadline (get :warning-period DEFAULT-WARNING-PERIOD))
                                  first-warning-day (-> item :deadline :base to-local-date (.minus warning-period))]
                            :when (compare/>= today-date first-warning-day)]
                        item)]
        (when (seq deadlines)
          (println (header "DEADLINES"))
          (doseq [item (sort-by (comp :base :deadline) deadlines)]
            (print-todo-line item {}))
          (println)))

      (let [{:keys [triage]} agenda
            grouped (group-by :agenda-frontlog-section triage)]
        (doseq [[title todos] (concat
                               (for [[section todos] grouped
                                     :when (not (nil? section))]
                                 [section todos])
                               [["FREE" (:frontlog-free agenda)]
                                ["TODAY" (:frontlog-today agenda)]
                                ["LATER" (:frontlog-later agenda)]
                                ["TRIAGE" (get grouped nil)]])
                :let [todos (filter :todo todos)]]
          (print-todos-with-header todos title false)))


      (println (header "CALENDAR"))
      (-> agenda
          :calendar-events
          (print-calendar))

      (when (seq backlog)
        (println "\n\n┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓")
        (println "┃                                   backlog                                     ┃")
        (println "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛")
        (when-let [todo (:stalest-backlog-item agenda)]
          (println (header
                    (format "STALEST BACKLOG ITEM (%d days)"
                            (:staleness-days todo))))
          (print-todo-line todo {})
          (println))
        (println (header "RANDOM BACKLOG ITEM"))
        (print-todo-line (rand-nth backlog) {})
        (let [sort-key (juxt (comp - :backlog-priority)
                             :file
                             :line-number)]
          (println)
          (println (header "ESTIMATED"))
          (->> backlog
               (filter :effort)
               (sort-by sort-key)
               (run! #(print-todo-line % {})))
          (println)
          (println (header "UNESTIMATED"))
          (->> backlog
               (remove :effort)
               (sort-by sort-key)
               (run! #(print-todo-line % {:omit-effort? true})))))

      (println "\n\n┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓")
      (println "┃                                   future                                      ┃")
      (println "┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛")
      (let [[estimated unestimated] ((juxt filter remove)
                                     :effort
                                     futurelog)]
        (print-todos-with-header estimated "ENFORCED" true)
        (print-todos-with-header unestimated "FREE" true)))))

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
                                         (blue (format "-- %02d:%02d free!" h m)))]]
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

(defn flag-nearest-start-time
  [calendar-events now]
  (if (empty? calendar-events)
    calendar-events
    (let [now-seconds (.toEpochSecond now)
          idx-of-nearest-start-time (->> calendar-events
                                         (map-indexed vector)
                                         (apply min-key (fn [[idx event]]
                                                          (let [seconds (-> event
                                                                            timetable-slot
                                                                            first
                                                                            ;; there's some
                                                                            ;; cleaner way
                                                                            ;; to accomplish
                                                                            ;; this but I'm
                                                                            ;; not sure what
                                                                            ;; it is
                                                                            (.atZone CHICAGO)
                                                                            .toEpochSecond)]
                                                            (Math/abs (- seconds now-seconds)))))
                                         first)]
      (-> calendar-events
          (cond-> (not (vector? calendar-events)) (vec))
          (update idx-of-nearest-start-time assoc :nearest-start-time true)))))

(defn find-in-seq
  "Returns [x xs] where x is the first element that matches
  pred (or nil if none do), and xs is every element except x,
  in the same order."
  [pred xs]
  (loop [xs xs
         ys []]
    (if (empty? xs)
      [nil ys]
      (let [[x & xs] xs]
        (if (pred x)
          [x (concat ys xs)]
          (recur xs (conj ys x)))))))

(defn split-frontlog-by-effort
  "todos is a sequence of todo items in priority order
  effort-proportion is a number between 0 and 1 (inclusive);

  Returns a pair of lists of [today later]."
  [todos effort-proportion]
  ;; intermediate durations measured in millis
  (let [total (->> todos
                   (map :effort)
                   (map (fn [^Duration effort] (.toMillis effort)))
                   (reduce +))
        target (Math/ceil (* total effort-proportion))]
    (loop [today []
           remaining-budget target
           later todos]
      (let [[selected later] (find-in-seq (fn [todo]
                                            (<= (.toMillis ^Duration (:effort todo))
                                                remaining-budget))
                                          later)]
        (if selected
          (recur (conj today selected)
                 (- remaining-budget (.toMillis ^Duration (:effort selected)))
                 later)
          [today later])))))

(defn calculate-agenda
  [deets-by-file cfg now]
  (let [today (.toLocalDate now)
        {:keys [deadlines todos past-log backlog calendar-events]} (synthesize-agenda deets-by-file today)
        custom-prefix (:custom-prefix cfg (constantly nil))
        scheduled-in-the-future? #(if-let [d (scheduled-date %)]
                                    (compare/< today d))
        futurelog (->> todos
                       (filter scheduled-in-the-future?)
                       (sort-by (juxt scheduled-date
                                      :file
                                      :line-number)))
        frontlog (->> todos
                      (remove scheduled-in-the-future?)
                      (concat past-log)
                      (sort-by (juxt (comp not some? :deadline)
                                     scheduled-date
                                     :file
                                     :line-number)))
        [frontlog frontlog-free] ((juxt filter remove) :effort frontlog)
        [frontlog-free triage] ((juxt filter remove) :scheduled frontlog-free)

        [frontlog-today frontlog-later]
        (split-frontlog-by-effort frontlog
                                  ((:frontlog-effort-proportion cfg) today))

        sort-key (juxt (comp - priority) :created-at :file :line-number)]
    {:deadlines      (sort-by sort-key deadlines)
     :backlog        (remove :generated-repeat? (sort-by sort-key backlog))
     :frontlog-free  frontlog-free
     :frontlog-today frontlog-today
     :frontlog-later frontlog-later
     :futurelog      futurelog
     :triage         triage
     :today-date     today
     :now            now
     :calendar-events (add-free-time calendar-events today now)
     :stalest-backlog-item (if-let [todos (->> backlog
                                               (remove :descendent-TODOs?)
                                               (remove :shadowed-by-sibling?)
                                               (seq))]
                             (let [today-epoch-day (.toEpochDay today)
                                   staleness-days (fn [item]
                                                    (if-let [updated-at (:updated-at item)]
                                                      (- today-epoch-day
                                                         (-> updated-at
                                                             to-local-date
                                                             .toEpochDay))
                                                      today-epoch-day))]
                               (->> todos
                                    (apply max-key staleness-days)
                                    ((fn [todo] (assoc todo :staleness-days (staleness-days todo)))))))}))

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
        (catch Exception e
          (log/error e "Exception in watch-abstract")
          (throw e))
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

  :directory          the directory containing org files
  :agenda-file        the file the agenda will be written to
  :reset-all-file     a file which, when touched, causes *all* org files
                      to be reloaded
  :refresh-file       a file which, when touched, triggers a regeneration
                      of the agenda
  :callback           (optional) function of the agenda data that is called
                      each time the agenda changes
  :notify             (optional) function of a map describing an item scheduled
                      at a particular time, called regularly during the first
                      thirty minutes following the start time"
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
