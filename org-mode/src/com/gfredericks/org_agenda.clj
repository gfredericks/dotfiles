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
   (java.time Duration LocalDate LocalDateTime LocalTime ZonedDateTime ZoneId)
   (java.time.format DateTimeFormatter)
   (java.util.concurrent LinkedBlockingQueue TimeUnit)))

(def CHICAGO (ZoneId/of "America/Chicago"))

(def org-timestamp-regex
  #"[<\[]([-\d]{10})(?: \w{3})?(?: (\d\d:\d\d)(?:-(\d\d:\d\d))?)?(?: (\+|\+\+|\.\+)(\d+)([ymwdh]))?( [^>\]]*?)?[>\]]")

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
  (let [[_ d t] (re-matches org-timestamp-regex s)]
    (LocalDateTime/of (LocalDate/parse d) (LocalTime/parse t))))

(defn to-local-date
  [x]
  (cond-> x (not (instance? LocalDate x)) .toLocalDate))

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
    (-> (org/parse-file r)
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
             (map #(assoc % ::org/line-number (::org/line-number (meta %))))))))

(defn timestamp-finder
  [context-regex-format-string]
  (let [pattern (re-pattern (format context-regex-format-string org-timestamp-regex))]
    (fn [line]
      (let [[_ date time time-end r-type r-num r-unit] (re-matches pattern line)
            base (if time
                   (let [t1 (LocalDateTime/parse (str date "T" time))]
                     (if time-end
                       (let [t2 (LocalDateTime/parse (str date "T" time-end))]
                         [t1 t2])
                       t1))
                   (if date
                     (LocalDate/parse date)))]
        (if r-type
          {:repeater [r-type (Long/parseLong r-num) r-unit]
           :base base}
          base)))))

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

(let [scheduled-finder         (timestamp-finder "\\s*SCHEDULED: %s")
      deadline-finder          (timestamp-finder "\\s*DEADLINE: %s")
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
      ::keys [tags-with-ancestors props-with-ancestors ancestor-headers]
      :as                  section}
     file-str
     today]
    (try
      (when (not-any? #(re-matches #"(?i)archive" %) (apply concat tags-with-ancestors))
        (let [get-timestamp (fn [finder allow-repeater?]
                              (let [ret (some->> prelude
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
              scheduled (get-timestamp scheduled-finder true)
              deadline  (get-timestamp deadline-finder true)
              tags (reduce into #{} tags-with-ancestors)
              properties (first props-with-ancestors)
              base (with-meta
                     {:todo               (if (contains? DONE-states todo) nil todo)
                      :done               (if (contains? DONE-states todo) todo)
                      :raw-header         raw-header
                      :header             header
                      :ancestor-headers   ancestor-headers
                      :scheduled          scheduled
                      :created-at         (get-timestamp created-at-finder false)
                      :deadline           deadline
                      :agenda-timestamp   (get-timestamp agenda-timestamp-finder false)
                      :closed-at          (get-timestamp closed-finder false)

                      :last-repeat        (some-> props-with-ancestors
                                                  first
                                                  (get "LAST_REPEAT")
                                                  parse-org-datetime)
                      :file               file-str
                      :line-number        line-number
                      :priority-cookie    priority-cookie
                      :parent-is-ordered? (-> props-with-ancestors second (get "ORDERED") (= "t"))
                      :agenda-section     (get properties "AGENDA_SECTION")
                      :properties         properties
                      :tags               tags
                      :effort             effort
                      :backlog?           (contains? tags "backlog")}
                     {:raw-section section})
              date-range (->> prelude
                              (keep (fn [line]
                                      (re-find agenda-date-range-pattern line)))
                              (first))
              scheduled-repeater? (:repeater scheduled)
              deadline-repeater? (:repeater deadline)]
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
                                          (assoc :repeat? true :last-repeat nil))))))

                deadline-repeater?
                (->> (apply-repeater (:repeater deadline) (:base deadline) today max-repeater-date)
                     (map-indexed (fn [idx deadline]
                                    (-> base
                                        (assoc :deadline deadline)
                                        (cond-> (pos? idx)
                                          (assoc :repeat? true :last-repeat nil))))))

                :else [base])))
      (catch Exception e
        (let [header (str "AGENDA ERROR: " (.getMessage e))
              f (format "/tmp/agenda-err-%02d.org" (mod (hash section) 100))]
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

(defn relevant-date
  [item]
  (some-> (or (let [s (:scheduled item)]
                (cond-> s (vector? s) first))
              (:deadline item))
          to-local-date))

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

(defn agenda-data-for-file
  [file today]
  (try
    (let [file-str (str file)
          all-items (->> (all-sections file)
                         (mapcat #(parse-section-for-agenda % file-str today)))
          calendar-events (->> all-items
                               (filter calendar-event?))
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
                                     (boolean
                                      (and (:parent-is-ordered? todo)
                                           (->> all-items
                                                (filter :todo)
                                                (some (fn [todo2]
                                                        (and (= (:ancestor-headers todo)
                                                                (:ancestor-headers todo2))
                                                             (< (:line-number todo2)
                                                                (:line-number todo)))))))))))))]
      {:todos   todos
       :todones (->> all-items
                     (filter :done))
       :calendar-events calendar-events})
    (catch FileNotFoundException e
      (log/warn "FileNotFoundException")
      {:todos [] :todones [] :calendar-events []})))


(defn synthesize-agenda
  [deets-by-file today]
  (let [today+10 (.plusDays today 10)
        today-7 (.plusDays today -7)
        m (->> deets-by-file
               vals
               (mapcat :todos)
               (remove :descendent-TODOs?)
               (remove :shadowed-by-sibling?)
               (remove #(some->> %
                                 :priority-cookie
                                 (re-find #"\d")))
               (keep (fn [{:keys [todo header deadline backlog?] :as item}]
                       (if backlog?
                         {:backlog #{item}}
                         (if (and deadline (compare/<= (to-local-date deadline) today))
                           {:deadlines #{item}}
                           (if-let [d (relevant-date item)]
                             (if-not (compare/< today+10 d)
                               {(compare/max today d) #{item}})
                             (if (:agenda-section item)
                               {today #{item}}
                               {:triage #{item}}))))))
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
  [cfg & body]
  `(let [cfg# ~cfg
         tmp-file# (File/createTempFile "org-agenda-" ".tmp")
         out-file# (:agenda-file cfg#)]
     (try
       (with-open [w# (io/writer tmp-file#)
                   pw# (java.io.PrintWriter. w#)]
         (binding [*out* pw#]
           (println (format "Written at %s" (java.time.Instant/now)))
           (println)
           ~@body
           (print "\n\n")
           (println (apply str (repeat 80 \;)))
           (println ";; Postamble\n")
           (println ";; Local Variables:")
           (println ";; eval: (gfredericks-agenda-mode 1)")
           (if-let [rf# (:refresh-file cfg#)]
             (printf ";; gfredericks-agenda-mode-refresh-file: \"%s\"\n" rf#))
           (println ";; End:"))
         (clojure.java.shell/sh "mv" (str tmp-file#) (str out-file#)))
       (finally
         (.delete tmp-file#)))))

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
  (let [format-todo (fn [{:keys [done todo priority-cookie header] :as item}]
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
        print-todo-line (fn [{:keys [effort properties] :as item}
                             {:keys [omit-effort?]}]
                          (println (format "-%s %s%s%s"
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
                                           (if-let [lt (:later-today-time item)]
                                             (format "(not until %s) " lt)
                                             "")
                                           (format-todo item)))
                          (when (= "t" (get properties "DEBUG"))
                            (clojure.pprint/pprint item)))
        print-calendar (fn [items]
                         (->> items
                              (map (juxt timetable-slot identity))
                              (filter first)
                              (sort-by first)
                              (run! (fn [[[t1 t2] item]]
                                      (let [past? (:past? item)
                                            t1 (.toLocalTime t1)
                                            t2 (some-> t2 .toLocalTime)]
                                        (printf "      %s%s%s: %s %s\n"
                                                (if (= "t" (get (:properties item) "NO_DURATION")) "*" " ")
                                                (if past? "# " "")
                                                (if t2
                                                  (if (all-day? item)
                                                    "(ALL DAY)"
                                                    (str t1 "-" t2))
                                                  t1)
                                                (if (or (:todo item) (:done item))
                                                  (format-todo item)
                                                  (:header item))
                                                ;; once we figure out enough elisp to stop using links this
                                                ;; can just be implicit
                                                (make-org-link item "(link)")))))))
        {:keys [deadlines triage today future past backlog]} agenda]
    (with-atomic-write-to cfg
      (when-let [preamble (:preamble cfg)]
        (println (cond (string? preamble)
                       preamble

                       (fn? preamble)
                       (preamble)

                       :else
                       (throw (ex-info "Bad preamble"
                                       {:preamble preamble})))))

      (println (format "Things to do right now: %d\n" (:todo-now-count agenda)))

      (when (seq deadlines)
        (println "== DEADLINES ==")
        (println "(TODO: show upcoming deadlines based on the -0d cookie)")
        (doseq [item deadlines]
          (print-todo-line item {}))
        (println))
      (when (seq triage)
        (println "== TRIAGE ==")
        (doseq [item triage]
          (print-todo-line item {}))
        (println))
      (let [print-stats
            (fn [{:keys [total-effort total-cal-time unefforted-count count-without-duration]}]
              (when-not (.isZero total-effort)
                (println (format "  Total effort: %s" (format-effort total-effort))))
              (when-not (.isZero total-cal-time)
                (println (format "  Total calendar time: %s" (format-effort total-cal-time))))
              (when (pos? unefforted-count)
                (println (format "# (%d items with no effort estimate)" unefforted-count)))
              (when (pos? count-without-duration)
                (println (format "# (%d calendar items with no duration)" count-without-duration))))]
        (println "== TODAY ==")
        (let [{:keys [stats todos calendar-events]} today]
          (when-not stats
            (throw (ex-info "Dang 1" {})))
          (print-stats stats)
          (print-calendar calendar-events)
          (->> todos
               (group-by :agenda-section)
               (sort)
               (run! (fn [[agenda-section todos]]
                       (when agenda-section
                         (printf "\n*%s*\n" (.toUpperCase agenda-section)))
                       (run! #(print-todo-line % {}) todos)))))
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
        (print-calendar calendar-events)
        (doseq [item past-log]
          (print-todo-line item {:omit-effort? true})))
      (when (seq backlog)
        (println "\n\n--------------------------------------------------------------------------------")
        (println "|                                   backlog                                    |")
        (println "--------------------------------------------------------------------------------")
        (doseq [item backlog]
          (print-todo-line item {}))))))

(defn calculate-agenda
  [deets-by-file cfg now]
  (let [today (.toLocalDate now)
        {:keys [triage deadlines by-day past-log backlog]} (synthesize-agenda deets-by-file today)
        custom-prefix (:custom-prefix cfg (constantly nil))
        stats-by-date (into {}
                            (for [[date {:keys [todos calendar-events]}] (sort by-day)
                                  :let [[total-cal-time count-without-duration] (summarize-calendar-events calendar-events)]]
                              [date
                               {:unefforted-count (->> todos
                                                       (remove :effort)
                                                       ;; TODO: this is calculated in two places; either make a
                                                       ;; function or add a :needs-effort? attr to the map
                                                       (remove #(= "t" (get (:properties %) "EFFORT_EXEMPT")))
                                                       (count))
                                :total-effort (->> todos
                                                   (keep :effort)
                                                   (reduce #(.plus %1 %2) Duration/ZERO))
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
                                                              relevant-date
                                                              :file
                                                              :line-number)))
                          :calendar-events (->> calendar-events
                                                (map (juxt timetable-slot identity))
                                                (filter first)
                                                (sort-by first)
                                                (map (fn [[[t1 t2] item]]
                                                       (let [past? (compare/< (.atZone (or t2 t1) CHICAGO) now)
                                                             t1 (.toLocalTime t1)
                                                             t2 (some-> t2 .toLocalTime)]
                                                         (assoc item
                                                                :timetable-slot [t1 t2]
                                                                :past? past?)))))}])
        sort-key (juxt (comp - priority) :created-at :file :line-number)
        today-stuff (second (first not-past))]
    {:deadlines      (sort-by sort-key deadlines)
     :triage         (sort-by sort-key triage)
     :backlog        (sort-by sort-key backlog)
     :today          today-stuff
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
  (let [q (LinkedBlockingQueue.)]
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
          (let [x (.poll q 1 TimeUnit/MINUTES)
                old-today today
                today (LocalDate/now CHICAGO)]
            (when (not= today old-today) (do-refresh-all))
            (if x
              (let [start (System/currentTimeMillis)
                    now (ZonedDateTime/now CHICAGO)
                    all (->> (repeatedly #(.poll q 50 TimeUnit/MILLISECONDS))
                             (take-while identity)
                             (cons x)
                             (distinct)
                             (doall))
                    by-file (into by-file
                                  (for [file all]
                                    [file (agenda-data-for-file file today)]))]
                (let [agenda (calculate-agenda by-file cfg now)]
                  (write-to-agenda-file agenda cfg)
                  (when callback
                    (callback agenda)))
                (log/infof "Updated at %s with %d changed files in %dms"
                           (pr-str (java.util.Date.))
                           (count all)
                           (- (System/currentTimeMillis) start))
                (recur by-file today))
              (recur by-file today))))
        (catch Exception e
          (with-atomic-write-to cfg
            (println e))
          (throw e))
        (finally
          (close-watcher watcher))))))
