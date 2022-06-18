(ns com.gfredericks.org-agenda
  (:require
   [clojure.java.io :as io]
   [clojure.pprint :refer [print-table]]
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
  #"[<\[]([-\d]{10})(?: \w{3})?(?: (\d\d:\d\d)(?:-(\d\d:\d\d))?)?( [^>\]]*?)?[>\]]")

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

(defn remove-tags
  [header]
  (second (re-matches header-with-tags-regex header)))

(let [scheduled-pattern
      (re-pattern (str "\\s*SCHEDULED: " org-timestamp-regex))
      deadline-pattern
      (re-pattern (str "\\s*DEADLINE: " org-timestamp-regex))
      closed-pattern
      (re-pattern (str "\\s*CLOSED: " org-timestamp-regex ".*"))
      created-at-pattern
      (re-pattern (str "\\s*Created at " org-timestamp-regex))
      agenda-timestamp-pattern
      (re-pattern (str ".*(?=<)" org-timestamp-regex "(?<=>).*"))
      agenda-date-range-pattern
      #"<(\d{4}-\d\d-\d\d)(?: \w\w\w)?>--<(\d{4}-\d\d-\d\d)(?: \w\w\w)?>"
      header-pattern
      (re-pattern (format (str #"(\*+) (?:(%s) )?(?:(\[#[A-Z0-9]+\]) )?(.*)")
                          TODO-state-pattern))]
  (defn parse-section-for-agenda
    [{::org/keys           [header prelude sections line-number]
      ::keys [tags-with-ancestors props-with-ancestors ancestor-headers]
      :as                  section}]
    (when (not-any? #(re-matches #"(?i)archive" %) (apply concat tags-with-ancestors))
      (let [get-timestamp (fn [p]
                            (some->> prelude
                                     (keep (fn [line]
                                             (let [[_ date time time-end]
                                                   (re-matches p line)]
                                               (if time
                                                 (let [t1 (LocalDateTime/parse (str date "T" time))]
                                                   (if time-end
                                                     (let [t2 (LocalDateTime/parse (str date "T" time-end))]
                                                       [t1 t2])
                                                     t1))
                                                 (if date
                                                   (LocalDate/parse date))))))
                                     (first)))
            [_ stars todo priority-cookie rest] (re-matches header-pattern header)
            raw-header header
            header (remove-tags rest)
            effort (when-let [s (-> props-with-ancestors first (get "Effort"))]
                     (when-let [[_ hours minutes] (re-matches #"(\d+):(\d+)" s)]
                       (Duration/ofMinutes (+ (Long/parseLong minutes)
                                              (* 60 (Long/parseLong hours))))))
            base (with-meta
                   {:todo               (if (contains? DONE-states todo) nil todo)
                    :done               (if (contains? DONE-states todo) todo)
                    :raw-header         raw-header
                    :header             header
                    :ancestor-headers   ancestor-headers
                    :scheduled          (get-timestamp scheduled-pattern)
                    :created-at         (get-timestamp created-at-pattern)
                    :deadline           (get-timestamp deadline-pattern)
                    :agenda-timestamp   (get-timestamp agenda-timestamp-pattern)
                    :closed-at          (get-timestamp closed-pattern)

                    :last-repeat        (some-> props-with-ancestors
                                                first
                                                (get "LAST_REPEAT")
                                                parse-org-datetime)
                    :line-number        line-number
                    :priority-cookie    priority-cookie
                    :parent-is-ordered? (-> props-with-ancestors second (get "ORDERED") (= "t"))
                    :properties         (first props-with-ancestors)
                    :tags               (reduce into #{} tags-with-ancestors)
                    :effort             effort}
                   {:raw-section section})]
        (if-let [range (->> prelude
                            (keep (fn [line]
                                    (re-find agenda-date-range-pattern line)))
                            (first))]
          (let [[_ d1 d2] range
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
          [base])))))

(defn to-local-date
  [x]
  (cond-> x (not (instance? LocalDate x)) .toLocalDate))

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
  [file]
  (try
    (let [file-str (str file)
          all-items (->> (all-sections file)
                         (mapcat parse-section-for-agenda)
                         (map #(assoc % :file file-str)))
          today (LocalDate/now)
          calendar-events (->> all-items
                               (filter calendar-event?))]
      {:todos (->> all-items
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
                                                              (:line-number todo)))))))))))))
       :todones (->> all-items
                     (filter :done))
       :calendar-events calendar-events})
    (catch FileNotFoundException e
      (log/warn "FileNotFoundException")
      {:todos [] :todones []  :calendar-events []})))


(defn synthesize-agenda
  [deets-by-file]
  (let [today (LocalDate/now)
        today+10 (.plusDays today 10)
        today-7 (.plusDays today -7)]
    (let [m (->> deets-by-file
                 vals
                 (mapcat :todos)
                 (remove :descendent-TODOs?)
                 (remove :shadowed-by-sibling?)
                 (remove #(some->> %
                                   :priority-cookie
                                   (re-find #"\d")))
                 (remove #(contains? (:tags %) "slow"))
                 (keep (fn [{:keys [todo header deadline] :as item}]
                         (if (and deadline (compare/<= (to-local-date deadline) today))
                           {:deadlines #{item}}
                           (if-let [d (relevant-date item)]
                             (if-not (compare/< today+10 d)
                               {(compare/max today d) #{item}})
                             {:triage #{item}}))))
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
                    (into {}))})))

(let [p (re-pattern (format (str #"(?:%s )?(.*)")
                            (str TODO-state-pattern)))]
  (defn make-org-link
    [item link-text]
    (let [{:keys [properties file header]} item]
      (if-let [id (get (:properties item) "CUSTOM_ID")]
        (format "[[file:%s::#%s][%s]]"
                file id link-text)
        (let [header-without-tags (remove-tags header)
              [_ post-todo] (re-matches p header-without-tags)
              header-for-link (string/replace (str "*" post-todo)
                                              #"[\[\]\\]"
                                              (fn [c] (str "\\" c)))]
          (format "[[file:%s::%s][%s]]"
                  file header-for-link link-text))))))

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
         out-file# (:agenda-file cfg#)]
     (with-open [w# (io/writer "/tmp/w")
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
       (clojure.java.shell/sh "mv" "/tmp/w" (str out-file#)))))

(defn write-to-agenda-file
  [deets-by-file {:keys [preamble] :as cfg}]
  (let [today (LocalDate/now)
        now (ZonedDateTime/now CHICAGO)
        {:keys [triage deadlines by-day past-log]} (synthesize-agenda deets-by-file)
        format-todo (fn [{:keys [done todo priority-cookie header] :as item}]
                      (format "%s%s %s"
                              (or done todo)
                              (if priority-cookie
                                (str " " priority-cookie)
                                "")
                              (if (= "t" (get (:properties item) "AGENDA_NO_LINK"))
                                header
                                (make-org-link item header))))
        print-todo-line (fn [{:keys [effort properties] :as item}
                             {:keys [omit-effort?]}]
                          (println (format "-%s %s"
                                           (if omit-effort?
                                             ""
                                             (str " "
                                                  (if effort
                                                    (format-effort effort)
                                                    (if (= "t" (get properties "EFFORT_EXEMPT"))
                                                      "    "
                                                      "?:??"))))
                                           (format-todo item))))
        print-calendar (fn [items]
                         (->> items
                              (map (juxt timetable-slot identity))
                              (filter first)
                              (sort-by first)
                              (run! (fn [[[t1 t2] item]]
                                      (let [past? (compare/< (.atZone (or t2 t1) CHICAGO) now)
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
                                                (make-org-link item "(link)")))))))]
    (with-atomic-write-to cfg
      (when preamble
        (println preamble))
      (when (seq deadlines)
        (println "== DEADLINES ==")
        (println "(TODO: show upcoming deadlines based on the -0d cookie)")
        (doseq [item (sort-by (juxt :created-at :file :line-number) deadlines)]
          (print-todo-line item {}))
        (println))
      (when (seq triage)
        (println "== TRIAGE ==")
        (doseq [item (sort-by (juxt :created-at :file :line) triage)]
          (print-todo-line item {}))
        (println))
      (let [stats-by-date
            (into {}
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
                      :count-without-duration count-without-duration}]))]
        (doseq [[date {:keys [todos calendar-events]}] (sort by-day)
                :when (compare/<= today date)
                :let [{:keys [unefforted-count total-effort total-cal-time count-without-duration]}
                      (stats-by-date date)]]
          (println (format "== %s =="
                           (if (= date today)
                             "TODAY"
                             (format-org-local-date date))))
          (when-not (.isZero total-effort)
            (println (format "  Total effort: %s" (format-effort total-effort))))
          (when-not (.isZero total-cal-time)
            (println (format "  Total calendar time: %s" (format-effort total-cal-time))))
          (when (pos? unefforted-count)
            (println (format "# (%d items with no effort estimate)" unefforted-count)))
          (when (pos? count-without-duration)
            (println (format "# (%d calendar items with no duration)" count-without-duration)))
          (print-calendar calendar-events)
          ;; TODO: we shouldn't be putting things in the calendar if they're
          ;; scheduled in the past...
          (->> todos
               (sort-by (juxt relevant-date :file :line))
               (run! (fn [item] (print-todo-line item {}))))
          (when (= date today)
            (println "\n\n--------------------------------------------------------------------------------")
            (println "|                                  future                                      |")
            (println "--------------------------------------------------------------------------------")
            (print-table ["date" "tot" "-e" "-d" "tot-e" "tot-d"]
                         (for [date (sort (keys by-day))
                               :when (compare/< today date)
                               :let [{:keys [unefforted-count total-effort total-cal-time count-without-duration]}
                                     (stats-by-date date)]]
                           {"date"  (format-org-local-date date)
                            "tot"   (format-effort (.plus total-effort total-cal-time))
                            "-e"    unefforted-count
                            "-d"    count-without-duration
                            "tot-e" (format-effort total-effort)
                            "tot-d" (format-effort total-cal-time)})))
          (println)))
      (println "\n\n--------------------------------------------------------------------------------")
      (println "|                                   past                                       |")
      (println "--------------------------------------------------------------------------------")
      (doseq [[date {:keys [calendar-events past-log]}] (reverse (sort by-day))
              :when (compare/<= date today)]
        (println)
        (println (format "== LOG: %s ==" (format-org-local-date date)))
        (print-calendar calendar-events)
        (doseq [item past-log]
          (print-todo-line item {:omit-effort? true}))))))

(defn watch
  [{:keys [directory reset-all-file] :as cfg}]
  (let [q (LinkedBlockingQueue.)]
    (run! #(.add q %) (all-org-files directory))
    (let [watcher (watch-dir (bound-fn [{:keys [file] :as event}]
                               (when (re-matches #".*\.org" (str file))
                                 (if (= reset-all-file (str file))
                                   (run! #(.add q %) (all-org-files directory))
                                   (.add q file))))
                             (io/file directory)
                             (fs/parent (io/file reset-all-file)))]
      ;; add watch to #'write-to-agenda-file??
      ;; for interactive dev; just have to remove it at the end
      (try
        (loop [by-file {}]
          (let [x (.poll q 1 TimeUnit/MINUTES)]
            (if x
              (let [start (System/currentTimeMillis)
                    all (->> (repeatedly #(.poll q 50 TimeUnit/MILLISECONDS))
                             (take-while identity)
                             (cons x)
                             (distinct)
                             (doall))
                    by-file (into by-file
                                  (for [file all]
                                    [file (agenda-data-for-file file)]))]
                ;; TODO: catch exceptions and write them to the file
                ;; though...we can end up with stale views of things that
                ;; way; it'd be hard to handle that without getting into a
                ;; thrashing loop
                (write-to-agenda-file by-file cfg)
                (log/infof "Updated at %s with %d changed files in %dms"
                           (pr-str (java.util.Date.))
                           (count all)
                           (- (System/currentTimeMillis) start))
                (recur by-file))
              (recur by-file))))
        (catch Exception e
          (with-atomic-write-to cfg
            (println e))
          (throw e))
        (finally
          (close-watcher watcher))))))
