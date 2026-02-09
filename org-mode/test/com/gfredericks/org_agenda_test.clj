(ns com.gfredericks.org-agenda-test
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]
   [clojure.test :refer [are deftest is]]
   [com.gfredericks.org-agenda :as oa])
  (:import
   (java.time Duration LocalDate LocalDateTime ZonedDateTime)
   (java.nio.file Files)))

(defn lines
  [& lines]
  (apply str (mapcat vector lines (repeat \newline))))

(defn LDT [s] (LocalDateTime/parse s))
(defn LD [s] (LocalDate/parse s))

(deftest parse-org-datetime-test
  (is (= (LDT "2022-06-11T11:14:00")
         (oa/parse-org-datetime "[2022-06-11 Sat 11:14]"))))

(deftest timestamp-finder-test
  (are [format-str line out] (= out ((oa/timestamp-finder format-str) line))
    "%s" "[2022-06-11 Sat 11:14]" {:base (LDT "2022-06-11T11:14:00") :active? false}
    "%s" "<2022-06-18>" {:base (LD "2022-06-18") :active? true}
    " heyo %s.*" " heyo <2022-01-01 Sat 08:00-10:43> anything" {:base [(LDT "2022-01-01T08:00")
                                                                       (LDT "2022-01-01T10:43")]
                                                                :active? true}
    "%s" "<2022-06-18 Sat +3d>" {:repeater ["+" 3 "d"] :base (LD "2022-06-18") :active? true}
    "%s" "<2022-06-18 Sat ++3d>" {:repeater ["++" 3 "d"] :base (LD "2022-06-18") :active? true}
    "%s" "<2022-06-18 Sat 19:00 .+1w>" {:repeater [".+" 1 "w"] :base (LDT "2022-06-18T19:00") :active? true}))

(deftest make-org-link-test
  (is (= "[[file:/fake-file.org::*TODO this has an extra TODO][TODO TODO this has an extra TODO]]"
         (oa/make-org-link
          {:header "TODO this has an extra TODO"
           :file "/fake-file.org"}
          "TODO TODO this has an extra TODO")))
  (is (= "[[file:/fake-file.org::*TODO this headline has \\[brackets\\]][TODO this headline has 〚brackets〛]]"
         (oa/make-org-link
          {:header "TODO this headline has [brackets]"
           :file "/fake-file.org"}
          "TODO this headline has [brackets]"))))

(let [empty-file-attrs (make-array java.nio.file.attribute.FileAttribute 0)]
  (defn do-integration
    [filenames->contents now agenda-validator]
    (let [dir (Files/createTempDirectory "org-agenda-tests-" empty-file-attrs)
          agenda (Files/createTempFile "org-agenda-tests-" ".org" empty-file-attrs)]
      (try
        (doseq [[filename contents] filenames->contents]
          (spit (str dir "/" filename) contents))
        (oa/do-once {:directory (str dir)
                     :agenda-file (str agenda)
                     :frontlog-effort-proportion (constantly 1/3)}
                    now)
        (-> agenda
            str
            slurp
            ;; remove ansi tags
            (string/replace #"\x1b\[.*?m" "")
            (agenda-validator))
        (finally
          (doseq [filename (keys filenames->contents)]
            (Files/delete (.resolve dir filename)))
          (Files/delete dir)
          (Files/delete agenda))))))

(deftest ordered-test
  ;; testing that ORDERED works correctly with descendents
  (do-integration
   {"only-file.org"
    (lines
     "* Hello"
     "  :PROPERTIES:"
     "  :ORDERED: t"
     "  :END:"
     "** TODO Aaa"
     "*** TODO Bbb"
     "** TODO Ccc"
     "*** TODO Ddd")}
   ;; doesn't matter
   (ZonedDateTime/of 2022 7 10 16 22 15 0 oa/CHICAGO)
   (fn [agenda]
     (is (= 1 (count (re-seq #"TODO\b" agenda))))
     (is (re-find #"Bbb" agenda)))))

(deftest calendar-reference-test
  (let [now (ZonedDateTime/of 2022 7 10 16 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      "* Something is happening on <2022-07-10 Sun>"}
     now
     (fn [agenda]
       ;; 2 because the link contains it as well
       (is (= 2 (count (re-seq #"Something is happening" agenda))))))
    (do-integration
     {"only-file.org"
      (lines
       "* Something is happening!"
       "But not until <2022-07-10 Sun>")}
     now
     (fn [agenda]
       (is (= 2 (count (re-seq #"Something is happening" agenda))))))))


(deftest regression-test-2022-12-23
  (let [now (ZonedDateTime/of 2022 12 23 16 22 15 0 oa/CHICAGO)]
    ;; fixing an issue with UPDATED_AT being just a date, not a
    ;; datetime
    (do-integration
     {"only-file.org"
      (lines
       "* TODO heyo heyo"
       "  :PROPERTIES:"
       "  :UPDATED_AT: [2022-12-20 Tue]"
       "  :END:")}
     now
     (fn [agenda]
       (is (re-find #"heyo heyo" agenda))))))

(deftest show-shadowed-deadlines
  (let [now (ZonedDateTime/of 2022 12 23 16 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO heyo heyo"
       "  DEADLINE: <2022-12-25 Sun>"
       "** TODO This is shadowing it")}
     now
     (fn [agenda]
       (is (re-find #"heyo heyo" agenda))))))

(deftest calendar-overlap-test
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* Thing one <2022-12-23 Fri 12:00-13:00>"
       "* Thing two <2022-12-23 Fri 12:30-14:00>")}
     now
     (fn [agenda]
       (is (re-find #"!!" agenda))))
    (do-integration
     {"only-file.org"
      (lines
       "* Thing one <2022-12-23 Fri 08:00-08:30>"
       "* Thing two <2022-12-23 Fri 08:20-09:03>")}
     now
     (fn [agenda]
       ;; no indicator when it's in the past
       (is (not (re-find #"!!" agenda)))))
    (do-integration
     {"only-file.org"
      (lines
       "* Thing one <2022-12-23 Fri 12:00-13:00>")}
     now
     (fn [agenda]
       (is (not (re-find #"!!" agenda)))))))


(deftest calendar-gap-test
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* Thing one <2022-12-23 Fri 12:00-13:00>"
       "* Thing two <2022-12-23 Fri 14:30-16:00>")}
     now
     (fn [agenda]
       (is (re-find #"01:30 free!" agenda))))))

(deftest deadline-daycount-test
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  DEADLINE: <2022-12-25 Sun>")}
     now
     (fn [agenda]
       (is (re-find #"TODO.+This thing.*\n.+in 2 days" agenda)))))
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  DEADLINE: <2022-12-14 Wed>")}
     now
     (fn [agenda]
       (is (re-find #".+TODO.+This thing.*\n.+9 days overdue!" agenda))))))

(deftest deadline-hiding-rules-test
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)
        deadline-section #(second (re-matches #"(?s)(.*?)══✦ FREE ✦══.*" %))]
    ;; default warning period is 14 days
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  DEADLINE: <2023-02-03 Fri>")}
     now
     (fn [agenda]
       (is (not (re-find #"days.+This thing" (deadline-section agenda))))))
    ;; can set a custom warning period
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  DEADLINE: <2022-12-30 Fri -3d>")}
     now
     (fn [agenda]
       (is (not (re-find #"7 days.+This thing" (deadline-section agenda))))))
    ;; hidden deadlines can still show up as regular TODOs
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  DEADLINE: <2023-02-03 Fri>"
       "  :PROPERTIES:"
       "  :BACKLOG_PRIORITY: 125"
       "  :END:")}
     now
     (fn [agenda]
       (is (re-find #"(?s)backlog.*TODO.+This thing" agenda))))))

(deftest deadline-with-scheduled-test
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  SCHEDULED: <2023-02-01 Wed> DEADLINE: <2023-02-03 Fri>")}
     now
     (fn [agenda]
       ;; doesn't show up at all, because the SCHEDULED date is
       ;; way in the future as well
       (is (not (re-find #"days.+This thing" agenda)))))))

(deftest todos-with-time-ranges-appear-in-calendar
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  <2022-12-23 Fri 12:00-13:30>")}
     now
     (fn [agenda]
       (is (re-find #"12:00-13:30: TODO This thing" agenda))))))

(deftest blocked-by-test
  (let [now (ZonedDateTime/of 2023 4 5 7 37 15 221 oa/CHICAGO)]
    (do-integration
     {"file-1.org"
      (lines
       "* TODO This one is blocked by the last one in this file"
       "  :PROPERTIES:"
       "  :BLOCKED_BY: 8cbae507-9b29-4b4a-9a55-1265c3f2b57f"
       "  :END:"
       "* TODO This one is blocked by the one in the other file"
       "  :PROPERTIES:"
       "  :BLOCKED_BY: 2d0ed083-7922-489f-b77c-8009320ec3b0"
       "  :CUSTOM_ID: 1d55b951-41d1-4876-bde8-603f086adb1e"
       "  :END:"
       "* TODO The first one is blocked by this one"
       "  :PROPERTIES:"
       "  :CUSTOM_ID: 8cbae507-9b29-4b4a-9a55-1265c3f2b57f"
       "  :END:"
       "* TODO This one DOES show up because its blocker is DONE"
       "  :PROPERTIES:"
       "  :BLOCKED_BY: 5ae318cc-ce9e-41cd-93bf-958d6432d29d"
       "  :END:"
       "* DONE See this one shouldn't be blocking anything anymore"
       "  :PROPERTIES:"
       "  :CUSTOM_ID: 5ae318cc-ce9e-41cd-93bf-958d6432d29d"
       "  :END:"
       "* TODO This one DOES show up because its blocker does not exist"
       "  :PROPERTIES:"
       "  :BLOCKED_BY: fb237be2-9ce7-43c7-981c-55a018b45974"
       "  :END:"
       "* TODO This one is blocked by another task that is blocked"
       "  :PROPERTIES:"
       "  :BLOCKED_BY: 1d55b951-41d1-4876-bde8-603f086adb1e"
       "  :END:"
       "* TODO This one blocks two different items"
       "  :PROPERTIES:"
       "  :CUSTOM_ID: 39c604ec-206b-4c6b-9453-074e5ba85a12"
       "  :END:"
       "* TODO This one is blocked by two items"
       "  :PROPERTIES:"
       "  :BLOCKED_BY: 39c604ec-206b-4c6b-9453-074e5ba85a12,1d55b951-41d1-4876-bde8-603f086adb1e"
       "  :END:"
       "* TODO This one is blocked by one done item and another undone item"
       "  :PROPERTIES:"
       "  :BLOCKED_BY: 5ae318cc-ce9e-41cd-93bf-958d6432d29d,39c604ec-206b-4c6b-9453-074e5ba85a12"
       "  :END:")
      "file-2.org"
      (lines
       "* TODO This blocker is in a different file"
       "  :PROPERTIES:"
       "  :CUSTOM_ID: 2d0ed083-7922-489f-b77c-8009320ec3b0"
       "  :END:")}
     now
     (fn [agenda]
       (is (re-find #"\[blocks 1 item\] .*The first one is blocked by this one" agenda))
       (is (re-find #"This blocker is in a different file" agenda))
       (is (not (re-find #"This one is blocked" agenda)))
       (is (re-find #"This one DOES show up because its blocker is DONE" agenda))
       (is (re-find #"This one DOES show up because its blocker does not exist" agenda))
       (is (re-find #"\[blocks 2 items\] .*This one blocks two different items" agenda))
       (is (not (re-find #"This one is blocked by two items" agenda)))
       (is (not (re-find #"This one is blocked by one done item and another undone item" agenda)))))))

(defn subsequence?
  [xs ys]
  (cond (empty? xs)
        true

        (empty? ys)
        false

        :else
        (let [[x & xs'] xs
              [y & ys'] ys]
          (if (= x y)
            (recur xs' ys')
            (recur xs ys')))))

(deftest split-frontlog-by-effort-test
  (let [todos (for [[id minutes] [[:a 7]
                                  [:b 3]
                                  [:c 10]
                                  [:d 1]
                                  [:e 5]]]
                {:id id, :effort (Duration/ofMinutes minutes)})]
    (are [proportion chosen-ids]
        (let [[today later] (oa/split-frontlog-by-effort todos proportion)]
          (and (= chosen-ids (map :id today))
               (= (set (map :id todos))
                  (set (map :id (concat today later))))
               (subsequence? (map :id today)
                             (map :id todos))
               (subsequence? (map :id later)
                             (map :id todos))))
      0 []
      1/5 [:b :d]
      1/4 [:b :d]
      1/3 [:a :d]
      2/5 [:a :b]
      1/2 [:a :b :d]
      2/3 [:a :b :d :e])))

(deftest agenda-frontlog-section-excluded-from-effort-queue
  (let [now (ZonedDateTime/of 2023 4 5 7 37 15 0 oa/CHICAGO)
        header-line (fn [title]
                      (format "═════════✦ %s ✦═════════" title))
        section-text (fn [agenda title next-title]
                       (let [header (header-line title)
                             start (.indexOf agenda header)]
                         (is (not= -1 start)
                             (format "Missing %s header" title))
                         (let [after (+ start (count header) 1)
                               end (if next-title
                                     (let [next-header (header-line next-title)
                                           idx (.indexOf agenda next-header)]
                                       (is (not= -1 idx)
                                           (format "Missing %s header" next-title))
                                       idx)
                                     (count agenda))]
                           (subs agenda after end))))]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO Hidden from queue"
       "  :PROPERTIES:"
       "  :Effort: 0:30"
       "  :AGENDA_FRONTLOG_SECTION: Special"
       "  :END:"
       "* TODO Normal item"
       "  :PROPERTIES:"
       "  :Effort: 0:15"
       "  :END:")}
     now
     (fn [agenda]
       (let [today (section-text agenda "TODAY" "CALENDAR")
             later (section-text agenda "LATER" "TRIAGE")]
         (is (re-find #"TODO Normal item" today))
         (is (not (re-find #"TODO Hidden from queue" today)))
         (is (not (re-find #"TODO Hidden from queue" later))))))))
