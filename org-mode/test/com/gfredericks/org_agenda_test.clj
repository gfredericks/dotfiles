(ns com.gfredericks.org-agenda-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [are deftest is]]
   [com.gfredericks.org-agenda :as oa])
  (:import
   (java.time LocalDate LocalDateTime ZonedDateTime)
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

(deftest apply-repeater-test
  (are [repeater base today max-date out] (= out (oa/apply-repeater repeater base today max-date))
    ["+" 3 "d"] (LD "2022-06-18") (LD "2022-09-20") (LD "2022-07-01")
    [(LD "2022-06-18") (LD "2022-06-21") (LD "2022-06-24") (LD "2022-06-27")
     (LD "2022-06-30")]

    ["++" 3 "d"] (LD "2022-06-18") (LD "2022-09-20") (LD "2022-10-01")
    [(LD "2022-06-18") (LD "2022-09-22") (LD "2022-09-25") (LD "2022-09-28") (LD "2022-10-01")]

    [".+" 2 "w"] (LD "2022-06-18") (LD "2022-09-20") (LD "2022-11-03")
    [(LD "2022-06-18") (LD "2022-10-04") (LD "2022-10-18") (LD "2022-11-01")]

    ["++" 1 "w"] (LD "2022-06-19") (LD "2022-06-19") (LD "2022-07-01")
    [(LD "2022-06-19") (LD "2022-06-26")]

    [".+" 1 "d"] (LD "2022-07-11") (LD "2022-07-10") (LD "2022-07-13")
    [(LD "2022-07-11") (LD "2022-07-12") (LD "2022-07-13")]))

(deftest make-org-link-test
  (is (= "[[file:/fake-file.org::*TODO this has an extra TODO][TODO TODO this has an extra TODO]]"
         (oa/make-org-link
          {:header "TODO this has an extra TODO"
           :file "/fake-file.org"}
          "TODO TODO this has an extra TODO")))
  (is (= "[[file:/fake-file.org::*TODO this headline has \\[brackets\\]][TODO this headline has &#x5b;brackets&#x5d;]]"
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
                     :agenda-file (str agenda)}
                    now)
        (agenda-validator (slurp (str agenda)))
        (finally
          (doseq [filename (keys filenames->contents)]
            (Files/delete (.resolve dir filename)))
          (Files/delete dir)
          (Files/delete agenda))))))

(deftest repeat-repeater-regression
  (let [now (ZonedDateTime/of 2022 7 10 16 22 15 0 oa/CHICAGO)]
    (doseq [rep [".+1d" "++1d"]]
      (do-integration
       {"only-file.org"
        (lines
         "* TODO this should just appear once a day"
         (format "  SCHEDULED: <2022-07-11 Mon %s>" rep))}
       now
       (fn [agenda]
         (is (= 20 (count (re-seq #"just appear once" agenda)))))))))

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
     (is (= 1 (count (re-seq #"TODO" agenda))))
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
       (is (re-find #"in 2 days.+TODO.+This thing" agenda)))))
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  DEADLINE: <2022-12-14 Wed>")}
     now
     (fn [agenda]
       (is (re-find #"9 days overdue!.+TODO.+This thing" agenda))))))

(deftest deadline-hiding-rules-test
  (let [now (ZonedDateTime/of 2022 12 23 9 22 15 0 oa/CHICAGO)
        deadline-section #(second (re-matches #"(?s)(.*?)== (TODAY|TRIAGE) ==.*" %))]
    ;; default warning period is 14 days
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  DEADLINE: <2023-02-03 Fri>")}
     now
     (fn [agenda]
       (is (not (re-find #"days.+This thing" (deadline-section agenda))))
       ))
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
       "  :BACKLOG_SECTION: truths"
       "  :END:")}
     now
     (fn [agenda]
       (is (re-find #"truths" agenda))
       (is (re-find #"TODO.+This thing" agenda))))))

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

(deftest schedule-dow-test
  (let [now (ZonedDateTime/of 2023 4 5 7 37 15 221 oa/CHICAGO)
        test-regex #"Bad SCHEDULED_DOW.+This thing"]
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  SCHEDULED: <2023-04-05 Wed>")}
     now
     (fn [agenda]
       (is (not (re-find test-regex agenda)))))
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  SCHEDULED: <2023-04-05 Wed>"
       "  :PROPERTIES:"
       "  :SCHEDULED_DOW: WEDNESDAY"
       "  :END:")}
     now
     (fn [agenda]
       (is (not (re-find test-regex agenda)))))
    (do-integration
     {"only-file.org"
      (lines
       "* TODO This thing"
       "  SCHEDULED: <2023-04-05 Wed>"
       "  :PROPERTIES:"
       "  :SCHEDULED_DOW: SUNDAY"
       "  :END:")}
     now
     (fn [agenda]
       (is (re-find test-regex agenda))))))
