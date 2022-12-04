(ns com.gfredericks.org-agenda-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [are deftest is]]
   [com.gfredericks.org-agenda :as oa])
  (:import (java.time LocalDate LocalDateTime ZonedDateTime)))

(defn LDT [s] (LocalDateTime/parse s))
(defn LD [s] (LocalDate/parse s))

(deftest parse-org-datetime-test
  (is (= (LDT "2022-06-11T11:14:00")
         (oa/parse-org-datetime "[2022-06-11 Sat 11:14]"))))

(deftest timestamp-finder-test
  (are [format-str line out] (= out ((oa/timestamp-finder format-str) line))
    "%s" "[2022-06-11 Sat 11:14]" (LDT "2022-06-11T11:14:00")
    "%s" "<2022-06-18>" (LD "2022-06-18")
    " heyo %s.*" " heyo <2022-01-01 Sat 08:00-10:43> anything" [(LDT "2022-01-01T08:00")
                                                                (LDT "2022-01-01T10:43")]
    "%s" "<2022-06-18 Sat +3d>" {:repeater ["+" 3 "d"] :base (LD "2022-06-18")}
    "%s" "<2022-06-18 Sat ++3d>" {:repeater ["++" 3 "d"] :base (LD "2022-06-18")}
    "%s" "<2022-06-18 Sat 19:00 .+1w>" {:repeater [".+" 1 "w"] :base (LDT "2022-06-18T19:00")}))

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
    (let [dir (java.nio.file.Files/createTempDirectory "org-agenda-tests-" empty-file-attrs)
          agenda (str (java.nio.file.Files/createTempFile "org-agenda-tests-" ".org" empty-file-attrs))]
      (doseq [[filename contents] filenames->contents]
        (spit (str dir "/" filename) contents))
      (oa/do-once {:directory (str dir)
                   :agenda-file agenda}
                  now)
      (agenda-validator (slurp agenda)))))

(deftest repeat-repeater-regression
  (let [now (ZonedDateTime/of 2022 7 10 16 22 15 0 oa/CHICAGO)]
    (doseq [rep [".+1d" "++1d"]]
      (do-integration
       {"only-file.org"
        (str "* TODO this should just appear once a day\n"
             (format "  SCHEDULED: <2022-07-11 Mon %s>\n"
                     rep))}
       now
       (fn [agenda]
         (is (= 20 (count (re-seq #"just appear once" agenda)))))))))

(comment
  ;; Currently failing
  (deftest ordered-test
    (do-integration
     {"only-file.org"
      (str "* Hello\n"
           "  :PROPERTIES:\n"
           "  :ORDERED: t\n"
           "  :END:\n"
           "** TODO Aaa\n"
           "*** TODO Bbb\n"
           "** TODO Ccc\n"
           "*** TODO Ddd\n")}
     ;; doesn't matter
     (ZonedDateTime/of 2022 7 10 16 22 15 0 oa/CHICAGO)
     (fn [agenda]
       (is (= 1 (count (re-seq #"TODO" agenda))))
       (is (re-find #"Bbb" agenda))))))
