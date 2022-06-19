(ns com.gfredericks.org-agenda-test
  (:require
   [clojure.java.io :as io]
   [clojure.test :refer [are deftest is]]
   [com.gfredericks.org-agenda :as oa])
  (:import (java.time LocalDate LocalDateTime)))

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
    [(LD "2022-06-19") (LD "2022-06-26")]))
