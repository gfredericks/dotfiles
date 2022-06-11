(ns com.gfredericks.org-agenda-test
  (:require
   [clojure.test :refer [deftest is]]
   [com.gfredericks.org-agenda :as oa])
  (:import (java.time LocalDateTime)))

(deftest parse-org-datetime-test
  (is (= (LocalDateTime/parse "2022-06-11T11:14:00")
         (oa/parse-org-datetime "[2022-06-11 Sat 11:14]"))))
