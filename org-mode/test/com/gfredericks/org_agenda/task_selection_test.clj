(ns com.gfredericks.org-agenda.task-selection-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.gfredericks.org-agenda.task-selection :as selection])
  (:import (java.time Duration)))

(defn minutes [n] (Duration/ofMinutes n))
(defn task [id n tags] {:id id :effort (minutes n) :own-tags (set tags)})
(defn pool [n pred] {:budget (minutes n) :item-allowed? pred})
(def any-pool (fn [n] (pool n (constantly true))))
(defn tagged-pool [n tag] (pool n #(contains? (:own-tags %) tag)))
(defn ids [[selected deferred]] [(mapv :id selected) (mapv :id deferred)])

(deftest single-pool-priority-and-order
  (let [todos [(task :a 7 []) (task :b 3 []) (task :c 10 [])
               (task :d 1 []) (task :e 5 [])]]
    (is (= [[:a :b :d] [:c :e]]
           (ids (selection/select-tasks [(any-pool 11)] todos))))
    (is (= [[] [:a :b :c :d :e]]
           (ids (selection/select-tasks [(any-pool 0)] todos))))
    (is (= [[:a :b :c :d :e] []]
           (ids (selection/select-tasks [(any-pool 30)] todos))))
    (is (= [[] []] (selection/select-tasks [(any-pool 10)] [])))
    (is (= [[:a :b :d] [:c :e]]
           (ids (selection/select-tasks [{:budget (minutes 11)}] todos))))))

(deftest overlapping-pools
  (let [away? #(not (contains? (:own-tags %) "home-only"))
        specs [(any-pool 30) (pool 120 away?)]
        away (task :away 30 [])
        home (task :home 30 ["home-only"])]
    (testing "flexible work must not unnecessarily consume general capacity"
      (is (= [[:away :home] []]
             (ids (selection/select-tasks specs [away home]))))
      (is (= [[:home :away] []]
             (ids (selection/select-tasks specs [home away])))))
    (testing "general capacity is shared rather than duplicated"
      (is (= [[:away :home] [:away2 :home2]]
             (ids (selection/select-tasks specs
                                         [away home (task :away2 120 [])
                                          (task :home2 1 ["home-only"])])))))
    (testing "effort of one task can be split between pools"
      (is (= [[:long] []]
             (ids (selection/select-tasks [(any-pool 30) (pool 120 away?)]
                                         [(task :long 150 [])])))))))

(deftest eligibility-and-boundaries
  (let [phone (tagged-pool 20 "phone")
        home (tagged-pool 10 "home")]
    (is (= [[:both :home] [:neither :phone]]
           (ids (selection/select-tasks [phone home]
                                       [(task :both 15 ["phone" "home"])
                                        (task :neither 0 [])
                                        (task :home 10 ["home"])
                                        (task :phone 6 ["phone"])]))))
    (is (false? (selection/feasible? [phone home] [(task :none 0 [])])))
    (is (true? (selection/feasible? [phone home] [(task :zero 0 ["phone"])])))
    (is (true? (selection/feasible? [] [])))
    (is (false? (selection/feasible? [] [(task :x 1 [])])))
    (is (= [[] [:x]] (ids (selection/select-tasks [] [(task :x 1 [])]))))))

(deftest subset-constraints
  (let [specs [(tagged-pool 5 "a") (tagged-pool 5 "b")
               (tagged-pool 5 "c")]]
    (testing "total capacity alone is not sufficient"
      (is (false? (selection/feasible? specs [(task :a 6 ["a"])]))))
    (testing "a pair of pools can be oversubscribed even with a spare third"
      (is (false? (selection/feasible? specs
                                            [(task :ab 11 ["a" "b"])]))))
    (testing "work eligible everywhere can use all three pools"
      (is (true? (selection/feasible? specs
                                           [(task :abc 15 ["a" "b" "c"])]))))
    (is (false? (selection/feasible? specs
                                          [(task :abc 16 ["a" "b" "c"])])))))

(deftest budgets-and-precision
  (is (false? (selection/feasible?
               [{:budget (Duration/ofMillis 1000) :item-allowed? (constantly true)}]
               [{:effort (Duration/ofMillis 1001)}])))
  (is (thrown? clojure.lang.ExceptionInfo
               (selection/select-tasks [(any-pool -1)] [])))
  (is (thrown? clojure.lang.ExceptionInfo
               (selection/feasible? [(any-pool 1)] [(task :x -1 [])])))
  (is (thrown? clojure.lang.ExceptionInfo
               (selection/feasible? (repeat 17 (any-pool 1)) []))))

;; Independent integer-unit flow oracle: repeatedly allocate one unit of
;; demand to an eligible pool, backtracking if that choice blocks later work.
(defn brute-feasible? [budgets demands]
  (letfn [(step [demands remaining]
            (if (empty? demands)
              true
              (let [[eligible n] (first demands)]
                (if (zero? n)
                  (boolean (and (seq eligible) (step (rest demands) remaining)))
                  (boolean
                   (some (fn [i]
                           (when (pos? (remaining i))
                             (step (cons [eligible (dec n)] (rest demands))
                                   (update remaining i dec))))
                         eligible))))))]
    (step demands (vec budgets))))

(deftest exhaustive-small-flow-cross-check
  (let [masks (range 8)
        budgets [1 2 1]
        specs (mapv (fn [i]
                      (pool (budgets i) #(bit-test (:mask %) i)))
                    (range 3))]
    ;; All pairs of eligibility patterns and small demands, including
    ;; impossible patterns and demand exceeding individual capacities.
    (doseq [a masks b masks x (range 4) y (range 4)]
      (let [todos [{:mask a :effort (minutes x)}
                   {:mask b :effort (minutes y)}]
            demands [[(filterv #(bit-test a %) (range 3)) x]
                     [(filterv #(bit-test b %) (range 3)) y]]]
        (is (= (brute-feasible? budgets demands)
               (selection/feasible? specs todos))
            (str "masks " a ", " b "; efforts " x ", " y))))))
