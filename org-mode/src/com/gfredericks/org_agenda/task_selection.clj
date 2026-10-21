(ns com.gfredericks.org-agenda.task-selection
  (:import (java.time Duration)))

(defn- millis
  [^Duration duration]
  (let [n (.toMillis duration)]
    (when (neg? n)
      (throw (ex-info "Effort and budgets must be nonnegative" {:duration duration})))
    n))

(defn- eligibility-mask
  [specs todo]
  (reduce-kv (fn [mask i {:keys [item-allowed?]}]
               (if ((or item-allowed? (constantly true)) todo) (bit-set mask i) mask))
             0 specs))

(defn- feasible-demands?
  [budgets demands]
  (let [k (count budgets)]
    (and (not (contains? demands 0))
         ;; For every set of pools S, work whose eligible pools are all
         ;; in S must fit in the combined capacity of S. This is the
         ;; max-flow/min-cut condition when effort may be split among pools.
         (every? (fn [subset]
                   (<= (reduce-kv (fn [sum mask effort]
                                    (if (= mask (bit-and mask subset))
                                      (+ sum effort)
                                      sum))
                                  0 demands)
                       (reduce-kv (fn [sum i budget]
                                    (if (bit-test subset i) (+ sum budget) sum))
                                  0 budgets)))
                 (range (bit-shift-left 1 k))))))

(defn- prepare-specs
  [specs]
  (let [specs (vec specs)]
    ;; Subset enumeration is exponential; avoid both pathological calls and
    ;; bit-mask overflow. Intended for a handful of overlapping pools.
    (when (> (count specs) 16)
      (throw (ex-info "At most 16 effort pools are supported" {:count (count specs)})))
    [specs (mapv (comp millis :budget) specs)]))

(defn feasible?
  "Whether all todos' efforts can be divided among eligible pools.

  Each spec has a Duration :budget and an optional :item-allowed? predicate on a todo (default: always true).
  Each todo has a Duration :effort. An item with no eligible pool is
  infeasible, even if its effort is zero. Intended for few pools (2^k
  subset checks for k pools)."
  [specs todos]
  (let [[specs budgets] (prepare-specs specs)
        demands (reduce (fn [demands todo]
                          (let [mask (eligibility-mask specs todo)]
                            (update demands mask (fnil + 0) (millis (:effort todo)))))
                        {} todos)]
    (feasible-demands? budgets demands)))

(defn select-tasks
  "Select todos in priority order, returning [selected deferred].

  Accept an item if it can be added without removing previously selected
  items. Effort is divisible between overlapping pools; tasks themselves
  are either selected or deferred. Both results retain input order.
  This function does not force high-priority items or select a fallback
  when no task fits; callers can apply such policies separately."
  [specs todos]
  (let [[specs budgets] (prepare-specs specs)]
    (let [[selected deferred _]
          (reduce (fn [[selected deferred demands] todo]
                    (let [mask (eligibility-mask specs todo)
                          candidate (update demands mask (fnil + 0) (millis (:effort todo)))]
                      (if (feasible-demands? budgets candidate)
                        [(conj selected todo) deferred candidate]
                        [selected (conj deferred todo) demands])))
                  [[] [] {}] todos)]
      [selected deferred])))
