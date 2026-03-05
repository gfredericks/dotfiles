(ns com.gfredericks.org-context
  (:require
   [clojure.java.shell :refer [sh]]
   [clojure.string :as string]
   [com.gfredericks.compare :as compare]
   [com.gfredericks.org-agenda :refer [CHICAGO parse-org-datetime with-atomic-write-with-postamble-to make-org-link]
    :as org-agenda]
   [plumbing.core :refer [distinct-by]])
  (:import
   (java.time LocalDate LocalDateTime LocalTime Period ZonedDateTime ZoneId)))

(defn now-local-datetime
  []
  (.toLocalDateTime (ZonedDateTime/now CHICAGO)))

(defn ^:private run-script
  [bash-string]
  (let [{:keys [err out exit]} (sh "/bin/bash" "-c" bash-string)
        [res msg] (case exit
                    0
                    [:unblocked out]
                    1
                    [:blocked out]
                    [:error err])]
    [res (if (seq msg) (string/trim msg))]))

(defn context-data
  [agenda-data]
  (let [items (distinct-by (juxt :file :line-number)
                           (concat (:frontlog-free agenda-data)
                                   (:frontlog-today agenda-data)
                                   (:triage agenda-data)
                                   (:frontlog-later agenda-data)
                                   (:deadlines agenda-data)))
        in-progress (->> items
                         (filter #(= "TODO" (:todo %)))
                         (filter #(->> (:props-with-ancestors %)
                                       (keep (fn [m] (get m "IN_PROGRESS")))
                                       (first)
                                       (= "t")))
                         (map (fn [{:keys [own-properties first-unchecked-item header] :as item}]
                                (let [blocked-script (get own-properties "BLOCKED_SCRIPT")
                                      [blocked-script-res blocked-script-msg] (some-> blocked-script run-script)
                                      state (case blocked-script-res
                                              nil :unblocked
                                              :unblocked :unblocked
                                              :blocked :blocked
                                              :error :error)
                                      ;; TODO: extract this to a function and also look at the timestamps
                                      ;; in the logbook
                                      updated-at (or (some-> (get own-properties "UPDATED_AT")
                                                             parse-org-datetime
                                                             (.atZone CHICAGO)
                                                             .toEpochSecond
                                                             -)
                                                     0)
                                      title (or first-unchecked-item header)]
                                  (assoc (select-keys item
                                                      [:properties :file :header :followup-note])
                                         :title title
                                         :state state
                                         :blocked-script-msg blocked-script-msg
                                         :context  (or (->> (:props-with-ancestors item)
                                                            (keep #(get % "IN_PROGRESS_CONTEXT"))
                                                            (first))
                                                       (->> (cond->> (->> (:ancestor-headers item)
                                                                          (rest)
                                                                          (reverse)
                                                                          (map (fn [s]
                                                                                 (second (re-matches #"\*+ (?:TODO )?(.*?)((  +:(\w+:)+)$)?" s)))))
                                                              first-unchecked-item
                                                              (cons header))
                                                            (string/join " ~ ")))
                                         :sort-key [(= :blocked state) updated-at]))))
                         (sort-by :sort-key)
                         (map #(dissoc % :sort-key)))]
    {:in-progress in-progress}))

(defn print-context-data
  [{:keys [in-progress]}]
  (doseq [{:keys [context title followup-note state blocked-script-msg] :as item} in-progress
          :let [formatted-state
                (case state
                  :blocked
                  (org-agenda/red-bold "BLOCKED  ")

                  :unblocked
                  (org-agenda/green-bold "UNBLOCKED")

                  :error
                  (org-agenda/yellow-italic "ERROR    ")

                  (format "WHAT EVEN IS %s" state))]]
    (printf "- %s  %s\n"
            formatted-state
            (make-org-link item title))
    (when blocked-script-msg
      (printf "  %s\n" (org-agenda/blue-bold
                        (if (= blocked-script-msg "timed out waiting for false")
                          "timed out"
                          blocked-script-msg))))
    (when context
      (printf "  - (%s)\n" context))
    (if followup-note
      (printf "  - %s\n" followup-note))
    (when-let [debug (:debug item)]
      (printf "  DEBUG: %s\n" (pr-str debug)))))
