(ns com.gfredericks.org-sync
  "Generic syncing logic."
  (:refer-clojure :exclude [merge])
  (:require
   [com.gfredericks.org-editor :as org])
  (:import (java.time ZonedDateTime ZoneId)))

;; Add this to top:

(defn ^:private prelude
  [category]
  )

(defn ^:private wrap-in-archive-file-prelude
  [time-zone category sections]
  {::org/prelude
   (filterv identity
            ["#    -*- mode: org -*-"
             (when category (str "#+CATEGORY: " category))
             ""
             "This file is generated automatically; edits may not be preserved."
             ""
             (str "This file was last generated at " (ZonedDateTime/now time-zone))])
   ::org/sections sections})

(defn ^:private wrap-in-live-file-prelude
  [time-zone category sections]
  {::org/prelude
   (filterv identity
            ["-*- eval: (auto-revert-mode 1); eval: (real-auto-save-mode 1); real-auto-save-interval: 1 -*-"
             (when category (str "#+CATEGORY: " category))
             ""
             "This file is generated automatically; edits may not be preserved."
             ""
             (str "This file was last generated at " (ZonedDateTime/now time-zone))])
   ::org/sections sections})

(defn default-merge-fn
  [existing-entry new-entry]
  (when-not (empty? (::org/sections new-entry))
    (throw (ex-info "default-merge-fn does not support subsections in new entries"
                    {:new-entry new-entry})))
  (let [new-entry (assoc new-entry ::org/sections (::org/sections existing-entry))]
    (reduce (fn [entry [k v]]
              (org/prop-assoc entry k v))
            new-entry
            (let [new-props (org/read-properties new-entry)
                  existing-props (org/read-properties existing-entry)]
              (remove (comp new-props key) existing-props)))))

(defn nil-if-equivalent
  [new-file existing-file]
  (if (and (= (::org/sections new-file)
              (::org/sections existing-file))
           ;; don't check timestamp
           (= (butlast (::org/prelude new-file))
              (butlast (::org/prelude existing-file))))
    nil
    new-file))

(defn merge
  "First arg is a map of :live, :archive, :deleted, each being
  a parsed org file.

  Returns an updated version of the first arg, but where the values
  can be nil if the contents haven't changed.

  new-entries is a list of org entries, and opts is a map
  containing:

    :time-zone (required): a java.time.ZoneId object which will
                           determine how timestamps are displayed
    :category  (optional): a string, will be added to a #+CATEGORY
                           entry at the top of the files
    :archive?  (optional): a predicate of an org entry that
                           determines if it should go in the archive
                           file; defaults to (constantly false),
                           in which case nothing will ever be moved
                           to the archive file
    :id-fn     (required): a function of an org entry that returns
                           an identifier that will be used to match
                           the new entries against existing entries
    :merge-fn  (optional): a function of an existing org entry and
                           a new org entry that merges their
                           contents; defaults to a function that
                           takes the header and prelude from the new
                           entry and the subsections from the existing
                           entry, and merges the properties, giving
                           preference to the new entry.
    :sort-key  (optional): a function of an org entry that returns
                           a comparable value on which to sort the
                           entries in both files; defaults to the
                           supplied :id-fn"
  [{:keys [live archive deleted]} new-entries opts]
  (let [{:keys [time-zone category archive? id-fn merge-fn sort-key]
         :or {archive? (constantly false)
              merge-fn default-merge-fn}}
        opts

        sort-key (or sort-key id-fn)

        new-entries-by-id (->> new-entries
                               (map (juxt id-fn identity))
                               (into {}))

        all-entries (concat (::org/sections live)
                            (::org/sections archive))
        all-entries-by-id (->> all-entries
                               (map (juxt id-fn identity))
                               (into {}))]
    (when-not (= (count all-entries)
                 (count all-entries-by-id))
      (throw (ex-info "Colliding IDs!"
                      {:example-id-and-cardinality (->> all-entries
                                                        (map id-fn)
                                                        frequencies
                                                        (filter #(< 1 (val %)))
                                                        (first))})))
    (let [[retained-entries new-deleted-entries]
          ((juxt filter remove) (comp new-entries-by-id id-fn) all-entries)

          retained-entries-by-id (->> retained-entries
                                      (map (juxt id-fn identity))
                                      (into {}))

          merged-entries (reduce (fn [entries-by-id new-entry]
                                   (update entries-by-id (id-fn new-entry)
                                           (fn [maybe-existing-entry]
                                             (if maybe-existing-entry
                                               (merge-fn maybe-existing-entry new-entry)
                                               new-entry))))
                                 retained-entries-by-id
                                 new-entries)
          new-live-file-contents (->> (vals merged-entries)
                                      (remove archive?)
                                      (sort-by sort-key)
                                      (wrap-in-live-file-prelude time-zone category))
          new-archive-file-contents (->> (vals merged-entries)
                                         (filter archive?)
                                         (sort-by sort-key)
                                         (wrap-in-archive-file-prelude time-zone category))]
      {:live    (nil-if-equivalent new-live-file-contents live)
       :archive (nil-if-equivalent new-archive-file-contents archive)
       :deleted (when (seq new-deleted-entries)
                  (wrap-in-archive-file-prelude time-zone category
                                                (concat (::org/sections deleted)
                                                        new-deleted-entries)))})))