(ns cowl.impl.common
  {:doc "Common utilities shared by property implementations"
   :author "Paula Gearon"}
  (:require [tiara.data :as data]))

(def om data/EMPTY_MAP)

(def os data/EMPTY_SET)

(def mm data/EMPTY_MULTI_MAP)

(defn mapos
  "Maps all elements in an ordered set, with the result being an ordered set"
  [f s]
  (into os (map f) s))

(defn value-deepmap
  "Maps a function down a nested ordered map where the values at the top level are vectors"
  [f m]
  (let [sf (fn [a] (if (sequential? a) (mapv f a) (f a)))]
    (into om (map (fn [[k v]] [k (mapv sf v)])) m)))

(defn recontextualize-annotations
  "Calls recontextualize on an annotations nested array, using refn to update document elements or
  raw entities (IRIs, keywords, etc)"
  [entity refn]
  (update entity :annotations value-deepmap refn))

(defn prop-attr
  [obj index other]
  (assert (= (cowl.protocols/id obj) (cowl.protocols/id other)))
  (-> obj
      (update index conj other)
      (update-in [:annotations index] conj (:annotations other))))

(defn prop-bool-attr
  ([obj index] (prop-bool-attr obj index nil))
  ([obj index attr]
   (-> obj
       (assoc index attr)
       (update-in [:annotations index] conj attr))))
