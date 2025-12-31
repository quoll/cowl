(ns cowl.impl
  {:doc "Base implementations for the COWL protocols"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [tiara.data :as data :refer [ordered-map]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io :as cio])
  (:import [cowl.protocols Annotatable Streamable]
           [tiara.data VecMap]))

(def local-id (rdf/iri "#"))
(def initv "v0.0.1")

(def default-pre "")

(def ^:dynamic *annotation-props*
  #{:rdfs/label :rdfs/comment :rdfs/seeAlso :rdfs/isDefinedBy
    :owl/versionInfo :owl/deprecated :owl/backwardCompatibleWith
    :owl/incompatibleWith :owl/priorVersion})

(def om data/EMPTY_MAP)

(def mm data/EMPTY_MULTI_MAP)

(defn check
  "Test that a value is one of a known set"
  ([expected value] (check expected value "Unexpected value"))
  ([expected value msg]
   (when-not (contains? expected value)
     (throw (ex-info (str msg ": " value) {:value value})))))

(defrecord OWLDocument [id version class-idx oprop-idx dprop-idx annotations instance-idx prefixes]
  Annotatable
  (annotate [this prop text]
    (check *annotation-props* prop "Unexpected annotation property")
    (update this :annotations assoc (rdf/curie prefixes prop) text))
  Streamable
  (emit [_ stream]
    (cio/write-prefixes stream prefixes)
    (cio/start-doc stream id version)
    (cio/write-doc-annotations stream annotations)
    (cio/end-doc stream)))

(defn as-namespace
  "Converts an IRI into a namespace IRI"
  [base-iri]
  (if base-iri
    (let [lst (last base-iri)]
      (if (#{\/ \#} lst)
        base-iri
        (str base-iri "/")))
    "#"))

(defn ->ordered-map
  "Ensures that a map is an ordered map. Acts as identity if `mp` is already ordered."
  [mp]
  (if (instance? VecMap mp)
    mp
    (into om (seq mp))))

(defn standardize-prefixes
  "Standardizes a prefix list to always use the `default-pre` (empty-keyword) as the default.
  If no default prefix exists, then create one from the provided iri."
  [iri prefixes]
  (let [[dk default] (->> [:_default (keyword "") nil]
                          (keep (fn [k] (when-let [n (get prefixes k)] [k n])))
                          first)]
    (if default
      (->> (dissoc prefixes dk)
           (into (ordered-map default-pre default)))
      (if (get prefixes default-pre)
        (->ordered-map prefixes)
        (into (ordered-map default-pre (as-namespace iri)) prefixes)))))

(defn localized-iri
  "Takes a full IRI form and identifies if it can be converted to a prefix/local pair using the
  provided prefix map. Uses a linear search through the namespaces.
  TODO: This should be part of RuDolF."
  [prefixes iri-str]
  (if-let [[pre nmspace] (->> prefixes
                              (keep (fn [[_ nmsp :as pn]]
                                      (when (str/starts-with? iri-str nmsp) pn)))
                              first)]
    (rdf/iri iri-str pre (subs iri-str (count nmspace)))
    (rdf/iri iri-str)))


(defn owl
  ([] (owl nil nil nil))
  ([id] (owl id nil nil))
  ([id version] (owl id version nil))
  ([id version prefixes]
   (let [pfxs (standardize-prefixes id (or prefixes rdf/common-prefixes))
         ont-iri (if id (localized-iri pfxs id) local-id)
         vi (as-namespace (:iri ont-iri))
         ver-iri (localized-iri pfxs (if version
                                       (if (str/index-of version "/")  ;; proxy for an IRI form
                                         version
                                         (str vi version))
                                       (str vi initv)))]
     (->OWLDocument ont-iri ver-iri om om om mm om pfxs))))
