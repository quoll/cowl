(ns cowl.impl
  {:doc "Base implementations for the COWL protocols"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [tiara.data :as data :refer [ordered-map]]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot]
            [cowl.io :as cio])
  (:import [cowl.protocols Annotatable Streamable Property ObjectPropertyProtocol Document]
           [tiara.data VecMap]
           [quoll.rdf IRI]))

(def local-id (rdf/iri "#"))
(def initv "0.0.1")

(def default-pre "")

(def owl-annotation-props
  #{:rdfs/label :rdfs/comment :rdfs/seeAlso :rdfs/isDefinedBy
    :owl/versionInfo :owl/deprecated :owl/backwardCompatibleWith
    :owl/incompatibleWith :owl/priorVersion})

(def om data/EMPTY_MAP)

(def os data/EMPTY_SET)

(def mm data/EMPTY_MULTI_MAP)

(defn localized-iri
  "Takes a full IRI form and identifies if it can be converted to a prefix/local pair using the
  provided prefix map. Uses a linear search through the namespaces.
  TODO: This should be part of RuDolF."
  [prefixes i]
  (if (and (instance? IRI i) (:local i))
    i
    (let [iri-str (if (string? i) i (:iri i))]
      (if-let [[pre nmspace] (->> prefixes
                                  (keep (fn [[_ nmsp :as pn]]
                                          (when (str/starts-with? iri-str nmsp) pn)))
                                  first)]
        (rdf/iri iri-str pre (subs iri-str (count nmspace)))
        (if (string? i) (rdf/iri iri-str) i)))))


(defrecord ObjectProperty [id annotations super-props equivs domain range disjoints
                           fn? inverse-fn? transitive? symmetric? asymmetric?
                           reflexive? irreflexive?]
  Annotatable
  (annotate [this prop text] (update this :annotations assoc prop text))
  (annotate [_ id prop text]
    (throw (ex-info "Object Properties do not contain other entities" {:id id :prop prop :text text})))
  Property
  (sub-property [this other] (update this :super-props conj other))
  (equivalent [this other] (update this :equivs conj other))
  (domain-of [this other] (update this :domain conj other))
  (range-of [this other] (update this :range conj other))
  (disjoint [this other] (update this :disjoints conj other))
  (functional [this] (assoc this :fn? true))
  ObjectPropertyProtocol
  (inverse-functional [this] (assoc this :inverse-fn? true))
  (transitive [this] (assoc this :transitive? true))
  (symmetric [this] (assoc this :symmetric? true))
  (asymmetric [this] (assoc this :asymmetric? true))
  (reflexive [this] (assoc this :reflexive? true))
  (irreflexive [this] (assoc this :irreflexive? true)))

(defn obj-property
  [id]
  (->ObjectProperty id mm os os os os os false false false false false false false))

(defn check
  "Test that a value is one of a known set"
  ([expected value] (check expected value "Unexpected value"))
  ([expected value msg]
   (when-not (contains? expected value)
     (throw (ex-info (str msg ": " value) {:value value})))))

(defn ominus
  "Performs a set minus operation, where the first set is ordered, and s2 may be a seq."
  [s1 s2]
  (persistent!
   (reduce (fn [result item] (if (contains? s1 item) (disj! result item) result))
           (transient s1)
           s2)))

(defn struct-merge
  "Merges records so that sets and maps are added to"
  [struct1 struct2]
  (reduce (fn [result [k v]]
            (cond
              (set? v) (let [sv (get result k)]
                         (if (set? sv)
                           (assoc result k (into sv v))
                           (assoc result k v)))
              (map? v) (let [mv (get result k)]
                         (if (map? mv)
                           (assoc result k (merge mv v))
                           (assoc result k v)))
              :else (assoc result k v)))
          struct1 struct2))

(defrecord OWLDocument [id version class-idx oprop-idx dprop-idx annotations
                        instance-idx prefixes annotation-props datatypes]
  Annotatable
  (annotate [this prop text]
    (let [new-prop (not (or (contains? owl-annotation-props prop)
                            (contains? annotation-props prop)))]
      (cond-> (update this :annotations assoc (rdf/curie prefixes prop) text)
        new-prop (update :annotation-props conj prop))))
  Document
  (object-property [this {:keys [id annotations super-props equivs domain range disjoints] :as prop}]
    (let [->iri #(localized-iri prefixes %)
          annotations* (into om (map (fn [[a v]] [(->iri a) v])) (seq annotations))
          new-aprops (ominus annotation-props (keys annotations*))
          prop* (cond-> prop
                  (seq super-props) (update :super-props mapv ->iri)
                  (seq equivs) (update :equivs mapv ->iri)
                  (seq domain) (update :domain mapv ->iri)
                  (seq range) (update :range mapv ->iri)
                  (seq disjoints) (update :disjoints mapv ->iri))]
      (cond-> (update this :oprop-idx update id struct-merge prop*)
        (seq new-aprops) (update :annotation-props into new-aprops))))
  Streamable
  (emit [_ stream]
    (cio/write-prefixes stream prefixes)
    (cio/start-doc stream id version)
    (cio/write-doc-annotations stream annotations)
    (cio/write-declarations stream (keys class-idx) (keys oprop-idx) (keys dprop-idx) annotation-props datatypes)
    (cio/write-oprops stream oprop-idx)
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
  "Ensures that a map is an ordered map. Acts as identity if `mp` is already ordered.
  TODO: This can be part of Tiara"
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

(defn owl
  ([] (owl nil nil nil))
  ([id] (owl id nil nil))
  ([id version] (owl id version nil))
  ([id version prefixes]
   (let [pfxs (standardize-prefixes id (or prefixes rdf/common-prefixes))
         ont-iri (if id (localized-iri pfxs id) local-id)
         vi (as-namespace (:iri ont-iri))
         ver-iri (if version
                   (localized-iri pfxs
                                  (if (str/index-of version "/") ;; proxy for an IRI form
                                    version
                                    (str vi version)))
                   (rdf/iri (str vi initv)))]
     (->OWLDocument ont-iri ver-iri om om om mm om pfxs os os))))

