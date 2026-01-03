(ns cowl.io
  {:doc "IO operations for COWL"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [quoll.rdf :as rdf]
            [cowl.protocols :as prot :refer [emit legal-inline-subprop?]])
  (:import [java.io Writer StringWriter]))

(defn write-iri
  [^Writer stream i]
  (.write stream (str i)))

(defn start-doc
  [^Writer stream id version]
  (.write stream "Ontology(")
  (.write stream (str id "\n"))
  (.write stream (str version "\n\n")))

(defn end-doc
  [^Writer stream]
  (.write stream ")\n"))

(defn write-prefixes
  [^Writer stream prefixes]
  ()
  (doseq [[pre nmsp] prefixes]
    (.write stream "Prefix(")
    (.write stream (name pre))
    (.write stream ":=<")
    (.write stream (str nmsp))
    (.write stream ">)\n"))
  (.write stream "\n"))

(defn escape
  [s]
  (str/replace s "\"" "\\\""))

(defn write-annotation
  [^Writer stream {:keys [annotations prop value]}]
  (.write stream "Annotation(")
  (doseq [ann annotations]
    (emit ann stream)
    (.write stream \space))
  (.write stream (str prop))
  (.write stream " \"")
  (.write stream (escape value))
  (.write stream "\")"))

(defn write-annotations
  [^Writer stream anns]
  (doseq [ann anns]
    (emit ann stream)
    (.write stream \space)))

(defn write-doc-annotations
  [^Writer stream anns]
  (doseq [ann anns]
    (emit ann stream)
    (.write stream \n)))


(defn id
  [inst]
  (if (instance? rdf/IRI inst) inst (:id inst)))

(defn write-declarations
  "Write declarations for all document entities to the stream.
  These differ to other objects in that declarations are not a stored object, and instead
  reference stored objects."
  [^Writer stream classes oprops dprops aprops dts instances]
  (doseq [clazz classes]
    (.write stream "Declaration(Class(")
    (.write stream (str (id clazz)))
    (.write stream "))\n"))
  (doseq [prop oprops]
    (.write stream "Declaration(ObjectProperty(")
    (.write stream (str (id prop)))
    (.write stream "))\n"))
  (doseq [prop dprops]
    (.write stream "Declaration(DataProperty(")
    (.write stream (str (id prop)))
    (.write stream "))\n"))
  (doseq [prop aprops]
    (.write stream "Declaration(AnnotationProperty(")
    (.write stream (str (id prop)))
    (.write stream "))\n"))
  (doseq [dt dts]
    (.write stream "Declaration(Datatype(")
    (.write stream (str (id dt)))
    (.write stream "))\n"))
  (doseq [inst instances]
    (.write stream "NamedIndividual(")
    (.write stream (str (id inst)))
    (.write stream ")\n"))
  (.write stream "\n"))

(defn write-header
  [^Writer stream text]
  (.write stream "\n############################\n#   ")
  (.write stream text)
  (.write stream "\n############################\n\n"))


(def rdfs-label (rdf/curie :rdfs/label))

(defn write-annotation-assertions
  [^Writer stream id annotations]
  (doseq [[p v] annotations]
    (.write stream "AnnotationAssertion(")
    (.write stream (str p \space id \space \" (escape v) "\")\n"))))

(def property-labels {:data "Data" :obj "Object"})

(def relation-labels {:equiv "Equivalent" :disjoint "Disjoint"})

(defn write-sub-property
  "Writes a single SubObjectPropertyOf expression. These are only used when annotations are present."
  [^Writer stream label {:keys [annotations child parent]}]
  (.write stream label)
  (write-annotations stream annotations)
  (when-not (legal-inline-subprop? child)
    (throw (ex-info "Illegal complex property declared as a subproperty" {:child child :parents parent})))
  (emit child stream)
  (.write stream \space)
  (emit parent stream)
  (.write stream ")\n"))

(defn write-sub-object-property 
  [^Writer stream sub-property]
  (write-sub-property stream "SubObjectPropertyOf(" sub-property))

(defn write-sub-properties
  "Writes a collection of super-properties that may not be SubObjectPropertyOf expressions."
  [^Writer stream prop-type child super-properties]
  (let [label (str "Sub" (property-labels prop-type) "PropertyOf(")]
    (doseq [supprop super-properties]
      (if (prot/object-subproperty-expr? supprop)
        (write-sub-property stream label supprop)
        (do  ;; not an explicit statement: only the parent. Write it ourselves.
          (.write stream label)
          (when-not (legal-inline-subprop? child)
            (throw (ex-info "Illegal complex property declared as a subproperty" {:child child :parent supprop})))
          (emit child stream)
          (.write stream \space)
          (emit supprop stream)
          (.write stream ")\n"))))))

(defn write-inverse-property
  [^Writer stream {:keys [annotations prop]}]
  (.write stream "ObjectInverseOf(")
  (write-annotations stream annotations)
  (.write stream (str prop)) ;; must be an IRI
  (.write stream ")"))

(defn write-expr-properties
  "Writes a single xProperties expression. These are only used when annotations are present."
  [^Writer stream label {:keys [annotations id props]}]
  (.write stream label)
  (write-annotations stream annotations)
  (emit id stream)
  (doseq [prop props]
    (.write stream \space)
    (emit prop stream))
  (.write stream ")\n"))


(defn write-rel-properties
  [^Writer stream rel-type prop-type props]
  (write-expr-properties stream (str (relation-labels rel-type) (prop-type property-labels) "Properties(") props))

(defn write-doc-x-properties
  [^Writer stream rel-type prop-type prop others]
  (let [label (str (relation-labels rel-type) (property-labels prop-type) "Properties(")]
    (doseq [equiv others]
      (if (prot/object-property? equiv)
        (do  ;; an explicit list of object, write ourselves
          (.write stream label)
          (emit prop stream)
          (doseq [p equiv]
            (.write stream \space)
            (emit p stream))
          (.write stream ")\n"))
        ;; an relation statement. It knows how to write itself, including annotations
        (emit equiv stream)))))

(defn write-obj-prop
  "Writes a document-level ObjectProperty.
  This is a series of all the ObjectPropertyAxioms about a single object property."
  [^Writer stream oprop]
  (let [[id {:keys [annotations super-props equivs domain range disjoints fn? inverse-fn? transitive?
                      symmetric? asymmetric? reflexive? irreflexive?]}] oprop]
    (.write stream (str "\n# Object Property: " id))
    (if-let [label (get annotations rdfs-label)]
      (.write stream (str "(" label  ")\n\n"))
      (.write stream "\n\n"))
    ;; At the document level, annotations are not usually written inside the property definition
    (write-annotation-assertions stream id annotations)
    (write-sub-properties stream :obj id super-props)
    (write-doc-x-properties stream :equiv :obj id equivs)
    (write-doc-x-properties stream :disjoint :obj id disjoints)
    ))

(defn write-obj-props
  [^Writer stream oprops]
  (write-header stream "Object Properties")
  (doseq [prop oprops]
    (write-obj-prop stream prop)))

(defn write-property-chain
  [^Writer stream props]
  (when-let [[f & r] (seq props)]
    (.write stream "ObjectPropertyChain(")
    (emit f stream)
    (doseq [prop r]
      (.write stream \space)
      (emit prop stream))
    (.write stream \))))

(defn ->str
  [streamable]
  (let [s (StringWriter.)]
    (emit streamable s)
    (str s)))
