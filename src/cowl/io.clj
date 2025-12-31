(ns cowl.io
  {:doc "IO operations for COWL"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [quoll.rdf :as rdf]
            [cowl.protocols :refer [emit]])
  (:import [java.io Writer StringWriter]))

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

(defn write-doc-annotations
  [^Writer stream anns]
  (doseq [[prop value] anns]
    (.write stream "Annotation(")
    (.write stream (str prop))
    (.write stream " \"")
    (.write stream (escape value))
    (.write stream "\")\n")))

(defn write-declarations
  [^Writer stream classes oprops dprops aprops dts]
  (doseq [clazz classes]
    (.write stream "Declaration(Class(")
    (.write stream (str clazz))
    (.write stream "))\n"))
  (doseq [prop oprops]
    (.write stream "Declaration(ObjectProperty(")
    (.write stream (str prop))
    (.write stream "))\n"))
  (doseq [prop dprops]
    (.write stream "Declaration(DataProperty(")
    (.write stream (str prop))
    (.write stream "))\n"))
  (doseq [prop aprops]
    (.write stream "Declaration(AnnotationProperty(")
    (.write stream (str prop))
    (.write stream "))\n"))
  (doseq [dt dts]
    (.write stream "Declaration(Datatype(")
    (.write stream (str dt))
    (.write stream "))\n"))
  (.write stream "\n"))

(defn write-header
  [^Writer stream text]
  (.write stream "\n############################\n#   ")
  (.write stream text)
  (.write stream "\n############################\n\n"))


(def rdfs-label (rdf/curie rdf/common-prefixes :rdfs/label))

(defn write-annotations
  [^Writer stream id annotations]
  (doseq [[p v] annotations]
    (.write stream "AnnotationAssertion(")
    (.write stream (str p \space id \space \" (escape v) "\")\n"))))

(defn write-oprops
  [^Writer stream oprops]
  (write-header stream "Object Properties")
  (doseq [[id {:keys [annotations]}] oprops]
    (.write stream (str "\n# Object Property: " id))
    (if-let [label (get annotations rdfs-label)]
      (.write stream (str "(" label  ")\n\n"))
      (.write stream "\n\n"))
    (write-annotations stream id annotations)))


(defn ->str
  [streamable]
  (let [s (StringWriter.)]
    (emit streamable s)
    (str s)))
