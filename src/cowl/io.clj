(ns cowl.io
  {:doc "IO operations for COWL"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
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


(defn ->str
  [streamable]
  (let [s (StringWriter.)]
    (emit streamable s)
    (str s)))
