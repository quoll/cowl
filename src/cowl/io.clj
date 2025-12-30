(ns cowl.io
  {:doc "IO operations for COWL"
   :author "Paula Gearon"}
  (:require [clojure.string :as str]
            [cowl.protocols :refer [emit]])
  (:import [java.io Writer StringWriter]))

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

(defn as-string
  [streamable]
  (let [s (StringWriter.)]
    (emit streamable s)
    (str s)))
