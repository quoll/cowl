(ns cowl.io.common
  {:doc "Common utilities for IO operations"
   :author "Paula Gearon"}
  (:import [java.io Writer]))

(defn write
  "Write a string to a Writer stream"
  [^Writer stream s]
  (.write stream s))
