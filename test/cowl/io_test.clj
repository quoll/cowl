(ns cowl.io-test
  (:require [clojure.test :refer [deftest testing is]]
            [cowl.protocols :refer [annotate]]
            [cowl.impl :refer [owl]]
            [cowl.io :refer [start-doc end-doc write-prefixes escape write-doc-annotations
                             write-declarations write-header write-annotations write-obj-props
                             ->str]])
  (:import [java.io StringWriter]))

(defn st
  "Creates a stream to test an io function, calling the function with that stream and returning
  the output as a string."
  [f arg]
  (let [s (StringWriter.)]
    (f s arg)
    (str s)))

(def empty-id "Prefix(:=<#>)\n")

(def prefix-block "Prefix(rdf:=<http://www.w3.org/1999/02/22-rdf-syntax-ns#>)
Prefix(rdfs:=<http://www.w3.org/2000/01/rdf-schema#>)
Prefix(xsd:=<http://www.w3.org/2001/XMLSchema#>)
Prefix(owl:=<http://www.w3.org/2002/07/owl#>)
Prefix(skos:=<http://www.w3.org/2004/02/skos/core#>)
Prefix(dcterms:=<http://purl.org/dc/terms/>)\n\n")

(deftest annotation-test
  (testing "Basic annotation"
    (let [doc (-> (owl nil nil nil)
                  (annotate :rdfs/label "Test doc")
                  (annotate :rdfs/comment "testing \"data\" here"))]
      (is (= (str "Annotation(rdfs:label \"Test doc\")\n"
                  "Annotation(rdfs:comment \"testing \\\"data\\\" here\")\n")
             (st write-doc-annotations (:annotations doc)))))))


(deftest annotation-doc-test
  (testing "Test a complete doc containing only a basic annotation"
    (let [doc (-> (owl nil nil nil)
                  (annotate :rdfs/label "Test doc")
                  (annotate :rdfs/comment "testing \"data\" here"))]
      (println doc)
      (is (= (str empty-id prefix-block
                  "Ontology(<#>\n<#0.0.1>\n\n"
                  "Annotation(rdfs:label \"Test doc\")\n"
                  "Annotation(rdfs:comment \"testing \\\"data\\\" here\")\n"
                  "\n"
                  "\n############################\n#   Object Properties\n############################\n\n"
                  ")\n")
             (->str doc))))))
