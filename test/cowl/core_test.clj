(ns cowl.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [cowl.core :refer [owl annotate ->str]]))

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
      (is (= 2 (count (:annotations doc))))
      (is (= (str empty-id prefix-block
                  "Ontology(<#>\n<#0.0.1>\n\n"
                  "Annotation(rdfs:label \"Test doc\")\n"
                  "Annotation(rdfs:comment \"testing \\\"data\\\" here\")\n)\n")
             (->str doc))))))

