(ns cowl.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [cowl.core :refer [owl annotate as-string]]))

(deftest annotation-test
  (testing "Basic annotation"
    (let [doc (-> (owl)
                  (annotate :rdfs/label "Test doc")
                  (annotate :rdfs/comment "testing \"data\" here"))]
      (is (= 2 (count (:annotations doc))))
      (is (= (str "Annotation(rdfs:label \"Test doc\")\n"
                  "Annotation(rdfs:comment \"testing \\\"data\\\" here\")\n")
             (as-string doc))))))

