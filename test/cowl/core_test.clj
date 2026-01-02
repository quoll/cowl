(ns cowl.core-test
  (:require [clojure.test :refer [deftest testing is]]
            [cowl.core :refer [owl annotate ->str]]))

(deftest annotation-test
  (testing "Basic annotation"
    (let [doc (-> (owl nil nil nil)
                  (annotate :rdfs/label "Test doc")
                  (annotate :rdfs/comment "testing \"data\" here"))]
      (is (= 2 (count (:annotations doc)))))))


