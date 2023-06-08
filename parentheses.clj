(ns parentheses
  (:require [clojure.string :as string]
            [clojure.test :refer [deftest is testing]]))

(defn- generate-rec
  [n diff current-comb all-combs]
  (cond 
    (< diff 0)
    all-combs

    (and (= n 0) (= diff 0))
    (conj all-combs (string/join "" current-comb))

    :else
    (reduce (fn [acc c]
              (if (< n 0)
                acc
                (generate-rec (dec n) (:diff c) (:current-comb c) acc)))
            all-combs
            [{:diff (inc diff)
              :current-comb (conj current-comb "[")}
             {:diff (dec diff)
              :current-comb (conj current-comb "]")}])))

(defn generate
  [n]
  (generate-rec (* n 2) 0 [] []))

(deftest generate-parentheses
  (testing "one set"
    (is (= #{"[]"} (set (generate 1)))))
  (testing "three sets"
    (is (= #{"[[[]]]" "[[][]]" "[[]][]" "[][[]]" "[][][]"} (set (generate 3))))))
