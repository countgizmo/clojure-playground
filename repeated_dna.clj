(ns repeated-dna
  (:require [clojure.test :refer [deftest is testing]]))

(defn repeated-dna-sequences
  [dna-seq]
  (loop [start-idx 0
         seen #{}
         result #{}]
    (if (> (+ start-idx 10) (count dna-seq))
      result
      (let [current-seq (subs dna-seq start-idx (+ start-idx 10))]
        (if (contains? seen current-seq)
          (recur (inc start-idx) seen (conj result current-seq))
          (recur (inc start-idx) (conj seen current-seq) result))))))


(deftest repeated-dna-sequences-test
  (testing "two sequences are repeated"
    (is (= #{"AAAAACCCCC" "CCCCCAAAAA" "AACCCCCAAA" "AAAACCCCCA" "AAACCCCCAA" "ACCCCCAAAA"}
           (repeated-dna-sequences "AAAAACCCCCAAAAACCCCCAAAAAGGGTTT")))))
