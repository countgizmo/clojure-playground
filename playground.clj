(ns playground
  (:require [clojure.test :refer [is deftest testing]]))


(defn anagrams?
  [s1 s2]
  (= (sort s1)
     (sort s2)))

(deftest anagrams-detection
  (testing "word is an anagram"
    (anagrams? "hello" "loelh"))
  (testing "word is not an anagram"
    (anagrams? "hello" "loelh1"))
  (testing "word is a different word completely"
    (anagrams? "hello" "jello")))

(defn first-and-last-position
  [arr target]
  (reduce (fn [[start end :as acc] i]
            (if (= target (get arr i))
              (if (nil? start)
                [i end]
                [start i])
              acc))
          [-1 -1]
          (range (count arr))))

(defn first-pos-binary
  [arr needle]
  (if (= needle (first arr))
    0
    (loop [lo 0
           hi (dec (count arr))]
      (if (<= lo hi)
        (let [mid (+ lo (int (/ (- hi lo) 2)))
              candidate (get arr mid)
              pre-candidate (get arr (dec mid))]
          (cond
            (and (= needle candidate)
                 (> needle pre-candidate))
            mid

            (> needle candidate)
            (recur (inc mid) hi)

            :else
            (recur lo (dec mid))))
        -1))))

(defn last-pos-binary
  [arr needle]
  (if (= needle (last arr))
    (dec (count arr))
    (loop [lo 0
           hi (dec (count arr))]
      (if (<= lo hi)
        (let [mid (+ lo (int (/ (- hi lo) 2)))
              candidate (get arr mid)
              pre-candidate (get arr (inc mid))]
          (cond
            (and (= needle candidate)
                 (< needle pre-candidate))
            mid

            (< needle candidate)
            (recur lo mid)

            :else
            (recur (inc mid) hi)))
        -1))))

(defn first-last-binary
  [arr needle]
  (if (or (empty? arr)
          (> needle (last arr))
          (< needle (first arr)))
    [-1 -1]
    [(first-pos-binary arr needle)
     (last-pos-binary arr needle)]))

(defn binary-search
  [arr needle]
  (loop [lo 0
         hi (count arr)]
    (when (< lo hi)
      (let [mid (+ lo (int (/ (- hi lo) 2)))
            candidate (get arr mid)]
        (cond
          (= needle candidate) mid
          (< needle candidate) (recur lo mid)
          (> needle candidate) (recur (inc mid) hi))))))


(def tree-1-symmetrical
  {:value 37
   :left {:value 5
          :left {:value 2
                 :left {:value 9
                        :left {:value 3}
                        :right {:value 0}}
                 :right {:value 7
                         :right {:value 6}}}
          :right {:value 8
                  :left {:value 1}}}
   :right {:value 5
           :right {:value 2
                   :right {:value 9
                          :right {:value 3}
                          :left {:value 0}}
                   :left {:value 7
                          :left {:value 6}}}
           :left {:value 8
                  :right {:value 1}}}})

(def tree-1-not-symmetrical
  {:value 37
   :left {:value 5
          :left {:value 2
                 :left {:value 9
                        :left {:value 3}
                        :right {:value 0}}
                 :right {:value 7
                         :right {:value 6}}}
          :right {:value 8
                  :left {:value 1}}}
   :right {:value 5
           :right {:value 2
                   :right {:value 9
                          :right {:value 3}
                          :left {:value 0}}
                   :left {:value 7
                          :left {:value 6}}}
           :left {:value 9
                  :right {:value 1}}}})


(defn are-symmetrical?
  [tree1 tree2]
  (cond
    (and (nil? tree1)
         (nil? tree2))
    true

    (= (:value tree1)
       (:value tree2))
    (and (are-symmetrical? (:left tree1)
                           (:right tree2))
         (are-symmetrical? (:right tree1)
                           (:left tree2)))

    :else
    false))

(defn symmetrical?
  [tree]
  (are-symmetrical? (:left tree) (:right tree)))

(deftest symmetrical-tree
  (testing "the tree is symmetrical"
    (is (true? (symmetrical? tree-1-symmetrical))))
  (testing "the tree is not symmetrical"
    (is (false? (symmetrical? tree-1-not-symmetrical)))))

(comment
  

  (= [2 6]
     (first-and-last-position [2 4 5 5 5 5 5 7 9 9] 5))
  (= [-1 -1]
     (first-and-last-position [2 4 5 5 5 5 5 7 9 9] 8))

  (first-last-binary [2 4 5 5 5 5 5 7 9 9] 5)
  (first-last-binary [5 5 5 5 5 7 9 9] 5)
  (first-last-binary [5 5 5 5 5 7 9 9] 1)
  (first-last-binary [2 4 5 5 5 5 5] 5)
  (first-last-binary [2 4 5 5 5 5 5] 8)
  (first-last-binary [2 4 5 5 5 5 5] 1)
  (first-last-binary [] 1)


  (binary-search [1 2 3 4 5 6 7 8 9] 9)
  (binary-search [1 2 3 4 5 6 7 8 9] 11)

  )
