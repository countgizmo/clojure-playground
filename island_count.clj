(ns island-count
  (:require [clojure.test :refer [deftest testing is]]))

(defn land?
  [grid position]
  (= (get-in grid position)
     1))

(def neighbors-mods
  [[ 1   0]
   [-1   0]
   [ 0   1]
   [ 0  -1]])

(defn neighbors
  [[row col]]
  (map (fn [[row-mod col-mod]]
         [(+ row row-mod) (+ col col-mod)])
       neighbors-mods))

(defn new-land?
  [grid [row col :as position] visited]
  (and (not (contains? visited position))
       (land? grid position)
       (and (< row (count grid)) (>= row 0))
       (and (< col (count (first grid))) (>= col 0))))

(defn explore
  [grid position visited]
  (reduce (fn [acc n]
            (if (new-land? grid n acc)
              (explore grid n (conj acc n))
              acc))
          (conj visited position)
          (neighbors position)))

(deftest explore-lands
  (testing "find adjacent lands"
    (is (= #{[0 0] [0 1] [0 2] [1 1] [1 2]}
           (explore [[1 1 1 0]
                     [0 1 1 0]
                     [0 0 0 0]]
                    [0 0]
                    #{}))))
  (testing "find no adjacent lands"
    (is (= #{[1 1]}
           (explore [[1 0 0 0]
                     [0 1 0 0]
                     [0 0 0 0]]
                    [1 1]
                    #{})))))

(defn count-islands
  [grid]
  (let [positions (vec (for [row-idx (range (count grid))
                             col-idx (range (count (get grid row-idx)))]
                         [row-idx col-idx]))]
    (loop [position-id 0
           result 0
           visited #{}]
      (if-let [position (get positions position-id)]
        (if (new-land? grid position visited)
          (recur (inc position-id)
                 (inc result)
                 (explore grid position visited))
          (recur (inc position-id)
                 result
                 visited))
        result))))


(deftest counting-islands
  (testing "several islands found"
    (is (= 2 (count-islands [[0 1 0 0]
                             [0 1 0 1]
                             [0 1 0 0]]))))
  (testing "no islands found"
    (is (zero? (count-islands [[0 0 0 0]
                               [0 0 0 0]
                               [0 0 0 0]])))))
