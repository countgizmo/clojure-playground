(ns game-of-life
  (:require [clojure.test :refer [deftest is testing]]))

(def neighbors-directions
  [[0  1]
   [0 -1]
   [1  0]
   [-1 0]
   [1  1]
   [-1 1]
   [1 -1]
   [-1 -1]])

(defn neighbors
  [[row col] grid]
  (->> neighbors-directions
       (map (fn [[row-mod col-mod]]
              (get-in grid [(+ row row-mod) (+ col col-mod)])))
       (remove #(or (nil? %)
                    (zero? %)))
       count))

(deftest test-neighbors-detection
  (testing "find one neighbor"
    (is (= 1 (neighbors [0 2]
                        [[0 0 1]
                         [0 0 1]
                         [0 0 0]]))))
  (testing "find many neighbors"
    (is (= 4 (neighbors [1 1]
                        [[0 1 1]
                         [0 1 1]
                         [0 1 0]]))))
  (testing "find no neighbors"
    (is (= 0 (neighbors [0 0]
                        [[0 0 1]
                         [0 0 1]
                         [0 0 0]])))))

(defn underpopulated?
  [n]
  (< n 2))

(defn overpopulated?
  [n]
  (> n 3))

(defn regenerated?
  [n]
  (= 3 n))

(defn unchanged?
  [n]
  (or (= 2 n)
      (= 3 n)))

(defn transform-cell
  [position grid]
  (let [n (neighbors position grid)
        cell (get-in grid position)
        alive? (= 1 cell)]
    (cond
      (and alive?
           (or (underpopulated? n)
               (overpopulated? n)))
      0

      (and (not alive?)
           (regenerated? n))
      1

      (and alive?
           (unchanged? n))
      1

      :else
      0)))

(defn step
  [grid]
  (for [row (range (count grid))]
    (for [col (range (count (get grid row)))]
      (transform-cell [row col] grid))))

(defn print-grid
  [grid]
  (for [row grid]
    (println row)))

(def grid
  [[0 0 1]
   [0 1 1]
   [0 1 1]])

(comment

 (-> grid
     step
     print-grid)
 )
