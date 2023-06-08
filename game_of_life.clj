(ns game-of-life)

(def grid
  [[0 0 1]
   [0 1 1]
   [0 1 1]])

(def mods
  [[0  1]
   [0 -1]
   [1  0]
   [-1 0]
   [1  1]
   [-1 1]
   [1 -1]
   [-1 -1]])

(defn alive?
  [position grid]
  (= 1 (get-in grid position)))

(defn neighbors
  [[row col] grid]
  (->> mods
       (map (fn [[row-mod col-mod]]
              (get-in grid [(+ row row-mod) (+ col col-mod)])))
       (remove #(or (nil? %)
                    (zero? %)))
       count))

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

(defn print
  [grid]
  (for [row grid]
    (println row)))

(comment

 (-> grid
     step
     print)
 )
