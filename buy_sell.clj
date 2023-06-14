(ns buy-sell
  (:require [clojure.test :refer [deftest testing is]]))

(defn max-profit
  [days]
  (loop [buy-idx 0
         sell-idx 1
         max-profit 0]
    (let [buy (get days buy-idx)
          sell (get days sell-idx)]
      (cond
        (>= sell-idx (count days)) max-profit
        (> sell buy) (recur buy-idx (inc sell-idx) (max (- sell buy)
                                                        max-profit))
        :else (recur (inc buy-idx) (inc sell-idx) max-profit)))))

(deftest max-profit-test
  (testing "there is a sell day bigger than a buy day"
    (is (= 5 (max-profit [7 1 5 3 6 4]))))
  (testing "there is no profit"
    (is (= 0 (max-profit [5 5 5]))))
  (testing "no days - no profit"
    (is (= 0 (max-profit []))))
  (testing "it is just getting worse and worse"
    (is (= 0 (max-profit [7 6 5 4 3 2 1])))))
