(ns advent-of-code-2018.day09-test
  (:require [advent-of-code-2018.day09 :as day09]
            [clojure.test :refer [deftest is]]))

(deftest solve1-test
  (is (= 32 (day09/solve1 9 25)))
  (is (= 8317 (day09/solve1 10 1618)))
  (is (= 146373 (day09/solve1 13 7999)))
  (is (= 2764 (day09/solve1 17 1104)))
  (is (= 54718 (day09/solve1 21 6111)))
  (is (= 37305 (day09/solve1 30 5807)))
  (is (= 410375 (day09/solve1 439 71307))))

(deftest ^:heavy solve2-test
  (is (= 3314195047 (day09/solve2 439 7130700))))
