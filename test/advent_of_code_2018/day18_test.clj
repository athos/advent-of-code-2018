(ns advent-of-code-2018.day18-test
  (:require [advent-of-code-2018.day18 :as day18]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def lines
  [".#.#...|#."
   ".....#|##|"
   ".|..|...#."
   "..|#.....#"
   "#.#|||#|#|"
   "...#.||..."
   ".|....|..."
   "||...#|.#|"
   "|.||||..|."
   "...#.|..|."])

(deftest solve1-test
  (is (= 1147 (day18/solve1 lines)))
  (with-open [r (io/reader (io/resource "input18.txt"))]
    (is (= 483840 (day18/solve1 (line-seq r))))))

(deftest solve2-test
  (with-open [r (io/reader (io/resource "input18.txt"))]
    (is (= 219919 (day18/solve2 (line-seq r))))))
