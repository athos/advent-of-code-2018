(ns advent-of-code-2018.day12-test
  (:require [advent-of-code-2018.day12 :as day12]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def sample-init "#..#.#..##......###...###")

(def sample-lines
  ["...## => #"
   "..#.. => #"
   ".#... => #"
   ".#.#. => #"
   ".#.## => #"
   ".##.. => #"
   ".#### => #"
   "#.#.# => #"
   "#.### => #"
   "##.#. => #"
   "##.## => #"
   "###.. => #"
   "###.# => #"
   "####. => #"])

(def init
  "#..####.##..#.##.#..#.....##..#.###.#..###....##.##.#.#....#.##.####.#..##.###.#.......#............")

(deftest solve1-test
  (is (= 325 (day12/solve1 20 sample-init sample-lines)))
  (with-open [r (io/reader (io/resource "input12.txt"))]
    (is (= 1696 (day12/solve1 20 init (line-seq r))))))

(deftest solve2-test
  (with-open [r (io/reader (io/resource "input12.txt"))]
    (is (= 1799999999458 (day12/solve2 50000000000 init (line-seq r))))))
