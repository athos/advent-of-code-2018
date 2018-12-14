(ns advent-of-code-2018.day11-test
  (:require [advent-of-code-2018.day11 :as day11]
            [clojure.test :refer [deftest is]]))

(deftest power-level-test
  (is (= -5 (day11/power-level 57 79 122)))
  (is (= 0 (day11/power-level 39 196 217)))
  (is (= 4 (day11/power-level 71 153 101))))

(deftest ^:heavy solve1-test
  (is (= "33,45" (day11/solve1 18)))
  (is (= "21,61" (day11/solve1 42)))
  (is (= "21,53" (day11/solve1 6548))))

(deftest ^:heavy solve2-test
  (is (= "90,269,16" (day11/solve2 18)))
  (is (= "232,251,12" (day11/solve2 42)))
  (is (= "233,250,12" (day11/solve2 6548))))
