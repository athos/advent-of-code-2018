(ns advent-of-code-2018.day14-test
  (:require [advent-of-code-2018.day14 :as day14]
            [clojure.test :refer [deftest is]]))

(deftest solve1-test
  (is (= "5158916779" (day14/solve1 9)))
  (is (= "0124515891" (day14/solve1 5)))
  (is (= "9251071085" (day14/solve1 18)))
  (is (= "5941429882" (day14/solve1 2018)))
  (is (= "9315164154" (day14/solve1 505961))))

(deftest ^:heavy solve2-test
  (is (= 9 (day14/solve2 [5 1 5 8 9])))
  (is (= 5 (day14/solve2 [0 1 2 4 5])))
  (is (= 18 (day14/solve2 [9 2 5 1 0])))
  (is (= 2018 (day14/solve2 [5 9 4 1 4])))
  (is (= 20231866 (day14/solve2 [5 0 5 9 6 1]))))
