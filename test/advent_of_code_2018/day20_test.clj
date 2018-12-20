(ns advent-of-code-2018.day20-test
  (:require [advent-of-code-2018.day20 :as day20]
            [clojure.java.io :as io]
            [clojure.test :refer [deftest is]]))

(def re1 "^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$")
(def re2 "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$")
(def re3 "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$")

(deftest solve1-test
  (is (= 18 (day20/solve1 re1)))
  (is (= 23 (day20/solve1 re2)))
  (is (= 31 (day20/solve1 re3)))
  (is (= 3046 (day20/solve1 (slurp (io/resource "input20.txt"))))))

(deftest solve2-test
  (is (= 8545 (day20/solve2 (slurp (io/resource "input20.txt"))))))
