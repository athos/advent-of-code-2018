(ns advent-of-code-2018.day01
  (:require [clojure.string :as str]))

(defn parse-freq-changes [s]
  (->> (str/split s #"\n")
       (map #(Long/parseLong %))))

(defn freq-history [cs]
  (reductions + 0 cs))

(defn solve1 [s]
  (->> (parse-freq-changes s)
       freq-history
       last))

(defn solve2 [s]
  (->> (parse-freq-changes s)
       cycle
       freq-history
       (reduce (fn [seen freq]
                 (if (seen freq)
                   (reduced freq)
                   (conj seen freq)))
               #{})))
