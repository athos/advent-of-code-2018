(ns advent-of-code-2018.day05
  (:require [clojure.string :as str]))

(set! *unchecked-math* true)

(defn match? [^long c1 ^long c2]
  (= (Math/abs (- c1 c2)) 32))

(defn react [cs [l lc] [r rc :as entry]]
  (if (and lc rc (match? lc rc))
    (recur (dissoc cs l r)
           (first (rsubseq cs < l))
           (first (subseq cs > r)))
    [cs entry]))

(defn scan [cs [i c :as entry]]
  (if-let [[i' c' :as entry'] (and i (first (subseq cs > i)))]
    (if (match? c c')
      (let [[cs entry] (react cs entry entry')]
        (recur cs entry))
      (recur cs entry'))
    cs))

(defn str->map [s]
  (into (sorted-map)
        (map-indexed (fn [i c] [i (int c)]))
        (str/trimr s)))

(defn solve [cs]
  (count (scan cs (first (subseq cs >= 0)))))

(defn solve1 [s]
  (solve (str->map s)))

(defn remove-units [cs targets]
  (reduce-kv #(if (contains? targets %3) (dissoc %1 %2) %1) cs cs))

(defn solve2 [s]
  (let [cs (str->map s)]
    (->> (range (int \A) (inc (int \Z)))
         (pmap #(solve (remove-units cs #{% (+ % 32)})))
         (apply min))))
