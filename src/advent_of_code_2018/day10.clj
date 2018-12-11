(ns advent-of-code-2018.day10
  (:require [clojure.string :as str]))

(defn parse-line [line]
  (let [[_ x y dx dy] (re-matches #"position=\<\s*(-?\d+),\s+(-?\d+)\> velocity=\<\s*(-?\d+),\s+(-?\d+)\>" line)]
    {:pos [(Long/parseLong y) (Long/parseLong x)]
     :vel [(Long/parseLong dy) (Long/parseLong dx)]}))

(defn points->map [ps]
  (reduce #(update %1 (:pos %2) (fnil conj []) (:vel %2)) {} ps))

(defn stringify-map [m]
  (let [[min-y max-y] (apply (juxt min max) (map (comp first key) m))
        [min-x max-x] (apply (juxt min max) (map (comp second key) m))]
    (->> (for [y (range min-y (inc max-y))]
           (->> (for [x (range min-x (inc max-x))]
                  (if (get m [y x]) \# \.))
                str/join))
         (str/join \newline))))

(defn step [m]
  (reduce-kv (fn [m [y x :as pos] vels]
               (reduce (fn [m [dy dx :as vel]]
                         (update m [(+ y dy) (+ x dx)] (fnil conj []) vel))
                       m vels))
             {} m))

(defn map-height [m]
  (let [[min-y max-y] (apply (juxt min max) (map first (keys m)))]
    (- max-y (dec min-y))))

(defn solve [m]
  (transduce (map-indexed (fn [i m] [(inc i) m (map-height m)]))
             (completing
              (fn [[i m h] [i' m' h' :as x]]
                (if (>= h h')
                  x
                  (reduced [i m]))))
             [0 m (map-height m)]
             (iterate step (step m))))

(defn solve1 [lines]
  (->> (solve (points->map (map parse-line lines)))
       second
       stringify-map))

(defn solve2 [lines]
  (first (solve (points->map (map parse-line lines)))))
