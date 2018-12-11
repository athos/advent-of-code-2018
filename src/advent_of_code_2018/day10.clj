(ns advent-of-code-2018.day10)

(defn parse-line [line]
  (let [[_ x y dx dy] (re-matches #"position=\<\s*(-?\d+),\s+(-?\d+)\> velocity=\<\s*(-?\d+),\s+(-?\d+)\>" line)]
    {:pos [(Long/parseLong y) (Long/parseLong x)]
     :vel [(Long/parseLong dy) (Long/parseLong dx)]}))

(defn points->map [ps]
  (reduce #(update %1 (:pos %2) (fnil conj []) (:vel %2)) {} ps))

(defn print-map [m]
  (let [[min-y max-y] (apply (juxt min max) (map (comp first key) m))
        [min-x max-x] (apply (juxt min max) (map (comp second key) m))
        width (- max-x (dec min-x))]
    (doseq [y (range min-y (inc max-y))]
      (doseq [x (range min-x (inc max-x))]
        (print (if (get m [y x]) \# \.)))
      (newline))))

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
  (->> (iterate step m)
       (partition 2 1)
       (split-with (fn [[m1 m2]] (> (map-height m1) (map-height m2))))))

(defn solve1 [lines]
  (->> (solve (points->map (map parse-line lines)))
       second
       ffirst
       print-map))

(defn solve2 [lines]
  (->> (solve (points->map (map parse-line lines)))
       first
       count))
