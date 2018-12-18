(ns advent-of-code-2018.day18)

(defn lines->map [lines]
  (->> (for [[y line] (map-indexed vector lines)
             [x c] (map-indexed vector line)]
         [[y x] c])
       (into {})))

(defn count-adjacent-acres [m c [y x]]
  (reduce (fn [n [dy dx]]
            (cond-> n
              (= c (get m [(+ y dy) (+ x dx)]))
              inc))
          0
          [[0 1] [-1 1] [-1 0] [-1 -1] [0 -1] [1 -1] [1 0] [1 1]]))

(defn step [{m :map :keys [height width] :as state}]
  (reduce (fn [state y]
            (reduce (fn [state x]
                      (let [pos [y x]
                            c (get m pos)]
                        (->> (case c
                               \. (if (>= (count-adjacent-acres m \| pos) 3) \| c)
                               \| (if (>= (count-adjacent-acres m \# pos) 3) \# c)
                               \# (if (and (>= (count-adjacent-acres m \# pos) 1)
                                           (>= (count-adjacent-acres m \| pos) 1))
                                    c \.))
                             (assoc-in state [:map pos]))))
                      state
                      (range 0 width)))
          state
          (range 0 height)))

(defn map-size [m]
  (let [height (inc (apply max (map first (keys m))))
        width (inc (apply max (map second (keys m))))]
    [height width]))

(defn resource-count [{m :map}]
  (reduce (fn [acc [_ c]]
            (cond-> acc
              (= c \|) (update 0 inc)
              (= c \#) (update 1 inc)))
          [0 0]
          m))

(defn resource-value [m]
  (apply * (resource-count m)))

(defn solve1 [lines]
  (let [m (lines->map lines)
        [height width] (map-size m)]
    (-> (iterate step {:map m :height height :width width})
        (nth 10)
        resource-value)))

(defn maybe-cycle? [log-entry]
  (and (>= (count log-entry) 10)
       (->> (take 5 (rseq log-entry))
            (partition 2 1)
            (map (fn [[i j]] (- i j)))
            (apply =))))

(defn solve2 [lines]
  (let [m (lines->map lines)
        [height width] (map-size m)]
    (loop [i 0, state {:map m :height height :width width}, logs {}]
      (let [k (resource-count state)
            entry (get logs k)]
        (if (maybe-cycle? entry)
          (let [[i j] (rseq entry)
                t (+ (rem (- 1000000000 i) (- i j)) i)
                [w l] (ffirst (filter #((val %) t) logs))]
            (* w l))
          (recur (inc i) (step state)
                 (update logs k (fnil conj (sorted-set)) i)))))))
