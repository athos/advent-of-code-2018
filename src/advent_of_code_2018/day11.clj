(ns advent-of-code-2018.day11)

(defn ^:dynamic power-level [serial y x]
  (let [rack-id (+ (long x) 10)]
    (-> rack-id
        (* (long y))
        (+ (long serial))
        (* rack-id)
        (rem 1000)
        (quot 100)
        (- 5))))

(defn total-power3 [^long serial ^long y ^long x]
  (->> (for [i (range 3)
             j (range 3)]
         (power-level serial (+ y (long i)) (+ x (long j))))
       (apply +)))

(defn ^:dynamic total-power [serial size y x]
  (let [serial (long serial), size (long size), y (long y), x (long x)]
    (if (= size 3)
      (total-power3 serial y x)
      (->> (concat (for [y (range y (+ y size))] (power-level serial y (+ x (dec size))))
                   (for [x (range x (+ x size))] (power-level serial (+ y (dec size)) x)))
           (apply + (total-power serial (dec size) y x))))))

(defn solve1 [^long serial]
  (->> (for [y (range 1 301)
             x (range 1 301)]
         [(total-power3 serial y x) y x])
       (apply max-key first)
       ((fn [[_ y x]] (str x \, y)))))

(defn solve2 [^long serial]
  (binding [power-level (memoize power-level)
            total-power (memoize total-power)]
    (->> (range 3 20)
         (pmap (fn [^long size]
                 (->> (for [y (range 1 (- 300 size))
                            x (range 1 (- 300 size))]
                        [(total-power serial size y x) size y x])
                      (apply max-key first))))
         (apply max-key first)
         ((fn [[_ size y x]] (str x \, y \, size))))))
