(ns advent-of-code-2018.day02)

(defn solve1 [ids]
  (->> ids
       (keep (fn [id]
               (some->> (frequencies id)
                        vals
                        (keep #{2 3})
                        not-empty
                        set)))
       (reduce (fn [ret v]
                 (cond-> ret
                   (contains? v 2) (update 2 inc)
                   (contains? v 3) (update 3 inc)))
               {2 0, 3 0})
       ((fn [{two 2, three 3}] (* two three)))))

(defn diffs [s1 s2]
  (->> (map = s1 s2)
       (keep-indexed #(when (not %2) %1))))

(defn diff-pairs [ids n]
  (for [id1 ids
        id2 ids
        :when (and (not= id1 id2)
                   (= (count (diffs id1 id2)) n))]
    [id1 id2]))

(defn solve2 [ids]
  (let [[[id1 id2]] (diff-pairs ids 1)
        [i] (diffs id1 id2)
        len (count id1)]
    (cond-> (subs id1 0 i)
      (not= i (dec len))
      (str (subs id1 (inc i) len)))))
