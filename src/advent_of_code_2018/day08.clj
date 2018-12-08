(ns advent-of-code-2018.day08)

(declare parse)

(defn parse-nodes [n input]
  (if (= n 0)
    [() input]
    (let [[node more] (parse input)
          [nodes more] (parse-nodes (dec n) more)]
      [(cons node nodes) more])))

(defn parse [[n-nodes n-metadata & more]]
  (let [[nodes more] (parse-nodes n-nodes more)
        [metadata more] (split-at n-metadata more)]
    [{:children (vec nodes), :metadata (vec metadata)}
     more]))

(defn with-parsed-node [s f]
  (with-in-str s
    (let [input (->> (repeatedly #(read *in* false nil))
                     (take-while some?))
          [node _] (parse input)]
      (f node))))

(defn sum-of-metadata [{:keys [children metadata]}]
  (apply + (apply + metadata)
         (map sum-of-metadata children)))

(defn solve1 [s]
  (with-parsed-node s sum-of-metadata))

(defn node-value [{:keys [children metadata]}]
  (if (empty? children)
    (apply + metadata)
    (let [n (count children)]
      (->> metadata
           (map (fn [i]
                  (if (< (dec i) n)
                    (node-value (nth children (dec i)))
                    0)))
           (apply +)))))

(defn solve2 [s]
  (with-parsed-node s node-value))
