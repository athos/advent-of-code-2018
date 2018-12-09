(ns advent-of-code-2018.day09)

(set! *unchecked-math* :warn-on-boxed)

(defprotocol INode
  (value [this])
  (left [this])
  (right [this])
  (set-left! [this node])
  (set-right! [this node]))

(deftype Node [value ^:volatile-mutable left ^:volatile-mutable right]
  INode
  (value [this] (.-value this))
  (left [this] (.-left this))
  (right [this] (.-right this))
  (set-left! [this node]
    (set! (.-left this) node))
  (set-right! [this node]
    (set! (.-right this) node))
  clojure.lang.ISeq
  (seq [this]
    (letfn [(rec [^Node node]
              (lazy-seq
               (when-not (identical? node this)
                 (cons (.-value node) (rec (.-right node))))))]
      (cons value (rec (.-right this))))))

(defn init-nodes []
  (let [zero (->Node 0 nil nil)
        one (->Node 1 nil nil)]
    (set-left! zero one)
    (set-right! zero one)
    (set-left! one zero)
    (set-right! one zero)
    one))

(defn add-value! [current-node val]
  (let [node (->Node val nil nil)
        r (right current-node)
        rr (right r)]
    (set-right! r node)
    (set-left! node r)
    (set-right! node rr)
    (set-left! rr node)
    node))

(defn remove-node! [current-node]
  (let [l (left current-node)
        r (right current-node)]
    (set-left! r l)
    (set-right! l r)
    r))

(defn num->player [players ^long i]
  (inc (rem (dec i) (count players))))

(defn step [{:keys [nodes players] :as state} ^long i]
  (if (> (rem i 23) 0)
    (let [next-node (add-value! nodes i)]
      (assoc state :nodes next-node))
    (let [player (num->player players i)
          node (->> (iterate left nodes)
                    (drop 7)
                    first)
          v (value node)]
      (assoc state
             :nodes (remove-node! node)
             :players (update players player + i v)))))

(defn solve1 [^long n-players ^long last]
  (let [nodes (init-nodes)
        players (zipmap (range 1 (inc n-players)) (repeat 0))]
    (loop [state {:nodes nodes :players players}, i 2]
      (if (<= i last)
        (recur (step state i) (inc i))
        (val (apply max-key val (:players state)))))))

(defn solve2 [n-players last]
  (solve1 n-players last))
