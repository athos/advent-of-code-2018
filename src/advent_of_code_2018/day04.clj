(ns advent-of-code-2018.day04
  (:import [java.time LocalDateTime ZoneOffset]))

(defn parse-record [line]
  (let [[_ year month day hour minute action]
        (re-matches #"^\[(\d+)-(\d\d)-(\d\d) (\d\d):(\d\d)\] (.+)$" line)
        [year month day hour minute] (->> [year month day hour minute]
                                          (map #(Long/parseLong ^String %)))]
    {:time (-> (LocalDateTime/of year month day hour minute 00)
               (.toEpochSecond ZoneOffset/UTC)
               (/ 60))
     :action (condp re-matches action
               #"Guard #(\d+) begins shift"
               :>> (fn [[_ id]] [:shift (Long/parseLong id)])
               #"wakes up" [:wakeup]
               #"falls asleep" [:sleep])}))

(defn step [state {[op arg] :action :keys [time]}]
  (case op
    :shift (assoc state :current arg)
    :sleep (assoc state :start time)
    :wakeup (let [{id :current :keys [start]} state]
              (update-in state [:guards id] (fnil conj []) [start time]))))

(defn most-frequent-minute [intervals]
  (->> intervals
       (reduce (fn [ret [s e]]
                 (->> (map #(mod % 60) (range s e))
                      frequencies
                      (merge-with + ret)))
               {})
       (apply max-key val)))

(defn solve [key-fn lines]
  (let [records (sort-by :time (map parse-record lines))
        {:keys [guards]} (reduce step {} records)
        [id intervals] (apply max-key key-fn guards)]
    (* id (first (most-frequent-minute intervals)))))

(defn solve1 [lines]
  (solve #(apply + (map (fn [[s e]] (- e s)) (val %))) lines))

(defn solve2 [lines]
  (solve #(second (most-frequent-minute (val %))) lines))
