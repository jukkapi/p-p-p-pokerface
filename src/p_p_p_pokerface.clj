(ns p-p-p-pokerface)

(def rank-repl {\A 14 \K 13 \Q 12 \J 11 \T 10})

(defn rank [card]
  (let [[r _] card]
    (cond
      (Character/isDigit r)(Integer/valueOf (str r))
      :else (get rank-repl r))))

(defn suit [card]
  (let [[_ s] card]
    (str s)))

(defn n-of-a-kind? [hand n]
  (= n (apply max (vals (frequencies (map rank hand))))))

(defn pair? [hand]
  (n-of-a-kind? hand 2))

(defn three-of-a-kind? [hand]
  (n-of-a-kind? hand 3))

(defn four-of-a-kind? [hand]
  (n-of-a-kind? hand 4))

(defn flush? [hand]
  (= 5 (apply max (vals (frequencies (map suit hand))))))

(defn full-house? [hand]
  (= [2 3] (sort (vals (frequencies (map rank hand))))))

(defn two-pairs? [hand]
  (= [1 2 2] (sort (vals (frequencies (map rank hand))))))

(defn straight? [hand]
  (let [sorted-by-rank (fn[hnd] (sort (map rank hnd)))
        lowest-rank (fn[hnd] (first (sorted-by-rank hnd)))
        highest-rank (fn[hnd] (last (sorted-by-rank hnd)))
        contains-ace? (fn[hnd] (= (highest-rank hnd) 14))
        replace-ace-and-sort (fn[hnd] (sort (replace {14 1} (sorted-by-rank hnd))))]
    (or (= (sorted-by-rank hand) (range (lowest-rank hand) (+ (lowest-rank hand) 5)))
        (if (contains-ace? hand)
          (= (replace-ace-and-sort hand) (range 1 6))
          false))))

(defn hand [a-hand]
  a-hand)

(defn card [a-card]
  a-card)

(defn straight-flush? [hand]
  (and (straight? hand) (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers [[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]]
        check (fn [ch] (if((first ch) hand)
                         (second ch)
                         0))]
  (apply max (map check checkers))))


