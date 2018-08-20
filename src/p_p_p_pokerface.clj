(ns p-p-p-pokerface)

(def char->rank {\T 10 \J 11 \Q 12 \K 13 \A 14})

(defn rank [card]
  (let [[rank _] card]
    (if (Character/isDigit rank)
      (Integer/valueOf (str rank))
      (char->rank rank))))

(defn suit [card]
  (let [[_ suit] card]
    (str suit)))

(defn rank-frequency [hand]
  (vals (frequencies (map rank hand))))

(defn max-rank-frequency [hand]
  (apply max (rank-frequency hand)))

(defn suit-frequency [hand]
  (apply max (vals (frequencies (map suit hand)))))

(defn pair? [hand]
  (= 2 (max-rank-frequency hand)))

(defn three-of-a-kind? [hand]
  (= 3 (max-rank-frequency hand)))

(defn four-of-a-kind? [hand]
  (= 4 (max-rank-frequency hand)))

(defn flush? [hand]
  (= 5 (suit-frequency hand)))

(defn full-house? [hand]
  (= (sort (rank-frequency hand)) [2 3]))

(defn two-pairs? [hand]
  (or (= (sort (rank-frequency hand)) [1 2 2])
      (= (sort (rank-frequency hand)) [1 4])))

(defn straight? [hand]
  (let [ranks (map rank hand)
        ace? (= 14 (apply max ranks))
        two? (= 2 (apply min ranks))
        low-ace-hand? (and ace? two?)
        seq-ranks? (fn [r]
                     (let [sorted-ranks (sort r)
                           lowest-rank (first sorted-ranks)]
                       (= sorted-ranks (range lowest-rank (+ lowest-rank 5)))))]
    (if low-ace-hand?
      (seq-ranks? (replace {14 1} ranks))
      (seq-ranks? ranks))))

(defn straight-flush? [hand]
  (and (flush? hand) (straight? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [matching-hand? (fn [checker] ((first checker) hand))
        checkers #{[high-card? 0]
                   [pair? 1]
                   [two-pairs? 2]
                   [three-of-a-kind? 3]
                   [straight? 4]
                   [flush? 5]
                   [full-house? 6]
                   [four-of-a-kind? 7]
                   [straight-flush? 8]}]
    (apply max (map second (filter matching-hand? checkers)))))
