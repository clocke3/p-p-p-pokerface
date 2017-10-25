(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card]
   (if (Character/isDigit fst) (Integer/valueOf (str fst))
   ({\T 10, \J 11, \Q 12, \K 13, \A 14} fst))))
                                             
(defn suit [card]
(let [[_ sec] card]
  (str sec)))

(defn pair? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 2))


(defn three-of-a-kind? [hand]
  (= (apply max (vals (frequencies (map rank hand)))) 3))

(defn four-of-a-kind? [hand]
 (= (apply max (vals (frequencies (map rank hand)))) 4))

(defn flush? [hand]
 (= (apply max (vals (frequencies (map suit hand)))) 5))

(defn full-house? [hand]
  (= (sort (vals (frequencies(map rank hand)))) [2 3]))

(defn two-pairs? [hand]
 (= (sort (vals (frequencies(map rank hand)))) [1 2 2]))

(defn straight? [hand]
  (let [ace1(sort(replace {14 1} (map rank  hand)))
       ace2 (sort(map rank hand))]
   (cond
     (= ace1 (range (first ace1)(+ (first ace1) 5))) true
     (= ace2 (range (first ace2) (+ (first ace2)5))) true
     :else false)))

(defn straight-flush? [hand]
  (and (flush? hand)(straight? hand)))


(defn value [hand]
  (cond
     (straight-flush? hand) 8
     (four-of-a-kind? hand) 7
     (full-house? hand) 6
     (flush? hand) 5
     (straight? hand) 4
     (three-of-a-kind? hand) 3
     (two-pairs? hand) 2
     (pair? hand) 1
     :else 0
    ))
        
  ;need help 
  