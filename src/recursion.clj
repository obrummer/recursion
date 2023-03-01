(ns recursion)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(product [])        ;=> 1  ; special case
(product [1 2 3])   ;=> 6
(product [1 2 3 4]) ;=> 24
(product [0 1 2])   ;=> 0
(product #{2 3 4})  ;=> 24 ; works for sets too!

(defn singleton? [coll]
  (if (empty? coll)
    false
    (empty? (rest coll))))

(singleton? [1])     ;=> true
(singleton? #{2})    ;=> true
(singleton? [])      ;=> false
(singleton? [1 2 3]) ;=> false

(defn my-last [coll]
  (first (reverse coll)))

(my-last [])      ;=> nil
(my-last [1 2 3]) ;=> 3
(my-last [2 5])   ;=> 5

(defn max-element [a-seq]
  (if (empty? a-seq)
    nil
    (apply max a-seq)))

(max-element [2 4 1 4]) ;=> 4
(max-element [2])       ;=> 2
(max-element [])        ;=> nil

(defn seq-max [seq-1 seq-2]
  (let [first-count (count seq-1)
        second-count (count seq-2)]
    (if (> first-count second-count) 
      seq-1
      seq-2)))

(seq-max [1] [1 2])   ;=> [1 2]
(seq-max [1 2] [3 4]) ;=> [3 4]

(defn longest-sequence [a-seq]
   (if (empty? a-seq)
    nil
    (seq-max (first a-seq)
       (longest-sequence (rest a-seq)))))

(longest-sequence [[1 2] [] [1 2 3]]) ;=> [1 2 3]
(longest-sequence [[1 2]])            ;=> [1 2]
(longest-sequence [])                 ;=> nil

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [names (cons (cond (pred? (first a-seq)) (first a-seq))
          (my-filter pred? (rest a-seq)))]
      (remove nil? names))))

(my-filter odd? [1 2 3 4]) ;=> (1 3)
(my-filter (fn [x] (> x 9000)) [12 49 90 9001]) ;=> (9001)
(my-filter even? [1 3 5 7]) ;=> ()

(defn sequence-contains? [elem a-seq]
  (cond
   (empty? a-seq)
     false                      ; the empty sequence contains only numbers
   (== elem (first a-seq))
     true
   (not (== elem (first a-seq)))
     (sequence-contains? elem (rest a-seq)) ; we got a number, let's check the rest
   :else
     false))

(sequence-contains? 3 [1 2 3]) ;=> true
(sequence-contains? 3 [4 7 9]) ;=> false
(sequence-contains? :pony [])  ;=> false

(defn my-take-while [pred? a-seq]
 (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (cons (first a-seq) (my-take-while pred? (rest a-seq)))
    :else
      '()))

(my-take-while odd? [1 2 3 4])  ;=> (1)
(my-take-while odd? [1 3 4 5])  ;=> (1 3)
(my-take-while even? [1 3 4 5]) ;=> ()
(my-take-while odd? [])         ;=> ()

(defn my-drop-while [pred? a-seq]
 (cond
    (empty? a-seq)
      '()
    (pred? (first a-seq))
      (my-drop-while pred? (rest a-seq))
    :else
      a-seq))
      


(my-drop-while odd? [1 2 3 4])  ;=> (2 3 4)
(my-drop-while odd? [1 3 4 5])  ;=> (4 5)
(my-drop-while even? [1 3 4 5]) ;=> (1 3 4 5)
(my-drop-while odd? [])         ;=> ()

(defn first-in [val seq-1 seq-2]
  (cond
    (and (empty? seq-1) (empty? seq-2)) 0
    (= (first seq-1) val) 1
    (= (first seq-2) val) 2
    :else (first-in val (rest seq-1) (rest seq-2))))


(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq)) true
    (= a-seq b-seq) true
    :else false))

(defn my-map [f seq-1 seq-2]
  (cond
    (or (empty? seq-1) (empty? seq-2)) '()
    (f (first seq-1) (first seq-2))
       (cons (f (first seq-1) (first seq-2)) (my-map f (rest seq-1) (rest seq-2)))
    :else false))

(my-map + [1 2 3] [4 4 4])   ;=> (5 6 7)
(my-map + [1 2 3 4] [0 0 0]) ;=> (1 2 3)
(my-map + [1 2 3] [])        ;=> ()

(defn power [n k]
   (if (zero? k)
    1
    (* n (power n (dec k)) )))

(power 2 2)  ;=> 4
(power 5 3)  ;=> 125
(power 7 0)  ;=> 1
(power 0 10) ;=> 0
(power 3 3) ;

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    (== n 2) 1
    :else (+ (fib (- n 1)) (fib (- n 2)))))

(fib 0) ;=> 0
(fib 1) ;=> 1
(fib 2) ;=> 1
(fib 3) ;=> 2
(fib 4) ;=> 3
(fib 5) ;=> 5
(fib 6) ;=> 8
(fib 10) ;=> 55

(defn my-repeat [how-many-times what-to-repeat]
  (if (>= 0 how-many-times) 
    '() 
    (cons what-to-repeat (my-repeat (dec how-many-times) what-to-repeat))))

(my-repeat 2 :a)    ;=> (:a :a)
(my-repeat 3 "lol") ;=> ("lol" "lol" "lol")
(my-repeat -1 :a)   ;=> ()

(defn my-range [up-to]
  (if (>= 0 up-to) 
    '() 
    (cons (- up-to 1) (my-range (dec up-to)))))

(my-range 0)  ;=> ()
(my-range 1)  ;=> (0)
(my-range 2)  ;=> (1 0)
(my-range 3)  ;=> (2 1 0)

(defn tails [a-seq]
  (if (empty? a-seq) 
    '(()) 
    (cons (sequence a-seq) (tails (rest a-seq)))
    ))

(defn inits [a-seq]
  (if (empty? a-seq) 
    '(()) 
    (cons (sequence a-seq) (inits (pop a-seq)))
    ))

(tails [1 2 3 4]) ;=> ((1 2 3 4) (2 3 4) (3 4) (4) ())
(inits [1 2 3 4]) ;=> (() (1) (1 2) (1 2 3) (1 2 3 4))

(defn rotations [a-seq]
   (let [t (tails a-seq)
        i (reverse (inits a-seq))]
    (cond (empty? a-seq) `(())
          :else (rest (map concat t i)))))


(rotations [])        ;=> (())
(rotations [1 2 3])   ;=> ((1 2 3) (2 3 1) (3 1 2))
(rotations [:a :b])   ;=> ((:a :b) (:b :a))
(rotations [:a :b])   ;=> ((:b :a) (:a :b))
(rotations [1 5 9 2]) ;=> ((1 5 9 2) (2 1 5 9) (9 2 1 5) (5 9 2 1))
(count (rotations [6 5 8 9 2])) ;=> 5

(defn my-frequencies-helper [freqs a-seq]
  (if (empty? a-seq)
    freqs
    true
    ;; (let [new-count (if (= elem (first coll))
    ;;                   (inc n)
    ;;                   n)]
    ;;   (count-elem-helper new-count
    ;;                      elem
    ;;                      (rest coll)))))
  ))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(my-frequencies []) ;=> {}
(my-frequencies [:a "moi" :a "moi" "moi" :a 1]) ;=> {:a 3, "moi" 3, 1 1}

(defn un-frequencies [a-map]
  [:-])

(defn my-take [n coll]
  (if (or (== n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll) ))))

(my-take 2 [1 2 3 4]) ;=> (1 2)
(my-take 4 [:a :b]);=> (:a :b)
(my-take 2 [])
(defn my-drop [n coll]
  (if (or (zero? n) (empty? coll))
    coll
    (my-drop (dec n) (rest coll) )))

(my-drop 2 [1 2 3 4]) ;=> (3 4)
(my-drop 4 [:a :b])   ;=> ()

(defn halve [a-seq]
  (let [amount (count a-seq)
        amount-halve (int (/ amount 2))] 
    [(my-take amount-halve a-seq) (my-drop amount-halve a-seq)]
    ))

(halve [1 2 3 4])   ;=> [(1 2) (3 4)]
(halve [1 2 3 4 5]) ;=> [(1 2) (3 4 5)]
(halve [1])         ;=> [() (1)]

(defn seq-merge-helper [coll]
  (if (empty? coll)
    coll
    (cons (first coll))
  ))

(defn seq-merge [a-seq b-seq]
  (let [merged (concat a-seq b-seq)]
    merged))

(seq-merge [4] [1 2 6 7])        ;=> (1 2 4 6 7)
(seq-merge [1 5 7 9] [2 2 8 10]) ;=> (1 2 2 5 7 8 9 10)

(defn merge-sort [a-seq]
  [:-])

(defn split-into-monotonics [a-seq]
  [:-])

(defn permutations [a-set]
  [:-])

(defn powerset [a-set]
  [:-])

