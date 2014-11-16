(ns recursion)

(use 'clojure.set)

(defn product [coll]
  (if (empty? coll)
    1
    (* (first coll)
       (product (rest coll)))))

(defn singleton? [coll]
  (and (not (empty? coll))
       (empty? (rest coll))))

(defn my-last [coll]
  (cond
    (empty? coll) nil
    (singleton? coll) (first coll)
    :else (my-last (rest coll))))

(defn max-element [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (max (first a-seq) (max-element (rest a-seq)))))

(defn seq-max [seq-1 seq-2]
  (if (> (count seq-1) (count seq-2))
    seq-1
    seq-2))

(defn longest-sequence [a-seq]
  (cond
    (empty? a-seq) nil
    (singleton? a-seq) (first a-seq)
    :else (seq-max (first a-seq) (longest-sequence (rest a-seq)))))

(defn my-filter [pred? a-seq]
  (if (empty? a-seq)
    a-seq
    (let [f (first a-seq)]
      (if (pred? f)
        (cons f (my-filter pred? (rest a-seq)))
        (my-filter pred? (rest a-seq))))))

(defn sequence-contains? [elem a-seq]
  (cond
    (empty? a-seq) false
    (= elem (first a-seq)) true
    :else (sequence-contains? elem (rest a-seq))))

(defn my-take-while [pred? a-seq]
  (let [f (first a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? f) (cons f (my-take-while pred? (rest a-seq)))
      :else '())))

(defn my-drop-while [pred? a-seq]
  (let [f (first a-seq)
        r (rest a-seq)]
    (cond
      (empty? a-seq) a-seq
      (pred? f) (my-drop-while pred? r)
      :else a-seq)))

(defn seq= [a-seq b-seq]
  (cond
    (and (empty? a-seq) (empty? b-seq))
      true
    (or (empty? a-seq) (empty? b-seq))
      false
    (= (first a-seq) (first b-seq))
      (seq= (rest a-seq) (rest b-seq))
    :else
      false))

(defn my-map [f seq-1 seq-2]
  (if (or (empty? seq-1) (empty? seq-2))
    '()
    (cons (f (first seq-1) (first seq-2))
          (my-map f (rest seq-1) (rest seq-2)))))

(defn power [n k]
  (if (== k 0)
    1
    (* n (power n (dec k)))))

(defn fib [n]
  (cond
    (== n 0) 0
    (== n 1) 1
    :else (+ (fib (dec n))
             (fib (- n 2)))))

(defn my-repeat [how-many-times what-to-repeat]
  (if (< how-many-times 1)
    '()
    (cons what-to-repeat
          (my-repeat (dec how-many-times) what-to-repeat))))

(defn my-range [up-to]
  (if (< up-to 1)
    '()
    (cons (dec up-to) (my-range (dec up-to)))))

(defn tails [a-seq]
  (if (empty? a-seq)
    '(())
    (cons (seq a-seq) (tails (rest a-seq)))))

(defn inits [a-seq]
  (map reverse (tails (reverse a-seq))))

(defn rotations [a-seq]
  (distinct 
    (map concat
         (tails a-seq)
         (reverse (inits a-seq)))))

(defn my-frequencies-helper [freqs a-seq]
  (let [f (first a-seq)
        c (get freqs f)]
    (cond
      (empty? a-seq) freqs
      (nil? c) (my-frequencies-helper
                 (assoc freqs f 1)
                 (rest a-seq))
      :else (my-frequencies-helper
              (assoc freqs f (inc c))
              (rest a-seq)))))

(defn my-frequencies [a-seq]
  (my-frequencies-helper {} a-seq))

(defn un-frequencies [a-map]
  (if (empty? a-map)
    '()
    (let [[k v] (first a-map)]
      (concat (repeat v k)
              (un-frequencies (rest a-map))))))

(defn my-take [n coll]
  (if (or (<= n 0) (empty? coll))
    '()
    (cons (first coll) (my-take (dec n) (rest coll)))))

(defn my-drop [n coll]
  (cond
    (empty? coll) '()
    (<= n 0) (seq coll)
    :else (my-drop (dec n) (rest coll))))

(defn halve [a-seq]
  (let [h (int (/ (count a-seq) 2))]
    [(my-take h a-seq) (my-drop h a-seq)]))

(defn seq-merge [a-seq b-seq]
  (cond
    (empty? a-seq) b-seq
    (empty? b-seq) a-seq
    :else (let [af (first a-seq)
                bf (first b-seq)]
            (if (< af bf)
              (cons af (seq-merge (rest a-seq) b-seq))
              (cons bf (seq-merge a-seq (rest b-seq)))))))

(defn merge-sort [a-seq]
  (if (or (empty? a-seq) (singleton? a-seq))
    a-seq
    (let [[a b] (halve a-seq)]
      (seq-merge (merge-sort a)
                 (merge-sort b)))))

(defn rinits [a-seq]
  (reverse (inits a-seq)))

(defn next-monotonic [inits-seq]
  (if (empty? inits-seq)
    nil
    (let [f (first inits-seq)]
      (cond
        (empty? f) 
          (next-monotonic (rest inits-seq))
        (or (apply <= f) (apply >= f))
          (let [r (next-monotonic (rest inits-seq))]
            (if (nil? r) f r))
        :else nil))))

(defn split-into-monotonics [a-seq]
  (if (empty? a-seq) '()
    (let [ri (rinits a-seq)
          next-mon (next-monotonic ri)]
      (cons next-mon
            (split-into-monotonics
              (drop (count next-mon) a-seq))))))

(defn permutations [a-seq]
  (let [permutations-inner
        (fn [a-seq]
          (cond
            (empty? a-seq) '(())
            (singleton? a-seq) (list a-seq)
            :else (map (fn [x] (cons (first a-seq) x))
                       (permutations (rest a-seq)))))]

   (if (empty? a-seq)
    '(())
    (loop [rem (rest a-seq)
           curr a-seq
           build '()]
      (if (empty? rem)
        (concat build (permutations-inner curr))
        (let [new-seq (concat (rest curr)
                              (list (first curr)))]
          (recur (rest rem)
                 new-seq
                 (concat build (permutations-inner curr)))))))))

(defn powerset [a-set]
  (if (empty? a-set)
    #{#{}}
    (loop [rem (rest a-set)
           f (first a-set)
           build #{#{}}]
      (let [m (map (fn [x] (conj x f)) build)
            u (union build m)]
        (if (empty? rem)
          u
          (recur (rest rem)
                 (first rem)
                 u))))))

