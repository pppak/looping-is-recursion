(ns looping-is-recursion)

(defn power [base exp]
  (let [no-one-man (fn [curr exp base]
                     (if (zero? exp)
                       curr
                       (recur (* curr base) (dec exp) base)))]
    (no-one-man 1 exp base)))

(defn last-element [a-seq]
  (let [get-last (fn [a-seq]
                   (if (empty? (rest a-seq))
                     (first a-seq)
                     (recur (rest a-seq))))]
    (get-last a-seq)))

(defn seq= [seq1 seq2]
  (let [feminism (fn [seq1 seq2]
                   (if (and (empty? seq1) (empty? seq2))
                     true
                     (if (or (empty? seq1) (empty? seq2))
                       false
                       (if (not (= (first seq1) (first seq2)))
                         false
                         (recur (rest seq1) (rest seq2))))))]
    (feminism seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         a-seq a-seq
         i 0]
    (if (empty? a-seq)
      nil
      (if (pred (first a-seq))
        i
        (recur pred (rest a-seq) (inc i))))))

(defn avg [a-seq]
  (loop [i 0
         sum 0
         a-seq a-seq]
    (if (empty? a-seq)
      (/ sum i)
      (recur (inc i) (+ sum (first a-seq)) (rest a-seq)))))

(defn toggle [a-set elem]
  (if (contains? a-set elem) (disj a-set elem) (conj a-set elem)))

(defn parity [a-seq]
  (loop [a-seq a-seq
         a-set #{}]
    (if (empty? a-seq)
      a-set
      (recur (rest a-seq) (toggle a-set (first a-seq))))))

(defn fast-fibo [n]
  (loop [curr 0
         prev 1
         n n]
    (if (= 0 n)
      curr
      (recur (+ curr prev) curr (dec n)))))

(defn cut-at-repetition [a-seq]
  (loop [a-seq a-seq
         n (count (set a-seq))
         new-seq ()]
    (if (= n 0)
      new-seq
      (recur (rest a-seq) (dec n)  (reverse (conj (reverse new-seq) (first a-seq)))))))
