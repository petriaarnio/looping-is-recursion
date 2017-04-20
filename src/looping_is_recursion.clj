(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (if (empty? (rest a-seq))
                   (first a-seq)
                   (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2)) true
                   (or (empty? seq1) (empty? seq2) ) false
                   (not (= (first seq1) (first seq2))) false
                   :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [acc 0
         next-seq a-seq]
    (cond
      (empty? next-seq) nil
      (pred (first next-seq)) acc
      :else (recur (inc acc) (rest next-seq)))))

(defn avg [a-seq]
  (loop [acc 0
         n 0
         next-seq a-seq]
    (cond
      (and (empty? next-seq) (= n 0)) 0
      (empty? next-seq) (/ acc n)
      :else (recur (+ acc (first next-seq)) (inc n) (rest next-seq)))))

(defn parity [a-seq]
  (loop [acc-set (set [])
         x-seq a-seq]
    (let [elem (first x-seq)
          toggle (fn [a-set yelem]
                    (if (contains? a-set elem)
                      (disj a-set yelem)
                      (conj a-set yelem)))]
      (cond
        (empty? x-seq) acc-set
        :else (recur (toggle acc-set elem) (rest x-seq))))))

(defn fast-fibo [n]
  (loop [xn n
         x 0
         y 1]
    (if (< xn 1)
      x
      (recur (dec xn) y (+ x y)))))

(defn cut-at-repetition [a-seq]
  (loop [acc []
         x-seq a-seq]
    (let [elem (first x-seq)]
      (cond
        (empty? x-seq) acc
        (contains? (set acc) elem) acc
        :else (recur (conj acc elem) (rest x-seq))))))

