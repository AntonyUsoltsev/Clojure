(ns lab1.core)

(defn square [x]
  (* x x))

(defn sum3 [a b c]
  (+ a b c))

(defn my-abs [x]
  (if (neg? x)
    (- x)
    x))

(defn signum [a]
  (if (neg? a)
    -1
    (if (== a 0)
      0
      1)))

(defn compare-lengths [s1 s2]
  (cond
    (> (count s1) (count s2)) "First"
    (= (count s1) (count s2)) "Equals"
    :else "Second"))


(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (dec n)))))

(defn fact-tail [n]
  (loop [i n acc 1]
    (if (zero? i)
      acc
      (recur (dec i) (* acc i)))))

(defn fact-tail-2 [n res]
    (if (zero? n)
      res
      (fact-tail-2 (- n 1) (* res n))))


(defn my-len [arr]
  (if (empty? arr)
    0
    (+ 1 (my-len (rest arr)))))

(defn my-reverse [arr]
  (reduce (fn [acc x] (cons x acc)) '() arr))


(defn -main []
  (println (+ 2 6))
  (def x 10)
  (let [y 5] (+ x y))
  (println (square 3))
  (println (sum3 3 2 4))
  (println (signum -4))
  (println (my-abs -7))
  (println (compare-lengths "hiwwwwo" "long-hi"))
  (println (fact 5))
  (println (fact-tail 5))
  (println (fact-tail-2 5 1))
  (println (my-len [1 3 4 8 7]))
  (println (my-len [1 3 4 8 7]))
  (println (my-reverse [1 3 4 8 7]))
)
