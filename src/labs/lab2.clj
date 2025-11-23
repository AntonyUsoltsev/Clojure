(ns labs.lab2)

;Реализовать функцию (оператор), принимающую аргументом функцию от одной переменной f и
;возвращающую функцию одной переменной, вычисляющую (численно) выражение:
; integrate_0^x (f(t)dt)
;Можно использовать метод трапеций с постоянным шагом.
;При оптимизации исходить из того, что полученная первообразная будет использоваться для
;построения графика (т.е. вызываться многократно в разных точках)

(defn square [x] (* x x))

; integrate_0^x (f(t)dt) = h * ((f_0 + f_n)/2 + sum_{1}^{n-1} (f_i))
;(defn integrate [f h]
;  (fn [x]
;    (let [n (int (/ x h))
;          x_i (map #(* h %) (range (inc n)))
;          y_i (map f x_i)
;          sum (+ (/ (first y_i) 2)
;                 (reduce (fn [acc ind] (+ acc (nth y_i ind))) 0 (range 1 n))
;                 (/ (last y_i) 2))]
;      (* h sum))))

(defn indexed-trapezoid [f h i]
  ;(println "Calculating for" (* h i) (* h (inc i)))
  (*
    (+ (f (* h i)) (f (* h (inc i))))
    h
    0.5)
  )

; integrate_0^x (f(t)dt) = sum_0^{n-1}((f_i + f_{i+1}) * 0.5 * h)
(defn integrate [f h]
  (fn [x]
    (let [n (int (/ x h))]
      (reduce + 0 (map (fn [idx] (indexed-trapezoid f h idx)) (range n))))
    )
  )

; 2.1. Оптимизируйте функцию с помощью мемоизации
(def indexed-trapezoid-mem
  (memoize indexed-trapezoid))

(defn integrate-memoized [f h]
  (memoize (fn [x]
             (let [n (int (/ x h))]
               (reduce
                 (fn [acc idx] (+ acc (indexed-trapezoid-mem f h idx)))
                 0
                 (range (dec n) -1 -1)))))
  )

; 2.2. Оптимизируйте функцию с помощью бесконечной последовательности частичных решений
; (F x_{i+1}) = (F x_i}) + 0.5 * h * (f(x_i}) + f(x_{i+1}))
(defn integrate-lazy [f h]
  (let [sums (iterate
               (fn [[x F]]
                 (let [x-next (+ x h)
                       F-next (+ F (* 0.5 h (+ (f x) (f x-next))))]
                   [x-next F-next]))
               [0.0 0.0])]
    (fn [x]
      (let [idx (int (/ x h))]
        (second (nth sums idx))))))

(defn -main []
  (let [h 0.1
        Int (integrate square h)]
    (println (Int 6))
    (println (Int 6))
    (println (Int 7))
    )

  (let [h 0.1
        IntMem (integrate-memoized square h)]
    (println (IntMem 6))
    (println (IntMem 7))
    (println (map IntMem (range 10)))
    )

  (let [h 0.1
        IntLazy (integrate-lazy square h)]
    (println (IntLazy 6))
    (println (IntLazy 7))
    )
  )



