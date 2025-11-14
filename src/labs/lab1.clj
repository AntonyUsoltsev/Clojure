(ns labs.lab1)


; 1. Базовые операции над структурами данных
; Общее условие:
; Задан набор символов и число n. Опишите функцию, которая возвращает список всех строк длины n,
; состоящих из этих символов и не содержащих двух одинаковых символов, идущих подряд.


; 1.1. Решите задачу с помощью элементарных операций над последовательностями и рекурсии
(defn permutations [alphabet n]
  (if (= n 1)
    (map str alphabet)
    (let [shorter (permutations alphabet (dec n))]
      (reduce
        (fn [acc prefix]
          (concat acc
                  (reduce
                    (fn [a s]
                      (if (= (last prefix) s)
                        a
                        (conj a (str prefix s))))
                    '()
                    alphabet)))
        '()
        shorter))))


(defn gen-strings [alphabet n]
  (if (= n 1)
    (map str alphabet)
    (for [s (gen-strings alphabet (dec n))
          c alphabet
          :when (not= (last s) c)]
      (str s c))))


; 1.2. Перепишите программу 1.1. так, чтобы все рекурсивные вызовы были хвостовыми

(defn permutations-tail [alphabet n]
  (loop [current (map str alphabet)
         k 1]
    (if (= k n)
      current
      (recur
        (reduce (fn [acc prefix]
                  (reduce (fn [a s]
                            (if (= (last prefix) s)
                              a
                              (conj a (str prefix s))))
                          acc
                          alphabet))
                '()
                current)
        (inc k)))))

(defn gen-strings-tail [alphabet n]
  (loop [current (map str alphabet)
         k 1]
    (if (= k n)
      current
      (recur
        (for [prefix current
              s alphabet
              :when (not= (last prefix) s)]
          (str prefix s))
        (inc k)))))


; 1.3. Определить функции my-map и my-filter, аналогичные map (для одного списка) и filter, выразив
; их через reduce и базовые операции над списками (cons, first, concat и т.п.)

(defn my-map [f arr]
  (reduce (fn [acc x]
            (conj acc (f x)))
          [] arr))

(defn my-filter [pred arr]
  (reduce (fn [acc x]
            (if (pred x)
              (conj acc x)
              acc))
          [] arr))


; 1.4. Решите задачу с помощью элементарных операций над последовательностями и функционалов
; map/reduce/filter
(defn gen-strings-func [alphabet n]
  (loop [current (map str alphabet)
         len 1]
    (if (= len n)
      current
      (recur
        (mapcat (fn [s]
                  (map #(str s %) (filter (fn [c] (not= (last s) c)) alphabet)))
                current)
        (inc len)))))


(defn -main []
  (println "1.1:" (permutations [\a \b \c] 3))
  (println "1.2:" (permutations-tail [\a \b \c] 3))
  (println "1.3 map/filter:"
           (my-map inc [1 2 3])
           (my-filter odd? [1 2 3 4]))
  (println "1.4:" (gen-strings-func [\a \b \c] 2))

  (println "1.1:" (gen-strings [\a \b \c] 2))
  (println "1.2:" (gen-strings-tail [\a \b \c] 2))
)