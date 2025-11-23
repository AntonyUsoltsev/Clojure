(ns labs.lab3)

;3.1. Реализуйте параллельный вариант filter (не обязательно ленивый) с помощью future.
;Параллельная обработка должна производиться блоками по заданному числу элементов. Размер
;блоков следует вычислять вручную, без использования готовых функций, таких как partition (для
;разделения последовательности следует использовать take и drop). Продемонстрируйте прирост
;производительности в сравнении с обычным фильтром.
(defn split-into-blocks [coll block-size]
  (loop [remaining coll
         blocks []]
    (if (empty? remaining)
      blocks
      (recur (drop block-size remaining)
             (conj blocks (vec (take block-size remaining)))))))

(defn parallel-filter [pred? coll block-size]
  (let [blocks (split-into-blocks coll block-size)
        futures (doall
                  (map (fn [block]
                         (future (doall (filter pred? block))))
                       blocks))
        results (doall (map deref futures))]
    (apply concat results)))

;3.2. Реализуйте ленивый параллельный filter, который должен работать в том числе с бесконечными
;потоками. Продемонстрируйте прирост производительности в сравнении с обычным фильтром.
(defn lazy-parallel-filter [pred? coll block-size thread-block-size]
  (lazy-cat
    (parallel-filter pred? (take block-size coll) thread-block-size)
    (if (empty? coll)
      coll
      (lazy-parallel-filter pred? (drop block-size coll) block-size thread-block-size))))

(defn -main []
  (println "Default filter:")
  (println (filter even? (range 10)))
  (println)

  (println "Parallel filter")
  (println (parallel-filter even? (range 10) 3))
  (println)

  (println "Lazy parallel filter")
  (println (take 5 (lazy-parallel-filter even? (range 20) 3 1)))
  (println)

  (println "Lazy filter with inf sequence")
  (println (take 10 (lazy-parallel-filter even? (range) 5 2)))


  (let [data (range 500)
        heavy-pred (fn [x]
                     (do
                       (Thread/sleep 20)
                       (even? x)))]
    (time (doall (parallel-filter heavy-pred data 50)))
    (time (doall (lazy-parallel-filter heavy-pred data 50 5)))
    (time (doall (filter heavy-pred data)))
    )
  )