(ns labs.lab3-test
  (:require [clojure.test :refer :all]
            [labs.lab3 :refer :all]))

(deftest test-parallel-filter
  (testing
    (is (= (parallel-filter even? (range 10) 3)
           (filter even? (range 10)))))

  (testing
    (is (= (parallel-filter even? [] 3) [])))

  (testing
    (is (= (parallel-filter even? (range 5) 10)
           (filter even? (range 5))))))

(deftest test-lazy-parallel-filter
  (testing
    (is (= (doall (lazy-parallel-filter even? (range 10) 3 1))
           (filter even? (range 10)))))

  (testing
    (is (= (take 5 (lazy-parallel-filter even? (range) 3 1))
           [0 2 4 6 8]))))

(run-tests)
