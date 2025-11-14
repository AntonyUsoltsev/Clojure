(ns labs.lab2-test
  (:require [clojure.test :refer :all]
            [labs.lab2 :refer :all]))

(defn linear_test [x] x)
(defn square_test [x] (* x x))
(defn cube_test [x] (* x x x))

(defn approx [expected actual & [eps]]
  (let [eps (or eps 0.02)]
    (<= (Math/abs (- expected actual)) eps)))

(deftest integrate-test
  (testing
    (is (approx 0.5 ((integrate linear_test 0.1) 1)))
    (is (approx 2.0 ((integrate linear_test 0.1) 2)))
    (is (approx (/ 1 3) ((integrate square_test 0.1) 1)))
    (is (approx (/ 8 3) ((integrate square_test 0.1) 2)))
    (is (approx 0.25 ((integrate cube_test 0.1) 1)))
    (is (approx 4.0 ((integrate cube_test 0.1) 2)))))

(deftest integrate-memoized-test
  (testing
    (is (approx 0.5 ((integrate-memoized linear_test 0.1) 1)))
    (is (approx 2.0 ((integrate-memoized linear_test 0.1) 2)))
    (is (approx (/ 1 3) ((integrate-memoized square_test 0.1) 1)))
    (is (approx (/ 8 3) ((integrate-memoized square_test 0.1) 2)))
    (is (approx 0.25 ((integrate-memoized cube_test 0.1) 1)))
    (is (approx 4.0 ((integrate-memoized cube_test 0.1) 2)))))

(deftest integrate-lazy-test
  (testing
    (is (approx 0.5 ((integrate-lazy linear_test 0.1) 1)))
    (is (approx 2.0 ((integrate-lazy linear_test 0.1) 2)))
    (is (approx (/ 1 3) ((integrate-lazy square_test 0.1) 1)))
    (is (approx (/ 8 3) ((integrate-lazy square_test 0.1) 2)))
    (is (approx 0.25 ((integrate-lazy cube_test 0.1) 1)))
    (is (approx 4.0 ((integrate-lazy cube_test 0.1) 2)))))

(run-tests)
