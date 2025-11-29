(ns labs.lab4-test
  (:require [clojure.test :refer :all]
            [labs.lab4 :refer :all]))

(deftest test-constructors
  (is (= (bin-var :p) [:var :p]))
  (is (= (bin-true) [:const true]))
  (is (= (bin-false) [:const false]))
  (is (= (bin-not (bin-var :q))
         [:not [:var :q]]))
  (is (= (bin-and (bin-var :p) (bin-var :q))
         [:and [:var :p] [:var :q]]))
  (is (= (bin-or (bin-var :p) (bin-var :q) (bin-var :r))
         [:or [:var :p] [:or [:var :q] [:var :r]]])))


(deftest test-eliminate-impl
  (let [p (bin-var :p)
        q (bin-var :q)]
    (is (= (eliminate-impl (bin-impl p q))
           (bin-or (bin-not p) q)))))


(deftest test-push-not
  (let [p (bin-var :p)
        q (bin-var :q)
        expr (bin-not (bin-and p q))]
    (is (= (push-not expr)
           (bin-or (bin-not p)
                   (bin-not q)))))

  (let [p (bin-var :p)]
    (is (= (push-not (bin-not (bin-not p)))
           p))))


(deftest test-distribute
  (let [p (bin-var :p)
        q (bin-var :q)
        r (bin-var :r)
        expr (bin-and p (bin-or q r))]
    (is (= (distribute-and-over-or expr)
           (bin-or (bin-and p q)
                   (bin-and p r)))))

  (let [a (bin-var :a)
        b (bin-var :b)
        c (bin-var :c)
        expr (bin-and (bin-or a b) c)]
    (is (= (distribute-and-over-or expr)
           (bin-or (bin-and a c)
                   (bin-and b c))))))


(deftest test-simplify
  (let [p (bin-var :p)]
    (is (= (simplify (bin-and p (bin-true)))
           p)))

  (let [p (bin-var :p)]
    (is (= (simplify (bin-and p (bin-false)))
           (bin-false))))

  (let [p (bin-var :p)]
    (is (= (simplify (bin-or p (bin-false)))
           p)))

  (let [p (bin-var :p)]
    (is (= (simplify (bin-or p (bin-true)))
           (bin-true))))

  (let [p (bin-var :p)]
    (is (= (simplify (bin-not (bin-not p)))
           p))))


(deftest test-substitute
  (let [p (bin-var :p)
        expr (bin-and p (bin-var :q))]
    (is (= (substitute expr :p true)
           (bin-and (bin-const true) (bin-var :q)))))

  (let [expr (bin-or (bin-var :p) (bin-var :q))]
    (is (= (substitute expr :p false)
           (bin-or (bin-const false) (bin-var :q))))))


(deftest test-to-dnf-simple
  (let [p (bin-var :p)
        q (bin-var :q)]
    (is (= (to-dnf (bin-impl p q))
           (bin-or (bin-not p) q)))))


(deftest test-substitute-and-dnf
  (let [p (bin-var :p)
        q (bin-var :q)
        expr (bin-and p (bin-or q (bin-true)))]
    (is (= (substitute_and_dnf expr :p false)
           (bin-false))))

  (let [p (bin-var :p)
        q (bin-var :q)
        expr (bin-and p (bin-or q (bin-false)))]
    (is (= (substitute_and_dnf expr :q true)
           p))))


(deftest test-complex-example
  (let [p (bin-var :p)
        q (bin-var :q)
        r (bin-var :r)
        expr (bin-or (bin-and (bin-impl p q)
                              (bin-not r))
                     (bin-and p r))
        dnf (to-dnf expr)]
    (is (nil? (some #{:impl} (tree-seq vector? rest dnf))))  ;нет импликаций в днфф

    (is (= dnf
           (bin-or
             (bin-or
               (bin-and (bin-not p) (bin-not r))
               (bin-and q (bin-not r)))
             (bin-and p r))))))
