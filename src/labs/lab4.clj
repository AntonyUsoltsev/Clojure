(ns labs.lab4
  "Операции:
  - переменные: (bin-var :p)          => [:var :p]
  - константы:  (bin-true), (b-false) => [:const true/false]
  - отрицание:  (bin-not e)           => [:not e]
  - конъюнкция: (bin-and e1 e2 ...)   => [:and e1 e2]
  - дизъюнкция: (bin-or  e1 e2 ...)   => [:or e1 e2]
  - импликация: (bin-impl e1 e2)      => [:impl e1 e2]

  Для добавления новой операции
    1) добавить keyword
    2) добавить метод построения
    3) добавить правило упрощения и преобразования выражения для новой опперации
       ИЛИ добавить преобразование операции через элементарные (как с импликацией)
    4) добавить обработку операции при подставновке"
  )

;; Построение переменных

(defn bin-var
  "Переменная с именем name"
  [name]
  [:var name])

(defn bin-true
  "Булева константа true"
  []
  [:const true])

(defn bin-false
  "Булева константа false"
  []
  [:const false])

(defn bin-const
  "Булева константа (true/false)"
  [v]
  [:const (boolean v)])

(defn bin-not
  "Отрицание выражения expr"
  [expr]
  [:not expr])

(defn bin-and
  "Конъюнкция a & b"
  ([a b]
   [:and a b])
  ([a b & more]
   (reduce bin-and (bin-and a b) more)))

(defn bin-or
  "Дизъюнкция a || b"
  ([a b]
   [:or a b])
  ([a b & more]
   (reduce bin-or (bin-or a b) more)))

(defn bin-impl
  "Импликация a -> b."
  [a b]
  [:impl a b])

(defn var-expr?
  "Проверка являетя ли expr перменной"
  [expr]
  (and (vector? expr)
       (= (first expr) :var)))

(defn const-expr?
  "Проверка являетя ли expr константой"
  [expr]
  (and (vector? expr)
       (= (first expr) :const)))

(defn true-expr?
  "Проверка являетя ли expr константой равное true"
  [expr]
  (and (const-expr? expr)
       (true? (second expr))))

(defn false-expr?
  "Проверка являетя ли expr константой равное false"
  [expr]
  (and (const-expr? expr)
       (false? (second expr))))

(defn fixpoint
  "Применяет f к x, пока результат не перестанет меняться"
  [f x]
  (loop [cur x]
    (let [next (f cur)]
      (if (= cur next)
        cur
        (recur next)))))

;; Алгоритм приведения к ДНФ

;; 1) Удаление импликаций
(defn eliminate-impl
  "Заменяет импликации по правилу (A -> B) = (!A || B)."
  [expr]
  (if (vector? expr)
    (let [[op & args] expr]
      (case op
        :impl (let [[a b] args]
                (bin-or (bin-not (eliminate-impl a))
                        (eliminate-impl b)))
        :and (let [[a b] args]
               (bin-and (eliminate-impl a)
                        (eliminate-impl b)))
        :or (let [[a b] args]
              (bin-or (eliminate-impl a)
                      (eliminate-impl b)))
        :not (let [[e] args]
               (bin-not (eliminate-impl e)))
        expr))
    expr))

;; 2) Проталкивание отрицаний внутрь
(defn push-not
  "Проталкивает отрицания внутрь формулы так, чтобы :not стоял только над переменными или константами"
  [expr]
  (if (vector? expr)
    (let [[op & args] expr]
      (case op
        :not (let [e (first args)]
               (if (vector? e)
                 (let [[eop & eargs] e]
                   (case eop
                     ; !!A => A
                     :not (push-not (first eargs))
                     ; !(A && B) => !A || !B
                     :and (let [[a b] eargs]
                            (bin-or (push-not (bin-not a))
                                    (push-not (bin-not b))))
                     ; !(A || B) => !A && !B
                     :or (let [[a b] eargs]
                           (bin-and (push-not (bin-not a))
                                    (push-not (bin-not b))))
                     (bin-not (push-not e))))
                 (bin-not e)))
        :and (let [[a b] args]
               (bin-and (push-not a)
                        (push-not b)))
        :or (let [[a b] args]
              (bin-or (push-not a)
                      (push-not b)))
        expr))
    expr))

;; 3) Распределение конъюнкции над дизъюнкцией
(defn distribute-step
  "Один шаг распределения конъюнкции над дизъюнкцией"
  [expr]
  (if (vector? expr)
    (let [[op & args] expr]
      (case op
        :and (let [[a b] args
                   a' (distribute-step a)
                   b' (distribute-step b)]
               (cond
                 ;; (A || B) && C => (A && C) || (B && C)
                 (and (vector? a')
                      (= (first a') :or))
                 (let [[_ a1 a2] a']
                   (bin-or (distribute-step (bin-and a1 b'))
                           (distribute-step (bin-and a2 b'))))

                 ;; A && (B || C) => (A && B) || (A && C)
                 (and (vector? b')
                      (= (first b') :or))
                 (let [[_ b1 b2] b']
                   (bin-or (distribute-step (bin-and a' b1)) (distribute-step (bin-and a' b2))))

                 :else
                 (bin-and a' b')))

        :or (let [[a b] args]
              (bin-or (distribute-step a)
                      (distribute-step b)))

        :not (let [[e] args]
               (bin-not (distribute-step e)))

        expr))
    expr))

(defn distribute-and-over-or
  "Распределяет конъюнкцию над дизъюнкцией до достижения фиксированной точки"
  [expr]
  (fixpoint distribute-step expr))

;; Упрощение (работа с константами, снятие лишних конъюнкций/дизъюнкций)
(defn simplify-step
  "Один шаг упрощения выражения"
  [expr]
  (if (vector? expr)
    (let [[op & args] expr]
      (case op
        :and (let [[a b] (map simplify-step args)]
               (cond
                 (or (false-expr? a) (false-expr? b)) (bin-false)
                 (true-expr? a) b
                 (true-expr? b) a
                 (= a b) a
                 :else (bin-and a b)))

        :or (let [[a b] (map simplify-step args)]
              (cond
                (true-expr? a) (bin-true)
                (true-expr? b) (bin-true)
                (false-expr? a) b
                (false-expr? b) a
                (= a b) a
                :else (bin-or a b)))

        :not (let [[e] args
                   e' (simplify-step e)]
               (cond
                 (true-expr? e') (bin-false)
                 (false-expr? e') (bin-true)

                 ;; !(!A) => A
                 (and (vector? e') (= (first e') :not))
                 (second e')

                 :else (bin-not e')))
        expr))
    expr))

(defn simplify
  "Упрощает выражение до фиксированной точки"
  [expr]
  (fixpoint simplify-step expr))

;; Подстановка значения переменной
(defn substitute
  "Рекурсивно подставляет значение value (true/false) вместо переменной var-name
в выражение expr. Возвращает новое выражение"
  [expr var-name value]
  (if (vector? expr)
    (let [[op & args] expr]
      (case op
        :var (let [[name] args]
               (if (= name var-name)
                 (bin-const value)
                 expr))

        :const expr

        :not (let [[e] args]
               (bin-not (substitute e var-name value)))

        :and (let [[a b] args]
               (bin-and (substitute a var-name value)
                        (substitute b var-name value)))

        :or (let [[a b] args]
              (bin-or (substitute a var-name value)
                      (substitute b var-name value)))

        :impl (let [[a b] args]
                (bin-impl (substitute a var-name value)
                          (substitute b var-name value)))

        (into [op] (map #(substitute % var-name value) args))))
    expr))


(defn to-dnf
  "Приводит выражение expr к ДНФ"
  [expr]
  (-> expr
      eliminate-impl
      push-not
      distribute-and-over-or
      simplify))

(defn substitute_and_dnf
  "Подставляет значение value вместо переменной var-name в expr и приводит результат к ДНф"
  [expr var-name value]
  (-> expr
      (substitute var-name value)
      to-dnf))


(defn -main [& _args]

  (println "Test 1 : (P -> Q)")
  (let [p (bin-var :p)
        q (bin-var :q)
        expr (bin-impl p q)]
    (println "Start expr:" expr)
    (println "DNF:" (to-dnf expr))
    (println))

  (println "Test 2: (P && (Q || R))")
  (let [p (bin-var :p)
        q (bin-var :q)
        r (bin-var :r)
        expr (bin-and p (bin-or q r))]
    (println "Start expr:" expr)
    (println "DNF:" (to-dnf expr))
    (println))

  (println "Test 3: substitute P=false into (P && (Q || true))")
  (let [p (bin-var :p)
        q (bin-var :q)
        expr (bin-and p (bin-or q (bin-true)))]
    (println "Start expr:" expr)
    (println "DNF ans substitute:"
             (substitute_and_dnf expr :p false))
    (println))

  (println "Test 4: complex expression")
  (let [p (bin-var :p)
        q (bin-var :q)
        r (bin-var :r)
        expr (bin-or (bin-and (bin-impl p q)
                              (bin-not r))
                     (bin-and p r))]
    (println "Start expr:" expr)
    (println "DNF:" (to-dnf expr)))
  )