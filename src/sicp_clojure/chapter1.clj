(ns sicp-clojure.chapter1)

;Compound Procedures

;(define name [params] (body))
(defn square [x] (* x x))
(defn sum-of-squares [x y] (+ (square x) (square y)))
(defn compound-fv [x] (sum-of-squares (+ x 1) (* x 2)))

;Conditional Expressions and Predicates
(defn abs 
  [x] (cond
        (> x 0)
          x
        (= x 0)
          0
        (< x 0)
          (- x)))

(defn clj-even?
  "Returns true if n is even, throws an exception if n is not an integer"
   [n] (if (integer? n)
        (zero? (rem n 2))
        (throw (IllegalArgumentException. (str "Argument must be an integer: " n)))))

