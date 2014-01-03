(ns sicp-clojure.chapter3
 (:require
    [clojure.math.numeric-tower :as math]))

  (defn power-of 
    ;time:O(n), space:O(n)
    ;2 to the power of 4 equals 16 
    ;2^4=16 a^b
    [a b] 
    (if (= b 0)
      1
    ;else
      (* a (power-of a (- b 1)))))

  (defn power-of-lin [a b]
    ;time:O(n), space:O(1)
    (defn expt-iter [a counter product]
      (if (= counter 0)
        product
      ;else
        (expt-iter a (- counter 1) (* a product))))
    (expt-iter a b 1))

  (defn fast-expt [a b]
    (defn c3-square [x] (* x x))
    (cond 
      (= b 0) 
        1
      (even? b) 
        (c3-square (fast-expt a (/ b 2)))
      :else 
        (* a (fast-expt a (- b 1)))))

  (defn a-mul-b [a b]
    (if (= b 0) 0
    ;else
    (+ a (* a (- b 1)))))

    ;This algorithm takes a number of steps that is linear in b.
    ;Now suppose we include, together with addition, operations
    ;double, which doubles an integer, and halve, which
    ;divides an (even) integer by 2. Using these, design a multiplication
    ;procedure analogous to fast-expt that uses a
    ;logarithmic number of steps.

  (defn fast-a-mul-b [a b] true)


