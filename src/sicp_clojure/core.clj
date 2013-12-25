(ns sicp-clojure.core
  (:gen-class)
  (:require [clojure.math.numeric-tower :as math])
  (:use [clojure.tools.logging  :only [info error ]]
        [clojure.repl           :only [source     ]]))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;TRACE;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^{:private true :dynamic true} *trace-depth* 0)
(defn tracer
  "This function is called by trace. Prints to standard output, but
  may be rebound to do anything you like. 'name' is optional."
  [name value]
  (println (str "TRACE" (when name (str " " name)) ": " value)))

(defn trace
  "Sends name (optional) and value to the tracer function, then
  returns value. May be wrapped around any expression without
  affecting the result."
  ([value] (trace nil value))
  ([name value]
     (tracer name (pr-str value))
     value))

(defn trace-indent
  "Returns an indentation string based on *trace-depth*"
  []
  (apply str (take *trace-depth* (repeat "| "))))

(defn trace-fn-call
  "Traces a single call to a function f with args. 'name' is the
  symbol name of the function."
  [name f args]
  (let [id (gensym "t")]
    (tracer id (str (trace-indent) (pr-str (cons name args))))
    (let [value (binding [*trace-depth* (inc *trace-depth*)]
                  (apply f args))]
      (tracer id (str (trace-indent) "=> " (pr-str value)))
      value)))

(defmacro deftrace
  "Use in place of defn; traces each call/return of this fn, including
   arguments. Nested calls to deftrace'd functions will print a
   tree-like structure.
   The first argument of the form definition can be a doc string"
  [name & definition]
  (let [doc-string (if (string? (first definition)) (first definition) "")
        fn-form  (if (string? (first definition)) (rest definition) definition)]
    `(do
       (def ~name)
       (let [f# (fn ~@fn-form)]
         (defn ~name ~doc-string [& args#]
           (trace-fn-call '~name f# args#))))))

(defmacro dotrace
  "Given a sequence of function identifiers, evaluate the body
  expressions in an environment in which the identifiers are bound to
  the traced functions. Does not work on inlined functions,
  such as clojure.core/+"
  [fnames & exprs]
  `(binding [~@(interleave fnames
                           (for [fname fnames]
                             `(let [f# @(var ~fname)]
                                (fn [& args#]
                                  (trace-fn-call '~fname f# args#)))))]
     ~@exprs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn foo
  "I don't do a whole lot."
  [x]
  (println x "Hello, World!"))

(defn chapter1 
  "Compound Procedures" 
  [] 
  (defn square [x] (* x x))
  (defn sum-of-squares [x y] (+ (square x) (square y)))
  (defn compound-fv [x] (sum-of-squares (+ x 1) (* x 2))))

(defn chapter2 
  "Conditional Expressions and Predicates" 
  []
  (defn abs [x] (cond 
                  (> x 0) x 
                  (= x 0) 0 
                  (< x 0) (- x))))
(defn chapter3 
  "Procedures and the Processes They Generate 
  Linear Recursion and Iteration"
  []
  (defn ^:dynamic factorial-lin-recur 
    ;linear recursion
    " (factorial 6)
      (* 6 (factorial 5))
      (* 6 (* 5 (factorial 4)))
      (* 6 (* 5 (* 4 (factorial 3))))
      (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
      (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
      (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
      (* 6 (* 5 (* 4 (* 3 2))))
      (* 6 (* 5 (* 4 6)))
      (* 6 (* 5 24))
      (* 6 120)
    720
    "
    [n]
    (if (= n 1)
      1
    ;else
      (* n (factorial-lin-recur (- n 1)))))

  (defn fact-iter [product counter max-count]
    ;letting the counter go over 
    ;makes the flow control easy
    (if (> counter max-count)
      product
    ;else
      (fact-iter (* counter product) (+ counter 1) max-count)))

  (defn fact-lin-it
    ;linear iteration
    " (factorial 6)
      (fact-iter 1 1 6)
      (fact-iter 1 2 6)
      (fact-iter 2 3 6)
      (fact-iter 6 4 6)
      (fact-iter 24 5 6)
      (fact-iter 120 6 6)
      (fact-iter 720 7 6)
      720
    "
    [n]
    (fact-iter 1 1 n))
    
  (defn ^:dynamic fact-lin-it2
    ;linear iteration with no external helper function
    [n]
    (defn ^:dynamic iter [product counter]
      (if (> counter n)
        product
      ;else
      (iter (* counter product) (+ counter 1))))
    (iter 1 1))

    "The expansion occurs as the process builds up a chain of deferred operations
    (in this case, a chain of multiplications). THe contraction occurs
    as the operations are actually performed. This type of process, characterized
    by a chain of deferred operations, is called a recursive process.

    think about: linear iterative process vs linear recursive process

    linear recursive:
      the number of deferred operations (the things we need to keep track of)
      grow linearly with n

    linear iterative:
      does not change size (always (iter x y) with n)
      we need to keep track of the same things each step"

;end chapter3
)

(defn chapter4
  "Tree recursion"
  []
  (defn ^:dynamic tree-fib [n]
    (cond 
      (= n 0) 0
      (= n 1) 1
    :else 
      (+ (tree-fib (- n 1)) (tree-fib (- n 2)))))

  (defn ^:dynamic change
    "How many different
    ways can we make change of $1.00, given 
    half-dollars, 50 
    quarters,     25
    dimes,        10
    nickels, and  5
    pennies?      1

    The number of ways to change amount a using n kinds of coins
    equals:

      the number of ways to change amount 1.00 using all but the first
      kind of coin, plus

      the number of ways to change amount 1.00-d using all n kinds of
      coins, where d is the denomination of the first kind of coin.
    "
    []
    ;100 -> (25 50)
    ;removing the first element 25, only one remains
    ;2x50 => 1

    ;1.00 - 25 = 75 / {25,50}
    ;1x25+1x50 3x25 => 2

    ;1.00 => 2x50 4x25 1x50+2x25 3
    )

    (defn first-denominatior
      [kinds-of-coins]
      (cond 
        (= kinds-of-coins 1) 1
        (= kinds-of-coins 2) 5
        (= kinds-of-coins 3) 10
        (= kinds-of-coins 4) 25
        (= kinds-of-coins 5) 50))
    
    (defn cc 
      [amount kinds-of-coins] 
      (cond 
        (or (< amount 0) (= kinds-of-coins 0)) 
          0
        (= amount 0) 
          1 
        :else
          (+ 
            (cc amount (- kinds-of-coins 1)) 
            (cc (- amount (first-denominatior kinds-of-coins)) kinds-of-coins))))

    (defn count-change 
      [amount]
      ;(count (list 1 5 10 25 50))
      ;5
      (cc amount 5))

;end chapter4
)

(defn chapter5 []
  ;2 to the power of 4 equals 16 
  ;2^4=16 a^b
  (defn power-of 
    ;time:O(n), space:O(n)
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
    (cond 
      (= b 0) 
        1
      (even? b) 
        (math/expt (fast-expt a (/ b 2)) 2)
      :else 
        (* a (fast-expt a (- b 1)))))

;end chapter5
)

(defn -main [& args]
  (chapter1)
    (info '(square 5) (square 5))
    (info '(sum-of-squares 33 55) (sum-of-squares 33 55))
    (info '(compound-fv 5) (compound-fv 5))
  (chapter2)
    (info '(abs 4) (abs 4))
  (chapter3)
    (info '(factorial-lin-recur 6) (factorial-lin-recur 6))
    "sicp-clojure.core=> (dotrace [factorial-lin-recur] (factorial-lin-recur 6))
    TRACE t1678: (factorial-lin-recur 6)
    TRACE t1679: | (factorial-lin-recur 5)
    TRACE t1680: | | (factorial-lin-recur 4)
    TRACE t1681: | | | (factorial-lin-recur 3)
    TRACE t1682: | | | | (factorial-lin-recur 2)
    TRACE t1683: | | | | | (factorial-lin-recur 1)
    TRACE t1683: | | | | | => 1
    TRACE t1682: | | | | => 2
    TRACE t1681: | | | => 6
    TRACE t1680: | | => 24
    TRACE t1679: | => 120
    TRACE t1678: => 720"    

    ;(info '(fact-lin-it 6) (fact-lin-it 6))
    (info '(fact-lin-it2 6) '(fact-lin-it2 6))
      "sicp-clojure.core=> (dotrace [iter] (fact-lin-it2 6))
      TRACE t1283: (iter 1 1)
      TRACE t1284: | (iter 1 2)
      TRACE t1285: | | (iter 2 3)
      TRACE t1286: | | | (iter 6 4)
      TRACE t1287: | | | | (iter 24 5)
      TRACE t1288: | | | | | (iter 120 6)
      TRACE t1289: | | | | | | (iter 720 7)
      TRACE t1289: | | | | | | => 720
      TRACE t1288: | | | | | => 720
      TRACE t1287: | | | | => 720
      TRACE t1286: | | | => 720
      TRACE t1285: | | => 720
      TRACE t1284: | => 720
      TRACE t1283: => 720
      720"
  (chapter4)
    (info '(tree-fib 6) (tree-fib 6))
    "sicp-clojure.core=> (dotrace [tree-fib] (tree-fib 4))
    TRACE t2050: (tree-fib 4)
    TRACE t2051: | (tree-fib 3)
    TRACE t2052: | | (tree-fib 2)
    TRACE t2053: | | | (tree-fib 1)
    TRACE t2053: | | | => 1
    TRACE t2054: | | | (tree-fib 0)
    TRACE t2054: | | | => 0
    TRACE t2052: | | => 1
    TRACE t2055: | | (tree-fib 1)
    TRACE t2055: | | => 1
    TRACE t2051: | => 2
    TRACE t2056: | (tree-fib 2)
    TRACE t2057: | | (tree-fib 1)
    TRACE t2057: | | => 1
    TRACE t2058: | | (tree-fib 0)
    TRACE t2058: | | => 0
    TRACE t2056: | => 1
    TRACE t2050: => 3"

    (info '(count-change 100) (count-change 100))
    ;292

  (chapter5)
    (info "teszt")
    (info '(power-of 2 100) (time (power-of 2N 100N)))
    (info '(fast-expt 2 100) (time (fast-expt 2N 100N)))
;end
)


