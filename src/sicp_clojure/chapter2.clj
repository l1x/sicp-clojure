(ns sicp-clojure.chapter2)

  ;Procedures and the Processes They Generate 
  ;Linear Recursion and Iteration"

  (defn factorial-lin-recur 
    [n]
    (if (= n 1)
      1
    ;else
      (* n (factorial-lin-recur (- n 1)))))

    ;linear recursion
    ;     (factorial 6)
    ;     (* 6 (factorial 5))
    ;     (* 6 (* 5 (factorial 4)))
    ;     (* 6 (* 5 (* 4 (factorial 3))))
    ;     (* 6 (* 5 (* 4 (* 3 (factorial 2)))))
    ;     (* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
    ;     (* 6 (* 5 (* 4 (* 3 (* 2 1)))))
    ;     (* 6 (* 5 (* 4 (* 3 2))))
    ;     (* 6 (* 5 (* 4 6)))
    ;     (* 6 (* 5 24))
    ;     (* 6 120)
    ;   720

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
    
  (defn fact-lin-it2
    ;linear iteration with no external helper function
    [n]
    (defn iter [product counter]
      (if (> counter n)
        product
      ;else
      (iter (* counter product) (+ counter 1))))
    (iter 1 1))

  ;   "The expansion occurs as the process builds up a chain of deferred operations
  ;   (in this case, a chain of multiplications). THe contraction occurs
  ;   as the operations are actually performed. This type of process, characterized
  ;   by a chain of deferred operations, is called a recursive process.

  ;   think about: linear iterative process vs linear recursive process

  ;   linear recursive:
  ;     the number of deferred operations (the things we need to keep track of)
  ;     grow linearly with n

  ;   linear iterative:
  ;     does not change size (always (iter x y) with n)
  ;     we need to keep track of the same things each step"

  (defn ^:dynamic tree-fib [n]
    (cond 
      (= n 0) 0
      (= n 1) 1
    :else 
      (+ (tree-fib (- n 1)) (tree-fib (- n 2)))))

  ;   "sicp-clojure.core=> (dotrace [tree-fib] (tree-fib 4))
  ;   TRACE t2050: (tree-fib 4)
  ;   TRACE t2051: | (tree-fib 3)
  ;   TRACE t2052: | | (tree-fib 2)
  ;   TRACE t2053: | | | (tree-fib 1)
  ;   TRACE t2053: | | | => 1
  ;   TRACE t2054: | | | (tree-fib 0)
  ;   TRACE t2054: | | | => 0
  ;   TRACE t2052: | | => 1
  ;   TRACE t2055: | | (tree-fib 1)
  ;   TRACE t2055: | | => 1
  ;   TRACE t2051: | => 2
  ;   TRACE t2056: | (tree-fib 2)
  ;   TRACE t2057: | | (tree-fib 1)
  ;   TRACE t2057: | | => 1
  ;   TRACE t2058: | | (tree-fib 0)
  ;   TRACE t2058: | | => 0
  ;   TRACE t2056: | => 1
  ;   TRACE t2050: => 3"

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

