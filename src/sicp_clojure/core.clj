(ns sicp-clojure.core
  (:gen-class)
  (:use 
    [clojure.tools.logging  :only [info error]  ]
    [clojure.repl           :only [source    ]  ]
    ;[sicp-clojure.trace                        ]
    [sicp-clojure.chapter1                      ]
    [sicp-clojure.chapter2                      ]
    [sicp-clojure.chapter3                      ]
    ))

(defn -main [& args]
  ;chapter1
    (info '(square 5) (square 5))
    (info '(sum-of-squares 33 55) (sum-of-squares 33 55))
    (info '(compound-fv 5) (compound-fv 5))
    (info '(abs 4) (abs 4))
  ;chapter2
    (info '(factorial-lin-recur 6) (factorial-lin-recur 6))
    (info '(fact-lin-it2 6) '(fact-lin-it2 6))
    (info '(tree-fib 6) (tree-fib 6))
    (info '(count-change 100) (count-change 100))
    ;292
  ;chapter3
    (info "slow:")
    (dotimes [n 10]
      (time (power-of 2N 200N)))
    (info "fast:")
    (dotimes [n 10]
      (time (fast-expt 2N 200N)))
    
;end -main
)


