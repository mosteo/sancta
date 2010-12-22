(load "../shop2/shop2")

(defun do-split (h)
  (print "Splitting")
  (if (> h 1) `(,@(do-split (- h 1)) ,h) '(1)))

(defdomain testdomain
(

            (:method (print-current-tasks)
                     ((eval (print-current-tasks)))
                     ())
            
            ; CONSUME
            ; Consume a single number
            (:operator (!consume ?a)
                       ((exists ?a))
                       ((exists ?a))
                       ((consumed ?a)))

            ; SPLIT
            ; This should create as many consumes as the parameter says
            (:method (split ?high)
                     Sub-split
                     ((exists ?high) (assign ?sub (do-split ?high)))
                     ((create-consumptions ?sub))
            )

            ; CREATE-CONSUMPTIONS
            ; Auxiliary method to create elementary !consumes
            (:method (create-consumptions (?first . ?rest))
                     there-are-remaining
                     (eval (> (length '?rest) 0))
                     ((!consume ?first) (create-consumptions ?rest))
                     the-last-one
                     ()
                     ((!consume ?first))
            )
))

(defproblem test testdomain
            ((exists 1) (exists 2) (exists 3))
            ((split 3))
)

(shop-untrace)
(shop-trace ':states)
(shop-trace ':methods)
(shop-trace ':axioms)
(shop-trace ':operators)
(shop-trace ':tasks)
(shop-trace ':goals)
(shop-trace ':protections)
;(shop-untrace)
(find-plans 'test :which :all :verbose :plans)
