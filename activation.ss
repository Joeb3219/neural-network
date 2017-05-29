(define sigmoid
  (lambda (n)
    (/ 1.0 (+ 1.0 (exp (- 0 n) ) ) )
  )
)

(define sigmoidPrime
  (lambda (n)
    (* (sigmoid n) (- 1 (sigmoid n)) )
  )
)