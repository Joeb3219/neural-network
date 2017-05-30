(define reduce
  (lambda (f L id)
    (if (null? L)
      id
      (f (car L)
        (reduce f (cdr L) id))
    )
  )
)

(define multListByScalar
  (lambda (lst scalar)
    (map
      (lambda (a)
        (* a scalar)
      )
      lst
    )
  )
)

(define average
  (lambda (lst)
    (/ (reduce + lst 0) (length lst))
  )
)