(define reduce
  (lambda (f L id)
    (if (null? L)
      id
      (f (car L)
        (reduce f (cdr L) id))
    )
  )
)