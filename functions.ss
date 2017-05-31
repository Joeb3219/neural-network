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

(define flattenInputDeltas
  (lambda (lst)
    (if (null? lst)
        '()
        (if (pair? (car lst))
          (append (flattenInputDeltas (car lst)) (flattenInputDeltas (cdr lst))) 
          (cons lst '())
        )
    )
  )
)

(define prettyPrint
  (lambda (text resultNet)
    (let ((inputs (getAllNodeIDsFromLayerID 0)) (outputs (getAllNodeIDsFromLayerID 2)))
      (display "===") (display text) (display "===") (newline)
      (display "{")
      (map
        (lambda (nodeID)
          (display (getNetValue resultNet nodeID)) (display ",")
        )
        inputs
      )
      (display "} => {")
      (map
        (lambda (nodeID)
          (display (getNetValue resultNet nodeID)) (display ",")
        )
        outputs
      )
      (display "}") (newline) (newline)
    )
  )
)