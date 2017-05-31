(load "functions.ss")

; Holds the current nodeID to be used in creating network connections.
(define currentNodeID 0)

; Returns the current value of currentNodeID and increments it for the next usage.
(define getNextID
  (lambda ()
    (set! currentNodeID (+ currentNodeID 1) ) ; First we increment and set the variable for next usage.
    (- currentNodeID 1) ; Then we return the new value - 1 to get back to our old value.
  )
)

; Creates a series of nodes.
; inputN, layersN, and outputN are the number of nodes in the input, hidden, and output layers, respectively.
(define createNetwork
  (lambda (inputN layersN outputN)
    (list
      (createLayer 0 inputN) ; Create the input layer
      (createLayer 1 layersN) ; Create the hidden layers
      (createLayer 2 outputN) ; Create the output layer
    )
  )
)

; Creates a layer via the format: (layerID (nodeIDs) )
(define createLayer
  (lambda (layerID numPairs)
    (list layerID (createPairs numPairs))
  )
)

; Generates numPairs pairIDs in a list.
(define createPairs
  (lambda (numPairs)
    (if (<= numPairs 0)
        '()
        (cons (getNextID) (createPairs (- numPairs 1)))
    )
  )
)

(define getOutputLayerID
  (lambda (nodes)
    (if (null? (cdr nodes))
        (car (car nodes)) ; The next item is null, so the car is the last layer therefore the output layer.
        (getOutputLayerID (cdr nodes))
    )
  )
)

; For each layer in 
(define createWeights
  (lambda (nodes)
    (let
      (
        (outputLayerID (getOutputLayerID nodes) )
        (currentID (car (car nodes)) )
      )
      (if (eq? currentID outputLayerID)
          '() ; This is the output layer
          ; We make a list of this layer and the next's random weights, combined with the random weights of the following layers.
          (append (randomlyWeightLayer nodes currentID (+ currentID 1)) (createWeights (cdr nodes))) ; This is a layer before the output layer
      )
    )
  )
)

(define getLayerById
  (lambda (nodes id)
    (if (null? nodes)
        '()
        (if (eq? (car (car nodes)) id)
            (car nodes)
            (getLayerById (cdr nodes) id)
        )
    )
  )
)

(define randomlyWeightLayer
  (lambda (nodes layerID nextLayerID)
    (randomlyWeightLayerHelper (car (cdr (getLayerById nodes layerID))) (car (cdr (getLayerById nodes nextLayerID))) )
  )
)

(define randomlyWeightLayerHelper
  (lambda (layerPairs nextLayerPairs)
    (if (null? layerPairs)
          '()
          (append (distributeNodeToLayer (car layerPairs) nextLayerPairs)
                (randomlyWeightLayerHelper (cdr layerPairs) nextLayerPairs))
    )
  )
)
    
(define distributeNodeToLayer
  (lambda (nodeID layer)
    (if (null? layer)
        '()
        (cons
         (randomlyWeightNode nodeID (car layer))
         (distributeNodeToLayer nodeID (cdr layer))
        )
    )
  )
)

(define randomlyWeightNode
  (lambda (nodeID targetID)
    (list nodeID targetID (getRandomWeight))
  )
)

(define currentWeight 6849)

; Returns the next "random" weight, which is psudo random, using the formula:
; X_{n+1} = (a*X_{n} + b) modulo m
(define getRandomWeight
  (let (
    (a 10102937483) ; A large number
    (b 8403041023) ; Another large number
    (m 100) ; The "seed", also used to divide the result to give a fraction. IE: number will be X/m.
  )
    (lambda ()
      (set! currentWeight (modulo (+ (* a currentWeight) b) m) ) ; First we increment and set the variable for next usage.
      (/ currentWeight m)
    )
  )
)

(define getAllNodeIDs
  (lambda (nodes)
    (if (null? nodes)
        '()
        (append (getAllNodeIDsFromLayer (car (cdr (car nodes)))) (getAllNodeIDs (cdr nodes)))
    )
  )
)

(define getAllNodeIDsFromLayerID
  (lambda (layerID)
    (car (cdr (reduce
      (lambda (a b)
        (if (eq? (car a) layerID)
            a
            b
        )
      )
      nodes
      (car nodes)
    )))
  )
)

(define getAllNodeIDsFromLayer
  (lambda (layer)
    (if (null? layer)
        '()
        (cons (car layer) (getAllNodeIDsFromLayer (cdr layer)))
    )
  )
)

(define getInputNodeIDs
  (lambda (nodes)
    (reverse (cdr
      (reduce
        (lambda (a b)
          (if (isInInputLayer? nodes a)
            (append b (cons a '()))
            b
          )
        )
        (getAllNodeIDs nodes)
        '(a)
      )
    )
    )
  )
)

(define isInInputLayer?
  (lambda (nodes id)
    (let ((inputLayer (car (cdr (car nodes)))))
      (if (null? inputLayer)
        #f
        (reduce
          (lambda (a b)
            (if (eq? a b)
              b
              #t
            )
          )
          (map (lambda (a) (eq? a id)) inputLayer)
          #f
        )
      )
    )
  )
)

(define getNetValue
  (lambda (net id)
    (reduce
      (lambda (a b)
        (if (eq? (car a) id)
          (cdr a)
          b
        )
      )
      net
      0
    )
  )
)

(define modifyConnectionWeight
  (lambda (start end delta)
    (map
      (lambda (connection)
        (if (and (eq? (car connection) start) (eq? (car (cdr connection)) end) )
          (set-cdr! (cdr connection) (cons (+ delta (car (cdr (cdr connection)))) '()))
          '()
        )
      )
      connections
    )
  )
)

(define getEmptyResultsNet
  (lambda (nodes)
    (map
      (lambda (a) (cons a 0))
      (getAllNodeIDs nodes)
    )
  )
)

(define getInboundWeightSum
  (lambda (nodeID)
    (reduce + (getInboundWeights nodeID) 0)
  )
)

(define getInboundWeights
  (lambda (nodeID)
    (reduce
      (lambda (a b)
        (if (eq? (car (cdr a)) nodeID)
          (append b (cons (car (cdr (cdr a))) '()))
          b
        )
      )
      connections
      '()
    )
  )
)

(define getLayerError
  (lambda (results expectedOutput layerID)
    (map
      (lambda (nodeID expected)
        (cons nodeID (- expected (getNetValue results nodeID)) )
      )
      (getAllNodeIDsFromLayerID layerID)
      expectedOutput
    )
  )
)

(define getConnectionWeight
  (lambda (start end)
    (reduce
      (lambda (a b)
        (if (and (eq? (car a) start) (eq? (car (cdr a)) end))
          (car (cdr (cdr a)))
          b
        )
      )
      connections
      0
    )
  )
)