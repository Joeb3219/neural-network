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

(define getAllNodeIDsFromLayer
  (lambda (layer)
    (if (null? layer)
        '()
        (cons (car layer) (getAllNodeIDsFromLayer (cdr layer)))
    )
  )
)