(load "activation.ss")
(load "network.ss")

; We define a network as follows:
; A network consists of two parts: a list of nodes and a list of connections
; The nodes list is structed as follows:
;  ;  A list of: ( (layer (nodeIDs)) (layer2 (nodeIDs)) ... )
; The connections list is structed as follows:
;  ;  A list of: ( (nodeID targetID weight) (nodeID2 targetID2 weight2) ... )

(define addTrainingData
  (lambda (input output)
    (set! trainingData
      (append 
        trainingData (cons (list input output) '())
      )
    )
  )
)

(define train
  (lambda (numIterations)
    (propogateIterationAndRetrain (car trainingData) numIterations)
    
  )
)

(define propogateIterationAndRetrain
  (lambda (set currentIteration)
    (if (> currentIteration 1)
      (begin (backPropogate set (forwardPropogate set)) (train (- currentIteration 1)))
      (backPropogate set (forwardPropogate set))
    )
  )
)

; Forward propogation works as such:
; Given an input vector, we assign each of the indices to the first layer of the network.
; We then, one by one, propogate each of the input nodes through the ENTIRE system, adding its result to the previous step.
; Once all results have propogated, we apply a sigmoid function to every node except the input.
; Most of these actions are done via a mapping function
(define forwardPropogate
  (lambda (set)
    (let (
           (input (car set))
           (output (car (cdr set)))
          )
      (evaluate (setInputs input (getEmptyResultsNet nodes)))
    )
  )
)

(define setInputs
  (lambda (input resultNet)
    (map
      (lambda (input nodeID)
        (setResult resultNet nodeID input)
      )
      input
      (getInputNodeIDs nodes)
    )
    resultNet
  )
)

(define setResult
  (lambda (results key value)
    (map
      (lambda (instance)
        (if (eq? (car instance) key)
            (set-cdr! instance value)
            instance
        )
      )
      results
    )
  )
)

(define addToResult
  (lambda (results key value)
    (map
      (lambda (instance)
        (if (eq? (car instance) key)
            (set-cdr! instance (+ (cdr instance) value))
            instance
        )
      )
      results
    )
  )
)

(define applyFunctionToNode
  (lambda (results key function)
    (map
      (lambda (instance)
        (if (eq? (car instance) key)
            (set-cdr! instance (function (cdr instance)))
            instance
        )
      )
      results
    )
  )
)

(define evaluate
  (lambda (resultsNet)
    (map
      (lambda (resultNode)
        (evaluateNode (car resultNode) (cdr resultNode) resultsNet)
        (if (isInInputLayer? nodes (car resultNode))
          '()
          (applyFunctionToNode resultsNet (car resultNode) activationFunction)
        )
      )
      resultsNet
    )
    resultsNet
  )
)

(define evaluateNode
  (lambda (nodeID nodeValue resultNet)
    (map
      (lambda (connectionNode)
        (let (
               (start (car connectionNode))
               (end (car (cdr connectionNode)))
               (value (car (cdr (cdr connectionNode))))
             )
          (if (eq? start nodeID)
              (addToResult resultNet end (* value nodeValue)); This is the correct node, so let's add its weight to the current node value
              '() ; We aren't looking at the right start node, so return null.
          )
        )
      )
      connections
    )
    resultNet
  )
)

(define backPropogate
  (lambda (set resultNet)
    (let (
           (input (car set))
           (output (car (cdr set)))
          )
      (let ((correctedOutputWeights (getCorrectedWeightsOutput set resultNet)))
        (let ((correctedHiddenWeights (getCorrectedWeightsHidden correctedOutputWeights resultNet)))
          (let ((correctedInputtWeights (getCorrectedWeightsInput correctedOutputWeights set resultNet)))
            ;(display "Output deltas: ") (display correctedOutputWeights) (newline)
            ;(display "Hidden deltas: ") (display correctedHiddenWeights) (newline)
            ;(display "Input deltas: ") (display correctedInputtWeights) (newline)
            ;(display "MERGING: ") (display (append correctedHiddenWeights correctedInputtWeights)) (newline)
            (map
              (lambda (connectionDelta)
                (modifyConnectionWeight (car connectionDelta) (car (cdr connectionDelta)) (car (cdr (cdr connectionDelta))))
              )
              (append correctedHiddenWeights correctedInputtWeights)
            )
            resultNet
          )
        )
      )
    )
  )
)

(define getCorrectedWeightsInput
  (lambda (outputErrors hiddenDeltas resultNet)
    (let* (
            (outputID (car (car outputErrors)))
            (deltaOutputSum (cdr (car outputErrors)))
            (deltaHiddenSum (multListByScalar
                              (map
                                *
                                (map
                                  (lambda (nodeID)
                                    (getConnectionWeight nodeID outputID)
                                  )
                                  (getAllNodeIDsFromLayerID 1)
                                )
                                (map activationFunctionPrime (getInboundWeights outputID))
                              )
                              deltaOutputSum
                            )
            )
          )
      (flattenInputDeltas (map
        (lambda (exteriorNodeID hiddenSum)
          (map
            (lambda (nodeID)
              (cons
                nodeID
                (cons
                  exteriorNodeID
                  (cons
                    (* (getNetValue resultNet nodeID) hiddenSum)
                    '()
                  )
                )
              )
            )
            (getAllNodeIDsFromLayerID 0)
          )
        )
        (getAllNodeIDsFromLayerID 1)
        deltaHiddenSum
      ))
    )
  )
)

; TODO: Currently only considers one output node, and should be expanded to map output nodes to true input nodes.
(define getCorrectedWeightsHidden
  (lambda (outputErrors resultNet)
    (let ((deltaOutputSum (cdr (car outputErrors))))
      (map
        (lambda (nodeID)
          (cons
            nodeID
            (cons
              (car (car outputErrors))
              (cons
                (* deltaOutputSum (getNetValue resultNet nodeID))
                '()
              )
            )
          )
        )
        (getAllNodeIDsFromLayerID 1)
      )
    )
  )
)

(define getCorrectedWeightsOutput
  (lambda (set resultNet)
    (let ((layerError (getLayerError resultNet (car (cdr set)) 2) ))
      (map
        (lambda (nodeID)
          (cons
            nodeID
            (* (activationFunctionPrime (getInboundWeightSum nodeID)) (reduce (lambda (a b) (if (eq? (car a) nodeID) (cdr a) b)) layerError 0 ) )
          )
        )
        (getAllNodeIDsFromLayerID 2)
      )
    )
  )
)

(define activationFunction sigmoid)
(define activationFunctionPrime sigmoidPrime)
(define trainingData '())
(define nodes (createNetwork 2 3 1))
(define connections (createWeights nodes))

; Define all training data here in the format of:
; (addTrainingData '(inputs) '(outputs))
(addTrainingData '(1 1) '(1) )
(addTrainingData '(1 0) '(0) )
(addTrainingData '(0 1) '(0) )
(addTrainingData '(0 0) '(0) )

(define beforeTraining (forwardPropogate  (car trainingData)))

(define results (train 20000))

(define afterTraining (forwardPropogate  (car trainingData)))

(display "Before training: ") (display beforeTraining) (newline)
(display "Training: ") (display results) (newline)
(display "After training: ") (display afterTraining) (newline)
