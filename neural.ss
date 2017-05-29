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
    (if (<= numIterations 0)
      '()
      (propogateIterationAndRetrain (car trainingData) numIterations)
    )
  )
)

(define propogateIterationAndRetrain
  (lambda (set currentIteration)
    (backPropogate set (forwardPropogate set)) (train (- currentIteration 1))
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
    (
  )
)

(define evaluate
  (lambda (resultsNet)
    resultsNet
  )
)

(define backPropogate
  (lambda (set realOutput)
    1
  )
)

(define getEmptyResultsNet
  (lambda (nodes)
    (map
      (lambda (a) (list a 0))
      (getAllNodeIDs nodes)
    )
  )
)

(define trainingData '())
(define nodes (createNetwork 2 3 1))
(define connections (createWeights nodes))

; Define all training data here in the format of:
; (addTrainingData '(inputs) '(outputs))
(addTrainingData '(1 1) '(1) )
(addTrainingData '(1 0) '(0) )
(addTrainingData '(0 1) '(0) )
(addTrainingData '(0 0) '(0) )

(train 1)