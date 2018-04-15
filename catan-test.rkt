#lang racket
; catan-test.rkt
; avj 2018-04-15

; Unit tests for Catan files

(require rackunit
         lang/posn
         "catan-graph.rkt")

(define catan-graph-tests

  (test-suite
   "Unit tests for catan-graph.rkt"

   ; Test posn-sub function
   (check-equal?
    (posn-sub (make-posn 5 3) (make-posn 1 2))
    (make-posn 4 1)
    "posn-sub")

   (test-case
    "Check is-neighbour? function"
    (check-equal?
     (is-neighbour? (Tile (Hex 'Brick 6) (make-posn 0 0))
                    (Tile (Hex 'Wool 4) (make-posn 1 0))
                    axial-directions)
     #t
     "Neighbour test 1")
    
    (check-equal?
     (is-neighbour? (Tile (Hex 'Brick 6) (make-posn 0 0))
                    (Tile (Hex 'Wool 4) (make-posn 2 0))
                    axial-directions)
     #f
     "Neighbour test 2")
    )))

(require rackunit/text-ui)
(run-tests catan-graph-tests)