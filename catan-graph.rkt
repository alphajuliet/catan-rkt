#lang racket
; catan-graph.rkt
; avj 2018-04-15 

; Describe a Catan board using a graph.

; Exports for unit testing
(provide Tile
         Hex
         posn-sub
         axial-directions
         is-neighbour?)

; Imports
(require lang/posn
         threading
         graph
         "catan.rkt")

;-----------------------
; Define an empty unweighted graph
(define (new-graph)
  (unweighted-graph/undirected '()))

; Add the standard board to the graph
; init-graph :: graph → [Tile] → void
(define (init-graph! g tiles)
  (for ([ t tiles ])
    (add-vertex! g t)))

; List of offsets to neighbours in axial coordinates
; axial-directions :: [posn]
(define axial-directions
  (list (make-posn +1  0)
        (make-posn +1 -1)
        (make-posn  0 -1)
        (make-posn -1  0)
        (make-posn -1 +1)
        (make-posn  0 +1)))

; Subtract XY coordinates
; posn-sub :: Tile → Tile → Tile
(define (posn-sub a b)
  (let ([ x1 (posn-x a) ]
        [ y1 (posn-y a) ]
        [ x2 (posn-x b) ]
        [ y2 (posn-y b) ])
    (make-posn (- x1 x2) (- y1 y2))))

; Define a neighbour function
; is-neighbour? :: Tile → Tile → [posn] → boolean
(define (is-neighbour? a b offsets)
  (let ([ diff (posn-sub (Tile-pos a) (Tile-pos b)) ])
    (if (member diff offsets) #t #f)))

;-----------------------
; Add edges between neighbouring tiles. Avoid duplicate edges.
; add-neighbours! :: graph → [Tile] → void
(define (add-neighbours! g board)
  (for* ([ a board ] ; compare all pairs of tiles
         [ b board ])
    (if (and (is-neighbour? a b axial-directions)
             (not (has-edge? g b a)))
        (add-edge! g a b)
        #f)))

;-----------------------
; Run it
(define g (new-graph))
(define b (std-board))
(init-graph! g b)
(add-neighbours! g b)
; There should be 7x6 + 6x4 + 6x3 = 84 edges in the standard board, with ordering.
; Confirm with (~> g get-edges length) → 84


; The End