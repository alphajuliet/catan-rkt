#lang racket
; catan.rkt 

; This is a re-write in (untyped) Racket of existing Haskell code that:
; (a) generates a random board for Settlers of Catan
; (b) draws it
; (c) does some simple analysis for initial placement of pieces

; Exports
(provide Hex Hex-resource Hex-number
         Tile Tile-type Tile-pos
         std-board
         draw-board)

; Imports
(require 2htdp/image)
(require lang/posn)

; Threading function
; e.g. (>> 5 (curry + 2)) => 7
(define (>> it . fs)
  ((apply compose (reverse fs)) it))

; Zip lists under a defined function. 
; (zip-with list '(1 2) '(a b)) => '((1 a) (2 b))
(define (zip-with f xs ys)
  (for/list ([x xs] [y ys]) (f x y)))

; ----------------------------------------
; Generate the board

; Custom datatypes
(struct Hex (resource number) #:transparent)
;(struct posn (x y) #:transparent)
(struct Tile (type pos) #:transparent)

; Map resource types to colour
(define colour-map
  (hash
   'Brick  "darkorange"
   'Grain  "gold"
   'Wood   "darkgreen"
   'Wool   "green"
   'Ore    "cornflowerblue"
   'Desert "brown"))

; Return a list of n_i copies of x_i as a flattened list
; e.g. (enumerate-list '(2 3) '(a b)) => '(a a b b b)
(define (enumerate-list ns xs)
  (flatten (zip-with make-list ns xs)))

; Generate a set of hexes by shuffling and pairing, and adding the desert tile
(define std-hexes
  (let ([ns '(2 3 3 4 4 5 5 6 6 8 8 9 9 10 10 11 11 12)]
        [rs (enumerate-list '(3 4 4 4 3) '(Brick Grain Wool Wood Ore))])
    (append
     (zip-with Hex rs (shuffle ns))
     (list (Hex 'Desert 7)))))

; Define the standard hex map of 19 hexes in rows of 3, 4, 5, 4 and 3.
; See http://www.redblobgames.com/grids/hexagons for details of the axial
; coordinate system used.
(define std-map
  (list
   (make-posn 0 -2) (make-posn 1 -2) (make-posn 2 -2)
   (make-posn -1 -1) (make-posn 0 -1) (make-posn 1 -1) (make-posn 2 -1)
   (make-posn -2 0) (make-posn -1 0) (make-posn 0 0) (make-posn 1 0) (make-posn 2 0)
   (make-posn -2 1) (make-posn -1 1) (make-posn 0 1) (make-posn 1 1)
   (make-posn -2 2) (make-posn -1 2) (make-posn 0 2)))

; Define the full board, i.e. the tiles on the map
(define (create-board hex-list coords)
  (zip-with Tile (shuffle hex-list) coords))

(define (std-board)
  (create-board std-hexes std-map))

; ----------------------------------------
; Draw the board

; Draw a hex tile with a given colour and label, oriented with a vertex at the top
(define (draw-hex size hx)
  (let* ([ colour (hash-ref colour-map (Hex-resource hx)) ]
         [ hexagon (rotate 30 (regular-polygon size 6 "solid" colour)) ]
         [ txt (text (number->string (Hex-number hx)) (* size 0.8) "white") ])
    (overlay/align "middle" "middle" txt hexagon)))

; Map from axial hex coordinates to cartesian space and linearly transform (kludge)
; hexToXY size (q, r) = ( size * sqrt 3 * (q + r/2), size * 1.5 * r )
; @@TODO remove the kludge
(define (hex-to-xy size qr)
  (let ([ q (posn-x qr) ]
        [ r (posn-y qr) ])
    (make-posn (+ (* size (sqrt 3) (+ q (/ r 2)))
                  (* size 5))
               (+ (* size 1.5 r)
                  (* size 5)))))

; Draw the board
(define (draw-board size board)
  (let ([ hexes (map Tile-type board) ]
        [ coords (map Tile-pos board) ])
    (place-images
     (map (curry draw-hex size) hexes)
     (map (curry hex-to-xy size) coords)
     (square (* size 10) "solid" "grey"))))

(define (run)
  (draw-board 40 (std-board)))

; ----------------------------------------
; Analyse the board

; ...to come

; The End