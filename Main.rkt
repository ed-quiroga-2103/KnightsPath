#lang racket/gui
(require embedded-gui)
(require racket/include)
(include "FuncionesMatriz.rkt")
(define n 5)

;Se crea y se a;ade el chess-piece-snip-class a la lista de snip-class
(define chess-piece-snip-class
  (make-object
      (class snip-class%
        (super-new)
        (send this set-classname "chess-piece-snip"))))

(send (get-the-snip-class-list) add chess-piece-snip-class)

;Definicion de la clase chess-piece
;Requiere Nombre, glyph, fuente, tama;o y posicion
(define chess-piece%
  (class snip%
    (init-field name glyph font size [location #f])
    (super-new)
    (send this set-snipclass chess-piece-snip-class)

    (define/public (set-location l) (set! location l))
    (define/public (get-location) location)
    (define/public (color)
      (if (equal? (string-upcase name) name) 'white 'black))

    (define/override (get-extent dc x y width height descent space lspace rspace)
      (when width (set-box! width size))
      (when height (set-box! height size))
      (when descent (set-box! descent 0.0))
      (when space (set-box! space 0.0))
      (when lspace (set-box! lspace 0.0))
      (when rspace (set-box! rspace 0.0)))

    (define/override (draw dc x y . other)
      (send dc set-font font)
      (send dc set-text-foreground "black")
      (define-values (glyph-width glyph-height baseline extra-space)
        (send dc get-text-extent glyph font #t))
      (let ((ox (/ (- size glyph-width) 2))
            (oy (/ (- size glyph-height 2))))
        (send dc draw-text glyph (+ x ox) (+ y oy))))
    ))

(define (valid-rank? rank) (and (>= rank 0) (< rank n)))
(define (valid-file? file) (and (>= file 0) (< file n)))



;Informacion para la pieza del caballo (Nombre y glyph del caballo)
(define chess-piece-data
  (hash
   "N" #\u2658 )
  )
  
;Funcion generadora de piezas
;Params: nombre(id) y posicion(location)
(define (make-chess-piece id [location #f])
  (define glyph (hash-ref chess-piece-data id))
  (define font (send the-font-list find-or-create-font 20 'default 'normal 'normal))
  (new chess-piece%
       [name id]
       [glyph (string glyph)]
       [font font]
       [size 35]
       [location location]))


;Definicion de la clase chess-board
;Clase pasteboard
(define chess-board%
  (class pasteboard%
    (super-new)

    (define drag-dx 0)
    (define drag-dy 0)
    (define highlight-location #f)

    (define/override (on-paint before? dc . other)
      (when before?
        (draw-chess-board dc)
        ))
    
    (define/augment (after-insert chess-piece . rest)
      (position-piece this chess-piece))

    ))

;Funcion para posicionar una pieza en el tablero
(define (position-piece board piece)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width n) (/ canvas-height n)))
  (define-values (rank file)
    (location->rank-file (send piece get-location)))
  (define-values (square-x square-y)
    (values (* file square-width) (* rank square-height)))
  (define piece-width (snip-width piece))
  (define piece-height (snip-height piece))
  (send board move-to piece
        (+ square-x (/ (- square-width piece-width) 2))
        (+ square-y (/ (- square-height piece-height) 2))))

;Funcion para relacionar una localizacion de la forma a1, b2...mn
;con una fila y columna como par ordenado, de la forma (0 0) (1 1)...(m n)
(define (location->rank-file location)
  (unless (and (string? location) (= (string-length location) 2))
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (define file
    (index-of '(#\a #\b #\c #\d #\e #\f #\g #\h #\i) (string-ref location 0)))
  (define rank
    (index-of '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) (string-ref location 1)))
  (unless (and rank file)
    (raise-argument-error 'location "valid chess position a1 .. h8" location))
  (values rank file))

;Funcion para relacionar un par ordenado, de la forma (0 0) (1 1)...(m n)
;con una localizacion de la forma "a1", "b2"..."mn"
(define (rank-file->location rank file)
  (unless (<= 0 rank n)
    (raise-argument-error 'rank "integer between 0 and n" rank))
  (unless (<= 0 file n)
    (raise-argument-error 'rank "integer between 0 and n" file))
  (string
   (list-ref '(#\a #\b #\c #\d #\e #\f #\g #\h #\i) file)
   (list-ref '(#\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9) rank)))

;Funcion para relacionar un par ordenado de pixeles con una localizacion
;dentro del tablero (x 100)(y 100) -> "a1"
(define (xy->location board x y)
  (define-values (canvas-width canvas-height)
    (let ((c (send board get-canvas)))
      (send c get-size)))
  (define-values (square-width square-height)
    (values (/ canvas-width n) (/ canvas-height n)))
  (define-values (rank file)
    (values (exact-truncate (/ y square-height)) (exact-truncate (/ x square-width))))
  (rank-file->location rank file))

;Funcion para Dibujar los cuadros labeled por rank y file
;del tablero
(define (draw-chess-board dc)
  (define brush (send the-brush-list find-or-create-brush "gray" 'solid))
  (define pen (send the-pen-list find-or-create-pen "black" 1 'transparent))
  (define font (send the-font-list find-or-create-font 5 'default 'normal 'normal))
  (define-values (dc-width dc-height) (send dc get-size))
  (define cell-width (/ dc-width n))
  (define cell-height (/ dc-height n))
  (define margin 3)
    
  (send dc clear)
  (send dc set-brush brush)
  (send dc set-pen pen)
  (send dc set-font font)
  
  (for* ([row (in-range n)] [col (in-range n)]
                            #:when (or (and (odd? row) (even? col))
                                       (and (even? row) (odd? col))))
    (define-values [x y] (values (* col cell-width) (* row cell-height)))
    (send dc draw-rectangle x y cell-width cell-height))

  (for ([(rank index) (in-indexed '("1" "2" "3" "4" "5" "6" "7" "8" "9"))])
    (define-values [_0 h _1 _2] (send dc get-text-extent rank font #t))
    (define y (+ (* index cell-height) (- (/ cell-height 2) (/ h 2))))
    (send dc draw-text rank margin y))
  
  (for ([(file index) (in-indexed '("a" "b" "c" "d" "e" "f" "g" "h" "i"))])
    (define-values [w h _1 _2] (send dc get-text-extent file font #t))
    (define x (+ (* index cell-width) (- (/ cell-width 2) (/ w 2))))
    (send dc draw-text file x (- dc-height h margin))))



;; Programa para generar un tablero de NxN:

;; The pasteboard% that will hold and manage the chess pieces
(define board (new chess-board%))
;; Toplevel window for our application
(define toplevel (new frame% [label "Chess Board"] [width (* 90 n)] [height (* 90 n)]))
;; The canvas which will display the pasteboard contents
(define canvas (new editor-canvas%
                    [parent toplevel]
                    [style '(no-hscroll no-vscroll)]
                    [horizontal-inset 0]
                    [vertical-inset 0]
                    [editor board]))
(send toplevel show #t)

(define initial
  (string-append
   "Nb9"))

;obtiene los movimientos en orden de la solucion
(define (get-moves-list solution n)
  (for/list ([i (* n n)])
    (getPos solution i)))

;Funcion para crea el tablero, recibe tama;o y matriz de solucion
;Llama a la funcion crear tablero con tama;o del tablero y la lista de movimientos de la solucion
(define (check-inputs size solution)
  (unless(recorridoValidoSimple size solution)
    (raise-argument-error 'solution "Recorrido no valido para la matriz" solution))
  (define move-list (get-moves-list solution size))
  (write move-list)
  )

;Convierte una lista de pares ordenados (posiciones i j dentro de una matriz)
;a una lista de fila columna (a1, b2 ...., mn)
(define (touple-list->rank-file-list touple-list)
  (for/list ([touple touple-list])
    (rank-file->location (car touple) (cadr touple))))

;Funcion que pinta/anima las posiciones del caballo 
(define (PDC-Pintar size path)
  (for ([elem (touple-list->rank-file-list path)])
    (send board insert (make-chess-piece "N" elem))
    (sleep 0.5)
    )
  ) 
