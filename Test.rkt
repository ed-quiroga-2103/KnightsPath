#lang racket
(require racket/include)
(include "FuncionesMatriz.rkt")

(define (loop movs cont matriz pos ind sol)
  (cond ((equal? cont 8) '())
        ((null? movs) '())
        ((and (miembro (car movs) sol)
          (not (null? (KnightsTour (cambiarValor ind matriz (getMovFila pos) (getMovColumna pos)) (car movs) (+ ind 1) (cons pos sol))))
        ) 
        (KnightsTour (cambiarValor ind matriz (getMovFila pos) (getMovColumna pos)) (car movs) (+ ind 1) (cons pos sol))
        )
        (else (loop (cdr movs) (+ 1 cont) matriz pos ind sol))
        )
  )

(define (KnightsTour matriz pos ind sol)
  (cond ((equal? ind (* (length matriz) (length matriz))) matriz)
        (else (loop (movsPosibles pos (length matriz)) 0 matriz pos ind sol))
        )
  )
(define (miembro lista ele)
  (cond ((null? lista) #f)
        ((equal? ele (car lista)) #t)
        (else (miembro (cdr lista) ele))
        )
  )













         