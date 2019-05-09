#lang racket
(require racket/include)
(include "FuncionesMatriz.rkt")

(define (movPosibles_aux pos cont)
  (cond ((equal? cont 8) '())
        ((equal? cont 0) (cons (list (+ (getMovFila pos) 1) (+ (getMovColumna pos) 2)) (movPosibles_aux pos (+ 1 cont))))
        ((equal? cont 1) (cons (list (+ (getMovFila pos) 2) (+ (getMovColumna pos) 1)) (movPosibles_aux pos (+ 1 cont))))
        ((equal? cont 2) (cons (list (- (getMovFila pos) 1) (+ (getMovColumna pos) 2)) (movPosibles_aux pos (+ 1 cont))))
        ((equal? cont 3) (cons (list (- (getMovFila pos) 2) (+ (getMovColumna pos) 1)) (movPosibles_aux pos (+ 1 cont))))
        ((equal? cont 4) (cons (list (+ (getMovFila pos) 1) (- (getMovColumna pos) 2)) (movPosibles_aux pos (+ 1 cont))))
        ((equal? cont 5) (cons (list (+ (getMovFila pos) 2) (- (getMovColumna pos) 1)) (movPosibles_aux pos (+ 1 cont))))
        ((equal? cont 6) (cons (list (- (getMovFila pos) 1) (- (getMovColumna pos) 2)) (movPosibles_aux pos (+ 1 cont))))
        ((equal? cont 7) (cons (list (- (getMovFila pos) 2) (- (getMovColumna pos) 1)) (movPosibles_aux pos (+ 1 cont))))
        )
  )

(define (validarMovs movs actual max)
  (cond ((null? movs) (cond (
                      (or (or (>= (getMovFila actual) max) (>= (getMovColumna actual) max))(or (< (getMovFila actual) 0) (< (getMovColumna actual) 0))) '())
                            (else (list actual))
                            )
                      )
        ((or (or (>= (getMovFila actual) max) (>= (getMovColumna actual) max))(or (< (getMovFila actual) 0) (< (getMovColumna actual) 0))) (validarMovs (cdr movs) (car movs) max))
        (else (cons actual (validarMovs (cdr movs) (car movs) max)))
        )
  )

(define (movsPosibles pos max)
  (cdr (validarMovs (movPosibles_aux pos 0) pos max))
  )

(define (movsPosiblesTodos movs max)
  (cond ((null? movs) '())
        (else (cons (movsPosibles (car movs) max) (movsPosiblesTodos (cdr movs) max)))
        )
  )



