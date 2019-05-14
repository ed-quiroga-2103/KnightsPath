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

(define (limite mov max)
  (and (and (< (getMovFila mov) max) (<= 0 (getMovFila mov))) (and (< (getMovColumna mov) max) (<= 0 (getMovColumna mov))))
  )


(define (vacio matriz i j)
  (and (limite (list i j) (sizeOfMat matriz)) (equal? (getMatEle matriz i j) -1))
  )

;Grado: (sizeOfMat (movsPosiblesTodos mov max))
(define (mejorMov_aux matriz movs mejor grad max)
  (cond ((null? movs) mejor)
        ((and (< (grado (car movs) matriz) grad) (vacio matriz (getMovFila (car movs)) (getMovColumna (car movs)))
              ) (mejorMov_aux matriz (cdr movs) (car movs) (grado (car movs) matriz) max))
        (else (mejorMov_aux matriz (cdr movs) mejor grad max))
        )
  )

(define (mejorMov actual matriz)
  (mejorMov_aux matriz (movsPosibles actual (sizeOfMat matriz)) '() 99 (sizeOfMat matriz))
  )
        
(define (solucion_aux pos matriz solucion ind)
  (cond ((null? (mejorMov pos matriz)) (reverse (cons pos solucion)))
        (else (solucion_aux (mejorMov pos matriz) (cambiarValor ind matriz (getMovFila pos) (getMovColumna pos)) (cons pos solucion)
                            (+ ind 1)))
        )
  )

(define (solucion pos max)
  (cond ((< max 5) '())
        (else (solucion_aux pos (crearMatriz max) '() 1))
  )
  )

(define (grado_aux movs matriz)
  (cond ((null? movs) '())
        ((vacio matriz (getMovFila (car movs)) (getMovColumna (car movs))) (cons (car movs) (grado_aux (cdr movs) matriz)))
        (else (grado_aux (cdr movs) matriz))
        )
  )

(define (grado mov matriz)
  (sizeOfMat (grado_aux (movsPosibles mov (sizeOfMat matriz)) matriz))
  )

