;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname |Funciones Matriz|) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;Funcion auxiliar que llena las columnas de la matriz
(define (llenarLista_aux ind lim)
  (cond ((equal? ind lim) '())
        (else (cons 0 (llenarLista_aux (+ ind 1) lim)))
        )
  )
;Funcion principal que llena las columnas de la matriz
(define (llenarLista lim)
  (llenarLista_aux 0 lim))

;Funcion auxiliar que llena las filas de la matriz
(define (crearMatriz_aux ind lim)
  (cond ((equal? ind lim) '())
        (else (cons (llenarLista lim) (crearMatriz_aux (+ ind 1) lim)))
        )
  )

;Funcion principal que llena las columnas de la matriz
(define (crearMatriz lim)
  (crearMatriz_aux 0 lim))

;Funcion auxiluar que obtiene una fila de la matriz
(define (getFila_aux matriz fila contFila)
  (cond ((equal? fila contFila) (car matriz))
        (else (getFila_aux (cdr matriz) fila (+ contFila 1)))
        )
  )

;Funcion que obtiene el tamano de una matriz cuadrada
(define (sizeOfMat matriz)
  (cond ((null? matriz) 0)
        (else (+ 1 (sizeOfMat (cdr matriz))))
        )
  )

;Funcion principal que obtiene una fila de la matriz
(define (getFila matriz fila)
  (cond ((>= fila (sizeOfMat matriz)) '())
        (else (getFila_aux matriz fila 0))
        )
  )

;Funcion auxiliar que obtiene un elemento de una fila de una matriz
(define (getCol_aux matriz col contCol)
  (cond ((equal? col contCol) (car matriz))
        (else (getCol_aux (cdr matriz) col (+ contCol 1)))
        )
  )

;Funcion principal que obtiene un elemento de una fila de una matriz
(define (getCol matriz col)
  (cond ((>= col (sizeOfMat matriz)) '())
        (else (getCol_aux matriz col 0))
        )
  )


;Funcion principal que busca un elemento en una matriz obteniendo una fila y luego buscando el elemento en la fila
(define (getMatEle matriz fila col)
  (getCol (getFila matriz fila) col))


   