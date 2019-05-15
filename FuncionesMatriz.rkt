
;Funcion auxiliar que llena las columnas de la matriz
(define (llenarLista_aux ind lim)
  (cond ((equal? ind lim) '())
        (else (cons -1 (llenarLista_aux (+ ind 1) lim)))
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

;Funcion auxiliar que devuelve la posicion de un elemento de una matriz (Si no existe devuelve una lista vacia)
(define (getPos_aux matriz ele i j size)
  (cond ((equal? (getMatEle matriz i j) ele) (list i j))
        ((and (equal? i size) (equal? j size)) '())
        ((equal? j size) (getPos_aux matriz ele (+ i 1) 0 size))
        (else (getPos_aux matriz ele i (+ j 1) size))
        )
  )
;Funcion principal que devuelve la posicion de un elemento de una matriz (Si no existe devuelve una lista vacia)
(define (getPos matriz ele)
  (getPos_aux matriz ele 0 0 (sizeOfMat matriz))
  )

;Para obtener filas y columnas de un movimiento representado por (i j) i:Fila j:Columna
(define (getMovFila mov)
  (car mov))
(define (getMovColumna mov)
  (cadr mov))

;Verifica si un movimiento es valido
(define (esMovValido actual siguiente)
  (cond ((and (equal? (+ (getMovFila actual) 1) (getMovFila siguiente)) (equal? (+ (getMovColumna actual) 2) (getMovColumna siguiente))) #t)
        ((and (equal? (+ (getMovFila actual) 2) (getMovFila siguiente)) (equal? (+ (getMovColumna actual) 1) (getMovColumna siguiente))) #t)
        ((and (equal? (+ (getMovFila actual) 1) (getMovFila siguiente)) (equal? (- (getMovColumna actual) 2) (getMovColumna siguiente))) #t)
        ((and (equal? (+ (getMovFila actual) 2) (getMovFila siguiente)) (equal? (- (getMovColumna actual) 1) (getMovColumna siguiente))) #t)
        ((and (equal? (- (getMovFila actual) 1) (getMovFila siguiente)) (equal? (+ (getMovColumna actual) 2) (getMovColumna siguiente))) #t)
        ((and (equal? (- (getMovFila actual) 2) (getMovFila siguiente)) (equal? (+ (getMovColumna actual) 1) (getMovColumna siguiente))) #t)
        ((and (equal? (- (getMovFila actual) 1) (getMovFila siguiente)) (equal? (- (getMovColumna actual) 2) (getMovColumna siguiente))) #t)
        ((and (equal? (- (getMovFila actual) 2) (getMovFila siguiente)) (equal? (- (getMovColumna actual) 1) (getMovColumna siguiente))) #t)
        (else #f)
        )
  )

;Funcion principal para cambiar un valor dentro de una matriz
(define (cambiarValor valor matriz fila columna)
  (cond ((or (>= fila (sizeOfMat matriz)) (>= columna (sizeOfMat matriz))) '())
        (else (cambiarValor_aux valor matriz fila columna 0 -1))
        )
  )

;Funcion auxiliar para cambiar un valor dentro de una matriz
(define (cambiarValor_aux valor matriz fila columna indF indC)
  (cond ((and (equal? fila indF) (equal? columna indC)) (cons valor (cdr matriz)))
        ((and (equal? fila indF) (equal? indC -1)) (cons (cambiarValor_aux valor (car matriz) fila columna indF (+ indC 1)) (cdr matriz)))
        ((equal? fila indF) (cons (car matriz) (cambiarValor_aux valor (cdr matriz) fila columna indF (+ indC 1))))
        (else (cons (car matriz) (cambiarValor_aux valor (cdr matriz) fila columna (+ indF 1) indC)))
        )
  )


;Funcion auxiliar para validar una solucion
(define (recorridoValido_aux solucion movimiento1 movimiento2)
  (cond ((null? solucion) (esMovValido movimiento1 movimiento2))
        (else (and (esMovValido movimiento1 movimiento2) (recorridoValido_aux (cdr solucion) movimiento2 (car solucion))))
        )
  )

;Funcion principal para validar un recorrido y retornar una matriz con la solucion
;HAY QUE CAMBIAR EL #t POR LA LINEA SIGUIENTE A ESTA EN LA VERSION FINAL. PARA PRUEBAS ES NECESARIO EL #t
;(equal? (sizeOfMat solucion) (* tamano tamano))
(define (PDC-Test tamano solucion)
  (cond ((equal? (* tamano tamano) (length solucion))
  (cond ((equal? 1 tamano) '((0)))
        ((not (null? solucion))(cond ((and #t (recorridoValido_aux (cddr solucion) (car solucion) (cadr solucion))) (solAMatriz solucion (crearMatriz tamano) 0))
        (else '())
        ))
        (else '())
                  )         
  )
        (else '())
        )
  )

;Funcion auxiliar que transforma una solucion a una matriz con el recorrido
(define (solAMatriz solucion matriz cont)
  (cond ((null? solucion) matriz)
        ((equal? -1 (getMatEle matriz (getMovFila (car solucion)) (getMovColumna (car solucion)))) (solAMatriz (cdr solucion) (cambiarValor cont matriz (getMovFila (car solucion)) (getMovColumna (car solucion))) (+ cont 1)))
        (else '())
        )
  )

;Funcion principal para validar un recorrido y retornar un booleano que dice si la solucion es valida
;HAY QUE CAMBIAR EL #t POR LA LINEA SIGUIENTE A ESTA EN LA VERSION FINAL. PARA PRUEBAS ES NECESARIO EL #t
;(equal? (sizeOfMat solucion) (* tamano tamano))
(define (recorridoValidoSimple tamano solucion)
  (cond ((and (equal? (sizeOfMat solucion) (* tamano tamano)) (recorridoValido_aux (cddr solucion) (car solucion) (cadr solucion))) #t)
        (else #f)
        )
  )


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

(define (PDC-Sol max pos) 
  (cond ((equal? max 1) (list pos))
        ((< max 5) '())
        ((or (>= (getMovFila pos) max) (>= (getMovColumna pos) max)) '())
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


;Funcion que valida si una casilla ya fue empleada
(define (miembro pos lista)
  (cond ((null? lista) #f)
        ((equal? (car lista) pos) #t)
        (else (miembro pos (cdr lista)))
        )
  )


;Funcion para eliminar
(define (delete_last lista)
  (aux_delete_last lista '()))

;Funcion auxiliar para eliminar el ultimo elemento de una lista
(define (aux_delete_last lista solucion)
  (cond ((null? list) (reverse solucion))
        ((null? (cdr lista)) (reverse solucion))
        (else (aux_delete_last (cdr lista) (append (list(car lista)) solucion)) )
        )
  )

;Funcion que retorna el ultimo valor de una lista
(define (last lista)
  (cond ((null? lista) lista)
        ((null? (cdr lista)) (car lista))
        (else (last (cdr lista)) )
        )
  )

(provide (all-defined-out))










