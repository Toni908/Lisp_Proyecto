;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

;; Documentació d'això...
(defun pinta (mapa)
    (cls)
    (color 0 0 0)
    ; Cabecera con info
    (princ "Ronda: ")
    (princ (car (car mapa)))
    
    (move 30 80)
    (princ "    Turn de Equip: ")
    (print (equip-actual mapa))
    
    ; Leyenda equipos
    (princ "Pintura E1: ")
    (print (cadr (car mapa)))
    
    (princ "Pintura E2: ")
    (print (caddr (car mapa)))
    
    ; Mapa
    (imprimir-files (cdr mapa) 0 10)
    (color 0 0 0)
)

(defun equip-actual (mapa)
  (cond 
    ((= (mod (car (car mapa)) 2) 0) 'e2)
    (t 'e1)
  )
)

;; Imprime Fila por fila
(defun imprimir-files (mapa fila mida)
    (cond
        ((null mapa) nil)
        (t
            (move 30 (- 315 (* fila mida)))
            (imprimir-fila (car mapa) 0 mida)
            (imprimir-files (cdr mapa) (+ fila 1) mida)
        )
    )
)

;; Imprime elemento, se utiliza con files
(defun imprimir-fila (fila columna mida)
    (cond
        ((null fila) nil)
        (t
            (moverel mida 0)
            (pinta-terreno (car fila) mida)
            (pinta-unidad (car fila) mida) ; para formas diferentes no cuadradas
            (pinta-marca (car fila) mida) ; pinta las marcas de las unidades
            (quadrat mida) ; marco cuadrado
            (imprimir-fila (cdr fila) (+ columna 1) mida)
        )
    )
)

(defun pinta-marca (casella mida)
    (moverel (/ mida 2) (/ mida 2))
    (let* ((marcas (cadddr (cdr (cdr casella )))))
          (cond ((contiene marcas 'b) 
                    (color 80 120 210)
                    (linea)
                )
                (t t))
          (cond ((contiene marcas 'r)
                    (color 210 80 80)
                    (moverel 0 2)
                    (linea)
                    (moverel 0 -2)
                )
                (t t))
          (cond ((contiene marcas 'g)
                    (color 80 185 80)
                    (moverel 0 -2)
                    (linea)
                    (moverel 0 2)
                )
                (t t))
    )
    (moverel (- (/ mida 2)) (- (/ mida 2)))
)

(defun linea ()
    (drawrel -2 0)
    (drawrel 5 0)
    (moverel -3 0)
)

(defun contiene (l e)
    (cond ((null l) nil)
          ((equal (car l) e) t)
          (t (contiene (cdr l) e))
    )
)

;; pinta las unidades
(defun pinta-unidad (casella mida)
    (cond
        ((equal (caddr casella) 'lab) (triangle mida))
        ((equal (caddr casella) 'unidad) (cercle casella mida))
        (t )
    )
)

;; rellena el quadrado de lineas
(defun pinta-terreno (casella mida &optional (counter mida))
    (cond
        ((= counter 0)
            (moverel 0 (- mida)))   ;; restaura posición al final
        (t
            (color-casella casella)
            (drawrel mida 0)
            (moverel (- mida) 1)
            (pinta-terreno casella mida (- counter 1))
        )
    )
)

; definir el color de las casillas base, agua, color, o equipo
(defun color-casella (casella)
    (cond
        ((equal (caddr casella) 'base) ; prioridad a las bases
            (cond
                ((equal (cadddr casella) 'e1) (color 255 0 255)) ; o 128 0 128 para morado mas profundo
                (t (color 255 255 0))
            )
        )
        ((equal (caddr casella) 'lab) ; el triangulo se lo haremos en estructura
            (cond
                ((equal (cadddr casella) 'e1) (color 255 0 255)) ; o 128 0 128 para morado mas profundo
                ((equal (cadddr casella) 'e2) (color 255 255 0)) ; o 128 0 128 para morado mas profundo
                (t (color 0 0 0))
            )
        )
        ((equal (car casella) 'aigua) (color 42 255 255))
        ((equal (cadr casella) 'b) (color 80 120 210))
        ((equal (cadr casella) 'r) (color 210 80 80))
        ((equal (cadr casella) 'g) (color 80 185 80))
        (t (color 211 211 211))
    )
)

; el color de las unidades, solo lo usaran los soldados
(defun color-unidad (casella)
    (cond
        ((equal (cadddr (cdr casella)) 'b) (color 80 120 210))
        ((equal (cadddr (cdr casella)) 'r) (color 210 80 80))
        ((equal (cadddr (cdr casella)) 'g) (color 80 185 80))
        (t t)
    )
)

;; Hace un quadrado, se utiliza solo para los bordes
(defun quadrat (mida)
    (color 0 0 0) ; negro
    (drawrel 0 mida)
    (drawrel mida 0)
    (drawrel 0 (- mida))
    (drawrel (- mida) 0)
)

; pone el puntero en el centro, llama a cercle y luego deja el puntero en su sitio
(defun cercle (casella mida)
    (color-unidad casella)
    (moverel (/ mida 2) 0)
    (cercle-fill mida)

    (color 0 0 0)
    (drawrel (/ mida 2) (/ mida 2))
    (drawrel (- (/ mida 2)) (/ mida 2))
    (drawrel (- (/ mida 2)) (- (/ mida 2)))
    (drawrel (/ mida 2) (- (/ mida 2)))

    (moverel (- (/ mida 2)) 0) ; restaurar posición
)

; rellena el rombo
(defun cercle-fill (mida &optional (counter mida))
    (cond
        ((= counter 0) 
            (moverel (/ mida 2) (- (/ mida 2)))
        )
        (t
            (drawrel (/ mida 2) (/ mida 2))
            (moverel (- (/ mida 2)) (- (/ mida 2)))
            (cond
                ((= (mod counter 2) 0) (moverel 0 1)) 
                (t (moverel (- 1) 0))
            )
            
            (cercle-fill mida (- counter 1))
        )
    )
)


;; triangulo laboratorio
(defun triangle (mida)
    (color 255 255 255)
    (triangle-fill mida 0)
    ;; restaurar posición como haces en pinta-terreno
    (moverel 0 (- mida))
)

; rellena triangulo
(defun triangle-fill (mida y)
    (cond
        ((= y mida) nil)
        (t
            ;; dibuja línea horizontal del triángulo
            (drawrel (- mida y) 0)
            ;; volver al inicio de la línea
            (moverel (- (- mida y)) 1)
            (triangle-fill mida (+ y 1))
        )
    )
)