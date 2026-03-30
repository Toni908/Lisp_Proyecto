;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

;; Documentació d'això...
(defun pinta ()
    "Pinta l'estat de la partida en un torn segons l'estat passat per paràmetre."
    42)

(defun simbolo-celda (celda)
  (cond
    ((equal (car celda) 'aigua) "~")
    ((member 'lab celda) "L")
    ((member 'base celda) "X")
    ((equal (cadr celda) 'r) "R")
    ((equal (cadr celda) 'g) "G")
    ((equal (cadr celda) 'b) "B")
    (t "?")))

;; Funcion para empezar la imprimicion del mapa
(defun imprimir-mapa (mapa)
  (cls)
  (move 30 40)
  (imprimir-files mapa 0 10) ;; tiene que ser par la mida
  (color 0 0 0) ; negro
)

;; Imprime Fila por fila
(defun imprimir-files (mapa fila mida)
  (cond
    ((null mapa) nil)
    (t
     (move 30 (- 320 (* fila mida)))
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
     (pinta-terreno fila mida)
     (pinta-unidad fila mida)
     (quadrat mida)
     (imprimir-fila (cdr fila) (+ columna 1) mida)
    )
  )
)

;; pinta las unidades
(defun pinta-unidad (fila mida)
  (cond ((equal (caddr (car fila)) 'lab) (triangle mida))
        ((equal (caddr (car fila)) 'base) ())
        (t )
  )
)

;; rellena el quadrado de lineas
(defun pinta-terreno (fila mida &optional (counter mida))
  (cond
    ((= counter 0)
     (moverel 0 (- mida)))   ;; restauras posición al final
    (t
      (color-casella (car fila))
      (drawrel mida 0)
      (moverel (- mida) 1)
      (pinta-terreno fila mida (- counter 1))
    )
  )
)

; tierra o agua
(defun color-casella (casella)
  (cond
    ((equal (car casella) 'aigua) (color 42 255 255))
    ((equal (cadr casella) 'b) (color 80 120 210))
    ((equal (cadr casella) 'r) (color 210 80 80))
    ((equal (cadr casella) 'g) (color 80 185 80))
    (t (color 211 211 211))))

;; Hace un quadrado, se utiliza solo para los bordes
(defun quadrat (mida)
    (color 0 0 0) ; negro
    (drawrel 0 mida)
    (drawrel mida 0)
    (drawrel 0 (- mida))
    (drawrel (- mida) 0))

;; Hace un Circulo
(defun cercle (x y radi segments)
 (mover (+ x radi) y)
 (cercle2 x y radi (/ 360 segments) 0)
)

;; Ayuda de circulos
(defun cercle2 (x y radi pas angle)
  (cond ((< angle 360) 
         (drawr (+ x (* radi (cos (radians (+ angle pas)))))
                (+ y (* radi (sin (radians (+ angle pas))))))
         (cercle2 x y radi pas (+ angle pas)))
        (t t)))

;; Ayuda de circulo
(defun mover (x y)
 (move (round x)
       (round y)))

;; Ayuda de circulo
(defun drawr (x y)
 (draw (round x)
       (round y)))

;; ayuda de circulo
(defun radians (graus)
 (/ (* graus (* 2 pi)) 360))

;; triangulo
(defun triangle (mida)
  (color 255 255 255)
  (drawrel mida 0)             ; base hacia la derecha
  (drawrel (- (/ mida 2)) mida) ; subida a la punta
  (drawrel (- (/ mida 2)) (- mida)) ; vuelta al inicio
)