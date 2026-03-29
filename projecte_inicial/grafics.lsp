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
  (imprimir-files mapa 0 14)
)

;; Imprime Fila por fila
(defun imprimir-files (mapa fila mida)
  (cond
    ((null mapa) nil)
    (t
     (move 30 (+ (* fila mida) 40))
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
     (quadrat mida)
     (imprimir-fila (cdr fila) (+ columna 1) mida)
    )
  )
)

;; Hace un quadrado
(defun quadrat (mida)
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
