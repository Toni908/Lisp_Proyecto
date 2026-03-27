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

(defun imprimir-mapa (mapa)
  (dolist (fila mapa)
    (dolist (celda fila)
      (format t "~a " (simbolo-celda celda)))
    (format t "~%")))

;; Documentació d'això...
(defun quadrat (mida)
    (drawrel 0 mida)
    (drawrel mida 0)
    (drawrel 0 (- mida))
    (drawrel (- mida) 0))