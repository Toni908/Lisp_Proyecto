;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; <Descripció de les funcions d'aquest fitxer>

;; Necessari per a l'optimització de crides recursives.
(load 'common) ; https://almy.us/files/xl305req.zip
(load 'tco)    ; https://github.com/antoni-oliver/defun-tco

;; Altres fitxers de la pràctica:
(load 'grafics)
(load 'agent-abc123)
(load 'agent-xyz999)

(setq nombre-mapa "maps/basic2.map")


;; Inicio, el monitor recursivo sera monitor, empezaremos con una array de estados generales que sera ronda pintura e1 
;; pintura e2, y el mapa
(defun inici ()
    (monitor (cons '(1 200 200) (llegeix-exp nombre-mapa))) 
)

(defun monitor (mapa)
    (pinta mapa)

    (let* ((tecla (get-key)))
        (cond ((= tecla 333) 
                (monitor (cons (cons (+ 1 (car (car mapa))) (cdr (car mapa))) (cdr mapa)))
              )
              (t (monitor mapa))
        )
    )

)


(defun llegeix-exp (nom-fitxer)
    (let* ((fp (open nom-fitxer))
    (e (read fp nil nil)))
    (close fp)
    e)
)
