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
(setq MAX-TORNS 1500)


;; Inicio, el monitor recursivo sera monitor, empezaremos con una array de estados generales que sera ronda pintura e1 
;; pintura e2, y el mapa
(defun inici ()
    (monitor (cons '(1 200 200) (llegeix-exp nombre-mapa))) 
)

(defun monitor (mapa)
    (pinta mapa)
    (cond
        ; Condición de fin de partida
        ;((fi-partida mapa) (mostra-guanyador mapa))
        (t
            ; Esperar tecla
            (let* ((tecla (get-key)))
                (cond
                    ((= tecla 333) (monitor (fer-torn mapa)))  ; flecha derecha → avanzar turno
                    ((= tecla 336) (cls))                           ; flecha abajo, salir
                    (t (monitor mapa))                          ; cualquier otra → no avanzar
                )
            )
        )
    )
)

(defun fer-torn (mapa)
    (let* ((equip (equip-actual mapa)) ;; equip actual
           ; (unitats (trobar-unitats mapa equip))
           ; Para cada unidad, llamar al agente y aplicar las acciones
           ; (nou-mapa (aplicar-accions mapa equip unitats))
          )
        ; Incrementar el turno
        (cons (cons (+ 1 (car (car mapa))) (cdr (car mapa))) (cdr mapa))
    )
)

(defun fi-partida (mapa)
    (cond 
        ((base-destruida) t)
        ((>= (torn mapa) MAX-TORNS) t)
    )
)

(defun equip-actual (mapa)
  (cond 
    ((= (mod (torn mapa) 2) 0) 'e2)
    (t 'e1)
  )
)

(defun mostra-guanyador (mapa)
    (t)
)

(defun torn (mapa) 
    (car (car mapa))    
)

(defun llegeix-exp (nom-fitxer)
    (let* ((fp (open nom-fitxer))
    (e (read fp nil nil)))
    (close fp)
    e)
)
