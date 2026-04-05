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
    (dribble "debug.txt")
    ;(monitor (cons '(1 200 200) (iniciar-mapa (llegeix-exp nombre-mapa) 0))) 
    (print (iniciar-mapa (llegeix-exp nombre-mapa) 0))
    (dribble)
)

; iniciar-mapa: recorre les files del mapa afegint meta-informació
; Paràmetres:
;   mapa - el mapa sense meta-informació
;   fila - la fila que anam
(defun iniciar-mapa (mapa f)
    (cond ((null mapa) nil)
          (t (cons (iniciar-mapa-fila (car mapa) f 0)
                   (iniciar-mapa (cdr mapa) (+ f 1)))))
)

; iniciar-mapa-fila: recorre les cel·les d'una fila afegint meta-informació
; Paràmetres:
;   fila - la fila actual
;   f    - índex de fila
;   c    - índex de columna
(defun iniciar-mapa-fila (fila f c)
    (cond ((null fila) nil)
          (t (cons (iniciar-mapa-celda (car fila) f c)
                   (iniciar-mapa-fila (cdr fila) f (+ c 1)))))
)

; iniciar-mapa-celda: afegeix coordenada a la cel·la, i si és base també id i ()
; Paràmetres:
;   celda - la cel·la actual
;   f     - índex de fila
;   c     - índex de columna
(defun iniciar-mapa-celda (celda f c)
    (cond ((pertany 'base celda)
           (append celda (list '() (list f c) (id-base celda))))
          ((pertany 'lab celda)
           (append celda (list 'nil (list f c))))
          (t
           (append celda (list (list f c))))))

; id-base: retorna 1 si és base e1, 2 si és base e2
(defun id-base (celda)
    (cond ((pertany 'e1 celda) 1)
          (t 2)))

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

;; funcion de clase
(defun llegeix-exp (nom-fitxer)
    (let* ((fp (open nom-fitxer))
    (e (read fp nil nil)))
    (close fp)
    e)
)

; pertany: comprova si x pertany a la llista l, funcion de clase
(defun pertany (x l)
    (cond ((null l) nil)
          ((equal x (car l)) t)
          (t (pertany x (cdr l)))))
