;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Antonio Garcia Font
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
    ;(monitor (cons (list 1 200 200 (random 1000) (random 1000)) (iniciar-mapa (llegeix-exp nombre-mapa) 0))) 
    ;(print (iniciar-mapa (llegeix-exp nombre-mapa) 0))
    (print (trobar-unitats (cons (list 1 200 200 500 500) (iniciar-mapa (llegeix-exp nombre-mapa) 0))))
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
           (append celda (list '() (id-base celda) (list f c))))
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

; get-equip-celda: retorna l'equip de la celda
(defun get-equip-celda (celda)
    (cadddr celda))

; es-unitat: comprova si una celda té una unitat (base o bolla)
(defun es-unitat (celda)
    (cond ((pertany 'base celda) t)
          ((pertany 'bolla celda) t)
          (t nil)))

; celda-a-unitat: construeix la llista d'info d'una unitat a partir de la celda i l'estat
(defun celda-a-unitat (celda mapa)
    (let* ((tipus (celda-tipus celda))                              ; agafa el tipus: 'base o 'bolla
           (equip (celda-equip celda))                              ; agafa l'equip: 'e1 o 'e2
           (pintura (cond ((equal equip 'e1) (cadr (car mapa)))     ; pintura e1 del estat
                          (t (caddr (car mapa)))))                  ; pintura e2 del estat
           (base (equal tipus 'base)))                              ; boolea: es base o no?
        (list
            (car (car mapa))                                        ; 1. ronda - del estat
            equip                                                   ; 2. equip
            pintura                                                 ; 3. pintura - del estat segons equip
            (cond (base (celda-id-base celda))                      ; 4. id - pos 6 si base
                  (t (celda-id-bolla celda)))                       ;       pos 7 si bolla
            tipus                                                   ; 5. tipus-unitat
            (cond (base (coord-amb-desplacament (celda-coord-base celda) mapa))
                  (t (coord-amb-desplacament (celda-coord-bolla celda) mapa)))
            (cond (base (celda-colors-pintat-base celda))           ; 7. colors-pintat - pos 5 si base
                  (t (celda-colors-pintat-bolla celda)))            ;                   pos 6 si bolla
            (cond (base nil)                                        ; 8. color-propi - nil si base
                  (t (celda-color-propi-bolla celda)));               pos 5 si bolla
            (cond (base nil)                                        ; 9. tr-pintar - nil si base
                  (t (celda-tr-pintar-bolla celda)))                ;              pos 8 si bolla
            (cond (base nil)                                        ; 10. tr-moure - nil si base
                  (t (celda-tr-moure-bolla celda)))                 ;              pos 9 si bolla
            nil)))                                                  ; 11. visio - nil de moment

;; tenemos las coordenadas siempre al final del mapa
(defun celda-coord (celda)
    (car (reverse celda)))

; coord-amb-desplacament: suma dx dy a una coordenada
(defun coord-amb-desplacament (coord mapa)
    (list (+ (car coord) (estat-dx mapa))
          (+ (cadr coord) (estat-dy mapa))))


(defun estat-torn (mapa) (car (car mapa)))
(defun estat-pintura-e1 (mapa) (cadr (car mapa)))
(defun estat-pintura-e2 (mapa) (caddr (car mapa)))
(defun estat-dx (mapa) (cadddr (car mapa)))
(defun estat-dy (mapa) (car (cddddr (car mapa))))
(defun celda-tipus (celda) (caddr celda))
(defun celda-equip (celda) (cadddr celda))
(defun celda-colors-pintat-base (celda) (car (cddr (cddr celda))))
(defun celda-id-base (celda) (cadr (cddr (cddr celda))))
(defun celda-coord-base (celda) (caddr (cddr (cddr celda))))
(defun celda-color-propi-bolla (celda) (car (cddr (cddr celda))))
(defun celda-colors-pintat-bolla (celda) (cadr (cddr (cddr celda))))
(defun celda-id-bolla (celda) (caddr (cddr (cddr celda))))
(defun celda-tr-pintar-bolla (celda) (cadddr (cddr (cddr celda))))
(defun celda-tr-moure-bolla (celda) (car (cddddr (cddr (cddr celda)))))
(defun celda-coord-bolla (celda) (cadr (cddddr (cddr (cddr celda)))))

; trobar-unitats-llista: filtra les celdas que son unitats
(defun trobar-unitats-llista (celdas mapa equip)
    (cond ((null celdas) nil)
          ((and (es-unitat (car celdas))
                (equal (get-equip-celda (car celdas)) equip))
           (cons (celda-a-unitat (car celdas) mapa)
                 (trobar-unitats-llista (cdr celdas) mapa equip)))
          (t (trobar-unitats-llista (cdr celdas) mapa equip))))

; trobar-unitats: retorna ((unitats-e1) (unitats-e2))
(defun trobar-unitats (mapa)
    (let* ((celdas (celdas-mapa (cdr mapa))))
        (list
            (trobar-unitats-llista celdas mapa 'e1)
            (trobar-unitats-llista celdas mapa 'e2))))

(defun celdas-mapa (mapa)
    (cond ((null mapa) nil)
          (t (append (car mapa) (celdas-mapa (cdr mapa))))))