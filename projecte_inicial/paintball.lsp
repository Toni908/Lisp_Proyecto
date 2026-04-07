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
    ;(dribble "debug.txt")
    ;(comptar-labs (cons (list 1 200 200 (random 1000) (random 1000)) (iniciar-mapa (llegeix-exp nombre-mapa) 0)))
    (monitor (cons (list 0 200 200 (random 1000) (random 1000)) (iniciar-mapa (llegeix-exp nombre-mapa) 0))) 
    ;(print (iniciar-mapa (llegeix-exp nombre-mapa) 0))
    ;(print (trobar-unitats (cons (list 1 200 200 500 500) (iniciar-mapa (llegeix-exp nombre-mapa) 0))))
    ;(dribble)
)

; iniciar-mapa: recorre les files del mapa afegint meta-informació
; Paràmetres:
;   mapa - el mapa sense meta-informació
;   fila - la fila que anam
(defun iniciar-mapa (mapa f)
    (cond ((null mapa) nil)
          (t (cons (iniciar-mapa-fila (car mapa) f 0)
                   (iniciar-mapa (cdr mapa) (+ f 1))))
    )
)

; iniciar-mapa-fila: recorre les cel·les d'una fila afegint meta-informació
; Paràmetres:
;   fila - la fila actual
;   f    - índex de fila
;   c    - índex de columna
(defun iniciar-mapa-fila (fila f c)
    (cond ((null fila) nil)
          (t (cons (iniciar-mapa-celda (car fila) f c)
                   (iniciar-mapa-fila (cdr fila) f (+ c 1))))
    )
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
           (append celda (list (list f c))))
    )
)

; id-base: retorna 1 si és base e1, 2 si és base e2
(defun id-base (celda)
    (cond ((pertany 'e1 celda) 1)
          (t 2)
    )
)

(defun monitor (mapa)
    (let* ((unitats (trobar-unitats mapa)))
        (pinta mapa)
        (cond
            ((fi-partida mapa unitats) (mostra-guanyador mapa unitats))
            (t
                (let* ((tecla (get-key)))
                    (cond
                        ((= tecla 333) (monitor (fer-torn mapa)))
                        ((= tecla 336) (cls))
                        (t (monitor mapa))
                    )
                )
            )
        )
    )
)

; mostra-guanyador: determina i mostra el guanyador
; Paràmetres:
;   mapa    - el mapa
;   unitats - ((unitats-e1) (unitats-e2))
(defun mostra-guanyador (mapa unitats)
    (let* ((te-base-e1 (te-base (car unitats)))
           (te-base-e2 (te-base (cadr unitats)))
           (bolles-e1 (compta-bolles (car unitats)))
           (bolles-e2 (compta-bolles (cadr unitats)))
           (pintura-e1 (estat-pintura-e1 mapa))
           (pintura-e2 (estat-pintura-e2 mapa))
           (guanyador
               (cond
                   ; base e1 destruida → guanya e2 ; base e2 destruida → guanya e1
                   ((not te-base-e1) 'e2)
                   ((not te-base-e2) 'e1)
                   ; desempat per bolles
                   ((> bolles-e1 bolles-e2) 'e1)
                   ((> bolles-e2 bolles-e1) 'e2)
                   ; desempat per pintura
                   ((> pintura-e1 pintura-e2) 'e1)
                   ((> pintura-e2 pintura-e1) 'e2)
                   ; desempat aleatori
                   (t (cond ((= (random 2) 0) 'e1)
                            (t 'e2))))))
        (print guanyador)
        guanyador))

; compta-bolles: compta les bolles vives d'un equip
; Paràmetres:
;   unitats-equip - llista d'unitats d'un equip
(defun compta-bolles (unitats-equip)
    (cond ((null unitats-equip) 0)
          ((equal (car (cddddr (car unitats-equip))) 'bolla)
           (+ 1 (compta-bolles (cdr unitats-equip))))
          (t (compta-bolles (cdr unitats-equip)))))

(defun fer-torn (mapa)
    (let* ((equip (equip-actual mapa))
           (labs (comptar-labs mapa))
           (labs-equip (cond ((equal equip 'e1) (car labs))
                             (t (cadr labs))))
           ; Actualitzar pintura del equip actiu
           (nou-estat (cond 
               ((equal equip 'e1)
                (list (+ 1 (torn mapa))
                      (+ (estat-pintura-e1 mapa) 2 labs-equip)
                      (estat-pintura-e2 mapa)
                      (estat-dx mapa)
                      (estat-dy mapa)))
               (t
                (list (+ 1 (torn mapa))
                      (estat-pintura-e1 mapa)
                      (+ (estat-pintura-e2 mapa) 2 labs-equip)
                      (estat-dx mapa)
                      (estat-dy mapa)))))
           (mapa-actualitzat (cons nou-estat (cdr mapa)))
           ; TODO: decrementar cooldowns
           ; TODO: cridar agents i aplicar accions
          )
        ; Incrementar el torn
        (cons nou-estat (cdr mapa-actualitzat))
    )
)

; cuenta laboratorios del mapa, y devuelve un array de (lab e1, lab e2)
(defun comptar-labs (mapa)
    (list (compta-labs-e (cdr mapa) 'e1) (compta-labs-e (cdr mapa) 'e2)) ; cdr mapa para quitar los metadatos primeros
)

(defun compta-labs-e (mapa x)
    (cond ((null mapa) 0)
          (t (+ (compta-labs-files (car mapa) x) (compta-labs-e (cdr mapa) x)))
    )    
)

(defun compta-labs-files (fila x)
    (cond ((null fila) 0)
          ((and (equal (caddr (car fila)) 'lab) 
                (equal (cadddr (car fila)) x))
           (+ 1 (compta-labs-files (cdr fila) x)))
          (t (compta-labs-files (cdr fila) x))
    )
)

(defun fi-partida (mapa unitats)
    (cond
        ((not (te-base (car unitats))) t)   ; base e1 destruida
        ((not (te-base (cadr unitats))) t)  ; base e2 destruida
        ((>= (torn mapa) MAX-TORNS) t)
        (t nil)
    )
)

(defun equip-actual (mapa)
  (cond 
    ((= (mod (torn mapa) 2) 0) 'e2)
    (t 'e1)
  )
)

(defun torn (mapa) (car (car mapa)))

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
            (calcular-visio (celda-coord celda) tipus mapa))))                                                  ; 11. visio - nil de moment

(defun celda-a-visio (celda mapa)
    (let* ((tipus (car celda)))
        (cond
            ; Agua: solo coordenada y tipo
            ((equal tipus 'aigua)
             (list (coord-amb-desplacament (celda-coord celda) mapa) 'aigua))
            ; Terra
            (t (let* ((color (cadr celda))
                      (element (caddr celda)))
                (cond
                    ; Terra buida: el tercer element es una llista (la coordenada)
                    ((listp element)
                     (list (coord-amb-desplacament (celda-coord celda) mapa) 'terra color))
                    ; Terra amb lab
                    ((equal element 'lab)
                     (list (coord-amb-desplacament (celda-coord celda) mapa) 'terra color
                           'lab (cadddr celda)))
                    ; Terra amb base
                    ((equal element 'base)
                     (list (coord-amb-desplacament (celda-coord celda) mapa) 'terra color
                           'base (celda-equip celda) (celda-colors-pintat-base celda) nil nil nil))
                    ; Terra amb bolla
                    ((equal element 'bolla)
                     (list (coord-amb-desplacament (celda-coord celda) mapa) 'terra color
                           'bolla (celda-equip celda) (celda-colors-pintat-bolla celda)
                           (celda-color-propi-bolla celda)
                           (celda-tr-pintar-bolla celda)
                           (celda-tr-moure-bolla celda)))))))))

; calcular-visio: retorna la llista de celdas visibles per una unitat
; Paràmetres:
;   coord-real - coordenada real (x y) de la unitat al mapa
;   tipus      - 'base o 'bolla
;   mapa       - el mapa
(defun calcular-visio (coord-real tipus mapa)
    (let* ((rang (cond ((equal tipus 'base) 64)
                       (t 20)))
           (celdas (celdas-mapa (cdr mapa)))
           (celdas-visibles (celdas-en-rang coord-real rang celdas)))
        (mapcar (lambda (c) (celda-a-visio c mapa)) celdas-visibles)))

; celdas-en-rang: retorna totes les celdas del mapa dins del rang d'una coordenada
; Paràmetres:
;   coord  - coordenada real (x y) de la unitat
;   rang   - rang màxim de visió (20 per bolla, 64 per base)
;   celdas - llista plana de totes les celdas del mapa
(defun celdas-en-rang (coord rang celdas)
    (cond ((null celdas) nil)
          ((<= (d2 coord (celda-coord (car celdas))) rang)
           (cons (car celdas) (celdas-en-rang coord rang (cdr celdas))))
          (t (celdas-en-rang coord rang (cdr celdas)))))

; d2: calcula la distancia euclidiana al quadrat entre dos coordenades
; Paràmetres:
;   coord-a - llista (x y)
;   coord-b - llista (x y)
(defun d2 (coord-a coord-b)
    (+ (* (- (car coord-a) (car coord-b)) (- (car coord-a) (car coord-b)))
       (* (- (cadr coord-a) (cadr coord-b)) (- (cadr coord-a) (cadr coord-b)))))

;; tenemos las coordenadas siempre al final del mapa
(defun celda-coord (celda)
    (car (reverse celda)))

; coord-amb-desplacament: suma dx dy a una coordenada
(defun coord-amb-desplacament (coord mapa)
    (list (+ (car coord) (estat-dx mapa))
          (+ (cadr coord) (estat-dy mapa))))

;; funcions per agafar x-element del mapa, ya que cada unitat les te a llocs diferents
(defun estat-torn (mapa) (car (car mapa)))
(defun estat-pintura-e1 (mapa) (cadr (car mapa)))
(defun estat-pintura-e2 (mapa) (caddr (car mapa)))
(defun estat-dx (mapa) (cadddr (car mapa)))
(defun estat-dy (mapa) (car (cddddr (car mapa))))
(defun celda-tipus (celda) (caddr celda))
(defun celda-equip (celda) (cadddr celda))
(defun celda-colors-pintat-base (celda) (car (cddr (cddr celda))))
(defun celda-id-base (celda) (cadr (cddr (cddr celda))))
(defun celda-color-propi-bolla (celda) (car (cddr (cddr celda))))
(defun celda-colors-pintat-bolla (celda) (cadr (cddr (cddr celda))))
(defun celda-id-bolla (celda) (caddr (cddr (cddr celda))))
(defun celda-tr-pintar-bolla (celda) (cadddr (cddr (cddr celda))))
(defun celda-tr-moure-bolla (celda) (car (cddddr (cddr (cddr celda)))))
(defun celda-coord-base (celda) (caddr (cddr (cddr celda)))) ;; realment podria sustituir aquestes dues per celda-coord, ya que el vaig cambiar per a sempre estar al final la coordenada en el mapa nostre
(defun celda-coord-bolla (celda) (cadr (cddddr (cddr (cddr celda)))))

; trobar-unitats-llista: filtra les celdas que son unitats
(defun trobar-unitats-llista (celdas mapa equip)
    (cond ((null celdas) nil)
          ((and (es-unitat (car celdas))
                (equal (get-equip-celda (car celdas)) equip))
           (cons (celda-a-unitat (car celdas) mapa)
                 (trobar-unitats-llista (cdr celdas) mapa equip)))
          (t (trobar-unitats-llista (cdr celdas) mapa equip))))

; te-base: comprova si una llista d'unitats té una base
; Paràmetres:
;   unitats-equip - llista d'unitats d'un equip 
(defun te-base (unitats-equip)
    (cond ((null unitats-equip) nil)
          ((equal (car (cddddr (car unitats-equip))) 'base) t) ;; cogemos la primera unidad, miram la columna tipus y veim si es base
          (t (te-base (cdr unitats-equip)))))   ;; sino seguimo cercant fins que no hi hagui mes unitats

; trobar-unitats: retorna ((unitats-e1) (unitats-e2))
(defun trobar-unitats (mapa)
    (let* ((celdas (celdas-mapa (cdr mapa))))
        (list
            (trobar-unitats-llista celdas mapa 'e1)
            (trobar-unitats-llista celdas mapa 'e2))))

(defun celdas-mapa (mapa)
    (cond ((null mapa) nil)
          (t (append (car mapa) (celdas-mapa (cdr mapa))))))