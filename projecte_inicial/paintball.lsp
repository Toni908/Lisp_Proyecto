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
(load 'agent-agf019)
(load 'agent-agf019_2)

(setq nombre-mapa "maps/basic1.map")
(setq MAX-TORNS 1500)

; bolla (terra g bolla e1 r (r) 3 9 0 (2 1))
; lab (terra b lab e1 nil (2 2))
; base (terra g base e1 nil 1 (0 1))
; terra (terra g (0 0))

; --------------- TESTS ---------------------

; (buscar-celda '(2 2) (celdas-mapa (cdr mapa3)))

;(setq mapa-test
;  (cons
;    (list 0 200 200 0 0)
;    (iniciar-mapa
;      '(((terra r) (terra g base e1) (terra b) (terra r) (terra g))
;        ((terra g) (terra r) (terra g) (terra b) (terra r))
;        ((terra b) (terra g) (terra g bolla e2 g (b g) 5 0 0 (2 2)) (terra g) (terra b)))
;      0)))
;
;(setq mapa-amb-bolla
;  (substituir-celda '(1 1)
;    '(terra r bolla e1 r (r) 99 0 0 (1 1))
;    (cdr mapa-test)))

;(setq mapa-amb-bolla (cons (car mapa-test) mapa-amb-bolla))
;(setq unitat-bolla
;  (list 0 'e1 150 99 'bolla (list 1 1) (list 'b) 'b 0 300 nil))
;(setq unitat-base
;  (list 0 'e1 200 1 'base (list 1 1) nil nil nil nil nil))
;(setq mapa2 (aplicar-mou mapa-amb-bolla (list 1 2) unitat-bolla))

;(setq mapa2 (aplicar-crea-bolla mapa-test (list 'b (list 2 1)) unitat-base))

;(setq unitat-bolla
;  (list 0 'e1 150 33 'bolla (list 2 1) (list 'b) 'b 0 0 nil))

; (setq mapa3 (aplicar-pinta mapa2 (list 2 2) unitat-bolla))

; --------------- TESTS ---------------------()

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

;; funcion de clase
(defun llegeix-exp (nom-fitxer)
    (let* ((fp (open nom-fitxer))
    (e (read fp nil nil)))
    (close fp)
    e)
)

(defun monitor (mapa)
    (pinta mapa)
    (cond
        ((fi-partida-mapa mapa) (mostra-guanyador mapa (trobar-unitats mapa)))
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

(defun fer-torn (mapa)
    (let* ((equip (equip-actual mapa))
           (labs (comptar-labs mapa))
           (labs-equip (cond ((equal equip 'e1) (car labs))
                             (t (cadr labs))))
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
           (mapa-v2 (decrementar-cooldowns (cons nou-estat (cdr mapa)) equip))
           
           )
        (ia-action mapa-v2 equip)
        ;mapa-v2 ; temporal
    )
)

(defun ia-action (mapa equip)
    (let* ((unitats (trobar-unitats mapa))
           (unitats-equip (cond 
                              ((equal equip 'e1) (car unitats))
                              (t (cadr unitats)))))
        
        (processar-unitats mapa unitats-equip equip)
    )
)

; aplicar todas las acciones pasadas por la ia
(defun processar-unitats (mapa unitats equip)
    (cond
        ((null unitats) mapa)
        (t
            (let* ((unitat (car unitats))
                   (accions (cond
                                ((equal equip 'e1) (agent-agf019 unitat))
                                (t (agent-agf019 unitat))))
                   (mapa-v2 (aplicar-accions-unitat mapa accions unitat)))
                
                (processar-unitats mapa-v2 (cdr unitats) equip)
            )
        )
    )
)

(defun aplicar-accions-unitat (mapa accions unitat)
    (cond
        ((null accions) mapa)
        (t
            (aplicar-accions-unitat
                (aplicar-accio mapa (car accions) unitat)
                (cdr accions)
                unitat
            )
        )
    )
)

;----------------------------------------------------------------------------------



;----------------------------------------------------------------------------------
; Logica de aplicar acciones en una lista

(defun aplicar-accio (mapa accio unitat)
  (cond
    ((equal (car accio) 'crea-bolla)
     (aplicar-crea-bolla mapa (cadr accio) unitat))

    ((equal (car accio) 'pinta)
     (aplicar-pinta mapa (cadr accio) unitat))

    ((equal (car accio) 'mou)
     (aplicar-mou mapa (cadr accio) unitat))

    (t mapa))
)

; buscar-celda: busca una celda al mapa per coordenada real
; Paràmetres:
;   coord  - coordenada real (x y)
;   celdas - llista plana de totes les celdas
(defun buscar-celda (coord celdas)
    (cond ((null celdas) nil)
          ((equal coord (celda-coord (car celdas))) (car celdas))
          (t (buscar-celda coord (cdr celdas)))))

; substituir-celda-fila: substitueix una celda dins una fila
(defun substituir-celda-fila (coord nova-celda fila)
    (cond ((null fila) nil)
          ((equal coord (celda-coord (car fila)))
           (cons nova-celda (cdr fila)))
          (t (cons (car fila) (substituir-celda-fila coord nova-celda (cdr fila))))))

; substituir-celda: substitueix una celda al mapa per una nova
(defun substituir-celda (coord nova-celda mapa)
    (cond ((null mapa) nil)
          (t (cons (substituir-celda-fila coord nova-celda (car mapa))
                   (substituir-celda coord nova-celda (cdr mapa))))))

; aplicar-crea-bolla: crea una bolla nova al mapa
; Paràmetres:
;   mapa   - el mapa
;   args   - (color coordenada-amb-desplacament)
;   unitat - la base que crea la bolla (per saber equip i coord)
(defun aplicar-crea-bolla (mapa args unitat)
    (let* ((color (car args))
           (coord-dst (coord-real (cadr args) mapa))
           (coord-src (coord-real (cadr (cddddr unitat)) mapa))
           (celdas (celdas-mapa (cdr mapa)))
           (celda-dst (buscar-celda coord-dst celdas))
           (equip (cadr unitat))
           (pintura (caddr unitat)))
        (cond
            ((null celda-dst) mapa)
            ((not (equal (car (cddddr unitat)) 'base)) mapa)
            ((not (pertany color '(r g b))) mapa)
            ((> (d2 coord-src coord-dst) 2) mapa)
            ((es-unitat celda-dst) mapa)
            ((< pintura 50) mapa)
            (t (let* (
                (nou-id (+ (* (torn mapa) 10) (cond ((equal equip 'e1) 3) (t 4))))
                (nova-bolla (list (car celda-dst)
                                  (cadr celda-dst)
                                  'bolla
                                  equip
                                  color
                                  (list color)
                                  nou-id
                                  0
                                  0
                                  coord-dst))
                (nou-estat (cond
                    ((equal equip 'e1)
                     (list (torn mapa)
                           (- (estat-pintura-e1 mapa) 50)
                           (estat-pintura-e2 mapa)
                           (estat-dx mapa)
                           (estat-dy mapa)))
                    (t
                     (list (torn mapa)
                           (estat-pintura-e1 mapa)
                           (- (estat-pintura-e2 mapa) 50)
                           (estat-dx mapa)
                           (estat-dy mapa)))))
                (mapa-v2 (cons nou-estat (substituir-celda coord-dst nova-bolla (cdr mapa))))
                (unitat-nova (celda-a-unitat nova-bolla mapa-v2))
                (accions (cond
                    ((equal equip 'e1) (agent-agf019 unitat-nova))
                    (t (agent-agf019 unitat-nova))))
                (mapa-final (aplicar-accions-unitat mapa-v2 accions unitat-nova)))
                mapa-final))))) ; <-- cuerpo del let*: devuelve mapa-final, paréntesis correctos

(defun aplicar-pinta (mapa args unitat)
  (let* ((coord-dst (coord-real args mapa))
         (coord-src (coord-real (cadr (cddddr unitat)) mapa))
         (celdas (celdas-mapa (cdr mapa)))
         (celda-dst (buscar-celda coord-dst celdas))
         (celda-src (buscar-celda coord-src celdas))
         (equip-pinta (cadr unitat)))

    (cond
      ((null celda-dst) mapa)
      ((null celda-src) mapa)
      ((not (equal (celda-tipus celda-src) 'bolla)) mapa)
      ((>= (celda-tr-pintar-bolla celda-src) 1) mapa)
      ((> (d2 coord-src coord-dst) 5) mapa)

      (t
       (let* ((color (celda-color-propi-bolla celda-src))
              (tipo (celda-tipus celda-dst))
              (equip (celda-equip celda-dst))
              (nou-color-suelo color)
              
              (nous-colors
                (cond
                    ((equal tipo 'bolla)
                     (cond
                       ((pertany color (celda-colors-pintat-bolla celda-dst))
                        (celda-colors-pintat-bolla celda-dst))
                       (t
                        (cons color (celda-colors-pintat-bolla celda-dst)))))
                    ((equal tipo 'base)
                     (cond
                       ((pertany color (celda-colors-pintat-base celda-dst))
                        (celda-colors-pintat-base celda-dst))
                       (t
                        (cons color (celda-colors-pintat-base celda-dst)))))
                    (t nil)))

              (nova-celda
               (cond
                 ((equal tipo 'bolla)
                  (cond
                    ((>= (length nous-colors) 3)
                     (list (car celda-dst) nou-color-suelo (celda-coord celda-dst)))
                    (t
                     (list (car celda-dst)
                           nou-color-suelo
                           'bolla
                           equip
                           (celda-color-propi-bolla celda-dst)
                           nous-colors
                           (celda-id-bolla celda-dst)
                           (celda-tr-pintar-bolla celda-dst)
                           (celda-tr-moure-bolla celda-dst)
                           (celda-coord celda-dst)))))
                 ((equal tipo 'base)
                  (cond
                    ((>= (length nous-colors) 3)
                     (list (car celda-dst) nou-color-suelo (celda-coord celda-dst)))
                    (t
                     (list (car celda-dst)
                           nou-color-suelo
                           'base
                           equip
                           nous-colors
                           (celda-id-base celda-dst)
                           (celda-coord celda-dst)))))
                 ((equal tipo 'lab)
                  (list (car celda-dst)
                        nou-color-suelo
                        'lab
                        equip-pinta
                        nil
                        (celda-coord celda-dst)))
                 (t
                  (append (list (car celda-dst) nou-color-suelo)
                          (cddr celda-dst)))))

              (mapa-v2 (substituir-celda coord-dst nova-celda (cdr mapa)))

              ;; cooldown: triple si la casella origen no es del color de la bolla
              (color-src (cadr celda-src))
              (cooldown (cond ((equal color-src color) 3)
                              (t 9)))

              ;; actualitzar cooldown de la bolla que dispara
              (nova-unitat-src
                (list (car celda-src)
                      (cadr celda-src)
                      'bolla
                      (celda-equip celda-src)
                      (celda-color-propi-bolla celda-src)
                      (celda-colors-pintat-bolla celda-src)
                      (celda-id-bolla celda-src)
                      cooldown
                      (celda-tr-moure-bolla celda-src)
                      (celda-coord celda-src)))

              (mapa-final (substituir-celda coord-src nova-unitat-src mapa-v2)))

         (cons (car mapa) mapa-final))))))

; aplicar-mou: mou una bolla a una nova casella
; Paràmetres:
;   mapa   - el mapa
;   args   - coordenada destino amb desplaçament (x y)
;   unitat - la bolla que es mou
(defun aplicar-mou (mapa args unitat)
    (let* ((coord-dst (coord-real args mapa))
           (coord-src (coord-real (cadr (cddddr unitat)) mapa))
           (celdas (celdas-mapa (cdr mapa)))
           (celda-dst (buscar-celda coord-dst celdas))
           (celda-src (buscar-celda coord-src celdas))
           (dist (d2 coord-src coord-dst)))
        (cond
            ((null celda-dst) mapa)
            ((null celda-src) mapa)
            ((not (equal (celda-tipus celda-src) 'bolla)) mapa)
            ((es-unitat celda-dst) mapa)
            ((> dist 2) mapa)
            ((>= (celda-tr-moure-bolla celda-src) 100) mapa)
            (t
             (let* (
                (base-cooldown (cond ((= dist 2) 141) (t 100)))
                (factor (cond ((equal (cadr celda-dst)
                                      (celda-color-propi-bolla celda-src)) 1)
                              (t 3)))
                (nou-cooldown (* base-cooldown factor))
                (celda-src-buida
                    (list (car celda-src)
                          (cadr celda-src)
                          coord-src))
                (nova-bolla
                    (list (car celda-dst)
                          (cadr celda-dst)
                          'bolla
                          (celda-equip celda-src)
                          (celda-color-propi-bolla celda-src)
                          (celda-colors-pintat-bolla celda-src)
                          (celda-id-bolla celda-src)
                          (celda-tr-pintar-bolla celda-src)
                          nou-cooldown
                          coord-dst))
                (mapa-v2 (substituir-celda coord-src celda-src-buida (cdr mapa)))
                (mapa-v3 (substituir-celda coord-dst nova-bolla mapa-v2)))
                (cons (car mapa) mapa-v3))))))

;-------------------------------------------------------------------------------



; ------------------------------------------------------------------
; Inici de mapa, metadatos per poder treballar millor

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

; ------------------------------------------------------------------



; ------------------------------------------------------------------
; decrementar cooldowns logica

(defun decrementar-cooldowns (mapa equip)
  (cons (car mapa) ; mantenemos el estado
        (decrementar-filas (cdr mapa) equip)))

(defun decrementar-filas (mapa equip)
  (cond
    ((null mapa) nil)
    (t (cons (decrementar-celdas (car mapa) equip)
             (decrementar-filas (cdr mapa) equip)))))

(defun decrementar-celdas (fila equip)
    (cond
        ((null fila) nil)
        (t (cons
            (let* ((celda (car fila)))
                (cond
                    ((and (pertany 'bolla celda)
                          (equal (celda-equip celda) equip)) ; si es bolla y de nuestro equipo activo
                     (list (car celda)                        ; terra
                           (cadr celda)                       ; color
                           (caddr celda)                      ; bolla
                           (cadddr celda)                     ; equip
                           (celda-color-propi-bolla celda)    ; color-propi
                           (celda-colors-pintat-bolla celda)  ; colors-pintat
                           (celda-id-bolla celda)             ; id
                           (max 0 (- (celda-tr-pintar-bolla celda) 1)) ; tr-pintar
                           (max 0 (- (celda-tr-moure-bolla celda) 100))  ; tr-moure de 100 en 100 per a decimals
                           (celda-coord celda)))              ; coord
                    (t celda)))
            (decrementar-celdas (cdr fila) equip))
        )
    )
)

;----------------------------------------------------------------------------------



;-------------------------------------------------------------------------------
; cuenta laboratorios del mapa, y devuelve un array de (numero_lab_e1, numero_lab_e2)

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

;------------------------------------------------------------------------------------------------



;------------------------------------------------------------------------------------------------
; Control fi partida

;; fi-partida-mapa: comprova fi de partida només amb el mapa, sense trobar-unitats
;; Paràmetres:
;;   mapa - el mapa actual
(defun fi-partida-mapa (mapa)
    (cond
        ((>= (torn mapa) MAX-TORNS) t)
        ((not (te-base-al-mapa (celdas-mapa (cdr mapa)) 'e1)) t)
        ((not (te-base-al-mapa (celdas-mapa (cdr mapa)) 'e2)) t)
        (t nil)
    )
)

;; te-base-al-mapa: comprova si existeix una base d'un equip a la llista de celdas
;; Paràmetres:
;;   celdas - llista plana de totes les celdas del mapa
;;   equip  - 'e1 o 'e2
(defun te-base-al-mapa (celdas equip)
    (cond
        ((null celdas) nil)
        ((and (equal (celda-tipus (car celdas)) 'base)
              (equal (celda-equip (car celdas)) equip)) t)
        (t (te-base-al-mapa (cdr celdas) equip))
    )
)

; te-base: comprova si una llista d'unitats té una base
; Paràmetres:
;   unitats-equip - llista d'unitats d'un equip 
(defun te-base (unitats-equip)
    (cond ((null unitats-equip) nil)
          ((equal (car (cddddr (car unitats-equip))) 'base) t) ;; cogemos la primera unidad, miram la columna tipus y veim si es base
          (t (te-base (cdr unitats-equip)))))   ;; sino seguimo cercant fins que no hi hagui mes unitats


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
        (princ "Fi de partida, el guanyador es: ")
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

;------------------------------------------------------------------------------------------------      



;------------------------------------------------------------------------------------------------   
; funcions per construir l'array esta a partir del mapa

; trobar-unitats-llista: filtra les celdas que son unitats
(defun trobar-unitats-llista (celdas mapa equip)
    (cond ((null celdas) nil)
          ((and (es-unitat (car celdas))
                (equal (get-equip-celda (car celdas)) equip))
           (cons (celda-a-unitat (car celdas) mapa)
                 (trobar-unitats-llista (cdr celdas) mapa equip)))
          (t (trobar-unitats-llista (cdr celdas) mapa equip))))

; es-unitat: comprova si una celda té una unitat (base o bolla)
(defun es-unitat (celda)
    (cond ((pertany 'base celda) t)
          ((pertany 'bolla celda) t)
          (t nil)))


; trobar-unitats: retorna ((unitats-e1) (unitats-e2))
(defun trobar-unitats (mapa)
    (let* ((celdas (celdas-mapa (cdr mapa))))
        (list
            (trobar-unitats-llista celdas mapa 'e1)
            (trobar-unitats-llista celdas mapa 'e2))))

; celdas-mapa: podria llamarlo aplanar mapa, ya que me lo aplana todo en una lista para poder buscar mas facilmente
(defun celdas-mapa (mapa)
    (cond ((null mapa) nil)
          (t (append (car mapa) (celdas-mapa (cdr mapa))))
    )
)

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

;------------------------------------------------------------------------------------------------   



;----------------------------------------------------------------------------------------
;Per treballar amb el desplazament

; coord-amb-desplacament: suma dx dy a una coordenada
(defun coord-amb-desplacament (coord mapa)
    (list (+ (car coord) (estat-dx mapa))
          (+ (cadr coord) (estat-dy mapa))
    )
)

; coord-real: resta dx dy a una coordenada
(defun coord-real (coord mapa)
    (list (- (car coord) (estat-dx mapa))
          (- (cadr coord) (estat-dy mapa))
    )
)

;------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------
; funcions per agafar x-element del mapa, ya que cada unitat les te a llocs diferents
; tambe funcions que se utilitzen un poc per tot

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
(defun celda-coord (celda) (car (reverse celda)))
(defun get-equip-celda (celda) (cadddr celda)) ; get-equip-celda: retorna l'equip de la celda
(defun torn (mapa) (car (car mapa))) ;; retorna el torn de la partida

(defun equip-actual (mapa)
  (cond 
    ((= (mod (torn mapa) 2) 0) 'e2)
    (t 'e1)
  )
)


; pertany: comprova si x pertany a la llista l, funcion de clase
(defun pertany (x l)
    (cond ((null l) nil)
          ((equal x (car l)) t)
          (t (pertany x (cdr l)))))

;-----------------------------------------------------------------------------------------

