;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Antonio Garcia Font
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del controlador principal.
;; Aquest fitxer conté la lògica principal del joc de paintball:
;; - Gestió de torns i estat del joc
;; - Aplicació d'accions de les unitats (moure, pintar, crear bolles)
;; - Control de cooldowns i condicions de fi de partida
;; - Càlcul de visió i distàncies
;; - Construcció de l'estat de les unitats a partir del mapa

;; Necessari per a l'optimització de crides recursives.
(load 'common) ; https://almy.us/files/xl305req.zip
(load 'tco)    ; https://github.com/antoni-oliver/defun-tco

;; Altres fitxers de la pràctica:
(load 'grafics)
(load 'agent-agf019)
(load 'agent-agf019_2)

(setq nombre-mapa "maps/basic2.map")
(setq MAX-TORNS 1500)
(setq *agent-agf019-random-state* (make-random-state t)) ;; Inicialització de l'estat aleatori

; --------------- MAPA DE REFERENCIA ---------------------
; aixi es como formateig el mapa a partir del fitxer.

; bolla (terra g bolla e1 r (r) 3 9 0 (2 1))
; lab (terra b lab e1 nil (2 2))
; base (terra g base e1 nil 1 (0 1))
; terra (terra g (0 0))

; --------------- TESTS ---------------------
; com vaig fer la IA al final, tenia aquests tests per provar les funcions d'aplicar accions, però ja no em serveixen per a res,
; els deixo comentats per si de cas són d'algun tipus d'interès per a algú

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

;; inici: Funció d'inici del joc
;; Inicialitza l'estat aleatori, carrega el mapa i inicia el monitor
;; Paràmetres: cap
;; Retorna: el resultat de la funció monitor
(defun inici ()
    (make-random-state t)
    ;(dribble "debug.txt")
    ;(comptar-labs (cons (list 1 200 200 (random 1000) (random 1000)) (iniciar-mapa (llegeix-exp nombre-mapa) 0)))
    (monitor (cons (list 0 200 200 (random 1000) (random 1000)) (iniciar-mapa (llegeix-exp nombre-mapa) 0))) 
    ;(print (iniciar-mapa (llegeix-exp nombre-mapa) 0))
    ;(print (trobar-unitats (cons (list 1 200 200 500 500) (iniciar-mapa (llegeix-exp nombre-mapa) 0))))
    ;(dribble)
)

;; llegeix-exp: Llegeix una expressió d'un fitxer
;; Paràmetres:
;;   nom-fitxer - nom del fitxer a llegir
;; Retorna: l'expressió llegida del fitxer
(defun llegeix-exp (nom-fitxer)
    (let* ((fp (open nom-fitxer))
    (e (read fp nil nil)))
    (close fp)
    e)
)

;; monitor: Bucle principal del joc que gestiona l'entrada de l'usuari
;; Mostra el mapa i espera input de teclat per avançar torns
;; Paràmetres:
;;   mapa - l'estat actual del mapa
;; Retorna: el resultat de mostra-guanyador quan acaba la partida
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

;; fer-torn: Executa un torn complet del joc
;; Actualitza l'estat de pintura segons els laboratoris, decrementa cooldowns
;; i crida la IA per executar les accions de l'equip actual
;; Paràmetres:
;;   mapa - l'estat actual del mapa
;; Retorna: el mapa actualitzat després d'executar el torn
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

;; ia-action: Crida la IA per obtenir i executar les accions de l'equip actual
;; Paràmetres:
;;   mapa  - l'estat actual del mapa
;;   equip - l'equip que juga aquest torn ('e1 o 'e2)
;; Retorna: el mapa després de processar totes les unitats
(defun ia-action (mapa equip)
    (let* ((unitats (trobar-unitats mapa))
           (unitats-equip (cond 
                              ((equal equip 'e1) (car unitats))
                              (t (cadr unitats)))))
        
        (processar-unitats mapa unitats-equip equip)
    )
)

;; processar-unitats: Processa totes les unitats d'un equip, aplicant les seves accions
;; Paràmetres:
;;   mapa   - l'estat actual del mapa
;;   unitats - llista d'unitats de l'equip
;;   equip  - l'equip actual ('e1 o 'e2)
;; Retorna: el mapa després d'aplicar totes les accions
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

;; aplicar-accions-unitat: Aplica totes les accions d'una unitat al mapa
;; Paràmetres:
;;   mapa    - l'estat actual del mapa
;;   accions - llista d'accions a aplicar
;;   unitat  - la unitat que executa les accions
;; Retorna: el mapa després d'aplicar totes les accions
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

;; aplicar-accio: Aplica una acció específica al mapa
;; Paràmetres:
;;   mapa   - l'estat actual del mapa
;;   accio  - l'acció a aplicar (crea-bolla, pinta o mou)
;;   unitat - la unitat que executa l'acció
;; Retorna: el mapa després d'aplicar l'acció
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

;; buscar-celda: Busca una cel·la al mapa per coordenada real
;; Paràmetres:
;;   coord  - coordenada real (x y)
;;   celdas - llista plana de totes les cel·les
;; Retorna: la cel·la trobada o nil si no existeix
(defun buscar-celda (coord celdas)
    (cond ((null celdas) nil)
          ((equal coord (celda-coord (car celdas))) (car celdas))
          (t (buscar-celda coord (cdr celdas)))))

;; substituir-celda-fila: Substitueix una cel·la dins una fila
;; Paràmetres:
;;   coord      - coordenada de la cel·la a substituir
;;   nova-celda - la nova cel·la
;;   fila       - la fila on cercar
;; Retorna: la fila amb la cel·la substituïda
(defun substituir-celda-fila (coord nova-celda fila)
    (cond ((null fila) nil)
          ((equal coord (celda-coord (car fila)))
           (cons nova-celda (cdr fila)))
          (t (cons (car fila) (substituir-celda-fila coord nova-celda (cdr fila))))))

;; substituir-celda: Substitueix una cel·la al mapa per una nova
;; Paràmetres:
;;   coord      - coordenada de la cel·la a substituir
;;   nova-celda - la nova cel·la
;;   mapa       - el mapa complet
;; Retorna: el mapa amb la cel·la substituïda
(defun substituir-celda (coord nova-celda mapa)
    (cond ((null mapa) nil)
          (t (cons (substituir-celda-fila coord nova-celda (car mapa))
                   (substituir-celda coord nova-celda (cdr mapa))))))

;; aplicar-crea-bolla: Crea una bolla nova al mapa
;; Aplica les validacions necessàries (cost de pintura, distància, ocupació)
;; i crida recursivament la IA per la nova bolla creada
;; Paràmetres:
;;   mapa   - l'estat actual del mapa
;;   args   - (color coordenada-amb-desplaçament)
;;   unitat - la base que crea la bolla
;; Retorna: el mapa amb la nova bolla creada i les seves accions aplicades
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
                mapa-final)))))

;; aplicar-pinta: Aplica l'acció de pintar una cel·la
;; Gestiona els colors pintats, destrucció d'unitats i cooldowns
;; Paràmetres:
;;   mapa   - l'estat actual del mapa
;;   args   - coordenada destí amb desplaçament
;;   unitat - la bolla que pinta
;; Retorna: el mapa amb la cel·la pintada i cooldown actualitzat
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

;; aplicar-mou: Mou una bolla a una nova casella
;; Gestiona els cooldowns segons la distància i el color de la casella
;; Paràmetres:
;;   mapa   - l'estat actual del mapa
;;   args   - coordenada destí amb desplaçament (x y)
;;   unitat - la bolla que es mou
;; Retorna: el mapa amb la bolla moguda i cooldown actualitzat
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

;; iniciar-mapa: Recorre les files del mapa afegint meta-informació (coordenades i ids)
;; Paràmetres:
;;   mapa - el mapa sense meta-informació
;;   f    - índex de la fila actual
;; Retorna: el mapa amb totes les cel·les inicialitzades
(defun iniciar-mapa (mapa f)
    (cond ((null mapa) nil)
          (t (cons (iniciar-mapa-fila (car mapa) f 0)
                   (iniciar-mapa (cdr mapa) (+ f 1))))
    )
)

;; iniciar-mapa-fila: Recorre les cel·les d'una fila afegint meta-informació
;; Paràmetres:
;;   fila - la fila actual
;;   f    - índex de fila
;;   c    - índex de columna
;; Retorna: la fila amb totes les cel·les inicialitzades
(defun iniciar-mapa-fila (fila f c)
    (cond ((null fila) nil)
          (t (cons (iniciar-mapa-celda (car fila) f c)
                   (iniciar-mapa-fila (cdr fila) f (+ c 1))))
    )
)

;; iniciar-mapa-celda: Afegeix coordenada a la cel·la, i si és base també id i llista buida
;; Paràmetres:
;;   celda - la cel·la actual
;;   f     - índex de fila
;;   c     - índex de columna
;; Retorna: la cel·la amb la meta-informació afegida
(defun iniciar-mapa-celda (celda f c)
    (cond ((pertany 'base celda)
           (append celda (list '() (id-base celda) (list f c))))
          ((pertany 'lab celda)
           (append celda (list 'nil (list f c))))
          (t
           (append celda (list (list f c))))
    )
)

;; id-base: Retorna l'id de la base segons l'equip
;; Paràmetres:
;;   celda - la cel·la de la base
;; Retorna: 1 si és base e1, 2 si és base e2
(defun id-base (celda)
    (cond ((pertany 'e1 celda) 1)
          (t 2)
    )
)

; ------------------------------------------------------------------



; ------------------------------------------------------------------
; decrementar cooldowns logica

;; decrementar-cooldowns: Decrementa els cooldowns de totes les unitats de l'equip actual
;; Paràmetres:
;;   mapa  - l'estat actual del mapa
;;   equip - l'equip que està jugant ('e1 o 'e2)
;; Retorna: el mapa amb els cooldowns decrementats
(defun decrementar-cooldowns (mapa equip)
  (cons (car mapa) ; mantenemos el estado
        (decrementar-filas (cdr mapa) equip)))

;; decrementar-filas: Decrementa cooldowns de totes les files del mapa
;; Paràmetres:
;;   mapa  - les files del mapa
;;   equip - l'equip actual
;; Retorna: les files amb cooldowns decrementats
(defun decrementar-filas (mapa equip)
  (cond
    ((null mapa) nil)
    (t (cons (decrementar-celdas (car mapa) equip)
             (decrementar-filas (cdr mapa) equip)))))

;; decrementar-celdas: Decrementa cooldowns de les cel·les d'una fila
;; Només afecta les bolles de l'equip actual
;; Paràmetres:
;;   fila  - la fila a processar
;;   equip - l'equip actual
;; Retorna: la fila amb cooldowns decrementats
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

;; comptar-labs: Compta els laboratoris de cada equip al mapa
;; Paràmetres:
;;   mapa - l'estat actual del mapa
;; Retorna: llista (labs-e1 labs-e2)
(defun comptar-labs (mapa)
    (list (compta-labs-e (cdr mapa) 'e1) (compta-labs-e (cdr mapa) 'e2)) ; cdr mapa para quitar los metadatos primeros
)

;; compta-labs-e: Compta els laboratoris d'un equip específic
;; Paràmetres:
;;   mapa - les files del mapa
;;   x    - l'equip a comptar ('e1 o 'e2)
;; Retorna: nombre de laboratoris de l'equip
(defun compta-labs-e (mapa x)
    (cond ((null mapa) 0)
          (t (+ (compta-labs-files (car mapa) x) (compta-labs-e (cdr mapa) x)))
    )    
)

;; compta-labs-files: Compta els laboratoris d'un equip en una fila
;; Paràmetres:
;;   fila - la fila a processar
;;   x    - l'equip a comptar
;; Retorna: nombre de laboratoris de l'equip en aquesta fila
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

;; fi-partida-mapa: Comprova si la partida ha acabat
;; Condicions: màxim de torns assolit o alguna base destruïda
;; Paràmetres:
;;   mapa - l'estat actual del mapa
;; Retorna: t si la partida ha acabat, nil altrament
(defun fi-partida-mapa (mapa)
    (cond
        ((>= (torn mapa) MAX-TORNS) t)
        ((not (te-base-al-mapa (celdas-mapa (cdr mapa)) 'e1)) t)
        ((not (te-base-al-mapa (celdas-mapa (cdr mapa)) 'e2)) t)
        (t nil)
    )
)

;; te-base-al-mapa: Comprova si existeix una base d'un equip al mapa
;; Paràmetres:
;;   celdas - llista plana de totes les cel·les del mapa
;;   equip  - l'equip a comprovar ('e1 o 'e2)
;; Retorna: t si existeix la base, nil altrament
(defun te-base-al-mapa (celdas equip)
    (cond
        ((null celdas) nil)
        ((and (equal (celda-tipus (car celdas)) 'base)
              (equal (celda-equip (car celdas)) equip)) t)
        (t (te-base-al-mapa (cdr celdas) equip))
    )
)

;; te-base: Comprova si una llista d'unitats té una base
;; Paràmetres:
;;   unitats-equip - llista d'unitats d'un equip
;; Retorna: t si hi ha una base, nil altrament
(defun te-base (unitats-equip)
    (cond ((null unitats-equip) nil)
          ((equal (car (cddddr (car unitats-equip))) 'base) t)
          (t (te-base (cdr unitats-equip)))))

;; mostra-guanyador: Determina i mostra el guanyador de la partida
;; Criteris: base destruïda > nombre de bolles > quantitat de pintura > aleatori
;; Paràmetres:
;;   mapa    - l'estat actual del mapa
;;   unitats - llista ((unitats-e1) (unitats-e2))
;; Retorna: 'e1 o 'e2 segons qui guanyi
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

;; compta-bolles: Compta les bolles vives d'un equip
;; Paràmetres:
;;   unitats-equip - llista d'unitats d'un equip
;; Retorna: nombre de bolles de l'equip
(defun compta-bolles (unitats-equip)
    (cond ((null unitats-equip) 0)
          ((equal (car (cddddr (car unitats-equip))) 'bolla)
           (+ 1 (compta-bolles (cdr unitats-equip))))
          (t (compta-bolles (cdr unitats-equip)))))

;------------------------------------------------------------------------------------------------      



;------------------------------------------------------------------------------------------------   
; funcions per construir l'array esta a partir del mapa

;; trobar-unitats-llista: Filtra les cel·les que són unitats d'un equip específic
;; Paràmetres:
;;   celdas - llista plana de cel·les
;;   mapa   - el mapa complet
;;   equip  - l'equip a filtrar ('e1 o 'e2)
;; Retorna: llista d'unitats de l'equip
(defun trobar-unitats-llista (celdas mapa equip)
    (cond ((null celdas) nil)
          ((and (es-unitat (car celdas))
                (equal (get-equip-celda (car celdas)) equip))
           (cons (celda-a-unitat (car celdas) mapa)
                 (trobar-unitats-llista (cdr celdas) mapa equip)))
          (t (trobar-unitats-llista (cdr celdas) mapa equip))))

;; es-unitat: Comprova si una cel·la conté una unitat (base o bolla)
;; Paràmetres:
;;   celda - la cel·la a comprovar
;; Retorna: t si és una unitat, nil altrament
(defun es-unitat (celda)
    (cond ((pertany 'base celda) t)
          ((pertany 'bolla celda) t)
          (t nil)))

;; trobar-unitats: Retorna totes les unitats del mapa separades per equip
;; Paràmetres:
;;   mapa - l'estat actual del mapa
;; Retorna: llista ((unitats-e1) (unitats-e2))
(defun trobar-unitats (mapa)
    (let* ((celdas (celdas-mapa (cdr mapa))))
        (list
            (trobar-unitats-llista celdas mapa 'e1)
            (trobar-unitats-llista celdas mapa 'e2))))

;; celdas-mapa: Aplana el mapa en una llista de cel·les per facilitar cerques
;; Paràmetres:
;;   mapa - les files del mapa
;; Retorna: llista plana de totes les cel·les
(defun celdas-mapa (mapa)
    (cond ((null mapa) nil)
          (t (append (car mapa) (celdas-mapa (cdr mapa))))
    )
)

;; celda-a-unitat: Construeix l'estructura de dades d'una unitat a partir de la cel·la
;; Format: (ronda equip pintura id tipus-unitat coord colors-pintat color-propi tr-pintar tr-moure visio)
;; Paràmetres:
;;   celda - la cel·la amb la unitat
;;   mapa  - l'estat actual del mapa
;; Retorna: llista amb tota la informació de la unitat
(defun celda-a-unitat (celda mapa)
    (let* ((tipus (celda-tipus celda))
           (equip (celda-equip celda))
           (pintura (cond ((equal equip 'e1) (cadr (car mapa)))
                          (t (caddr (car mapa)))))
           (base (equal tipus 'base)))
        (list
            (car (car mapa))                                        ; 1. ronda
            equip                                                   ; 2. equip
            pintura                                                 ; 3. pintura
            (cond (base (celda-id-base celda))                      ; 4. id
                  (t (celda-id-bolla celda)))
            tipus                                                   ; 5. tipus-unitat
            (cond (base (coord-amb-desplacament (celda-coord-base celda) mapa))  ; 6. coord
                  (t (coord-amb-desplacament (celda-coord-bolla celda) mapa)))
            (cond (base (celda-colors-pintat-base celda))           ; 7. colors-pintat
                  (t (celda-colors-pintat-bolla celda)))
            (cond (base nil)                                        ; 8. color-propi
                  (t (celda-color-propi-bolla celda)))
            (cond (base nil)                                        ; 9. tr-pintar
                  (t (celda-tr-pintar-bolla celda)))
            (cond (base nil)                                        ; 10. tr-moure
                  (t (celda-tr-moure-bolla celda)))
            (calcular-visio (celda-coord celda) tipus mapa))))      ; 11. visio

;; celda-a-visio: Converteix una cel·la a l'estructura de visió per la IA
;; Paràmetres:
;;   celda - la cel·la a convertir
;;   mapa  - l'estat actual del mapa
;; Retorna: llista amb la informació visible de la cel·la
(defun celda-a-visio (celda mapa)
    (let* ((tipus (car celda)))
        (cond
            ; Aigua: només coordenada i tipus
            ((equal tipus 'aigua)
             (list (coord-amb-desplacament (celda-coord celda) mapa) 'aigua))
            ; Terra
            (t (let* ((color (cadr celda))
                      (element (caddr celda)))
                (cond
                    ; Terra buida
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

;; calcular-visio: Calcula totes les cel·les visibles per una unitat
;; Paràmetres:
;;   coord-real - coordenada real (x y) de la unitat al mapa
;;   tipus      - 'base o 'bolla
;;   mapa       - l'estat actual del mapa
;; Retorna: llista de cel·les visibles en format de visió
(defun calcular-visio (coord-real tipus mapa)
    (let* ((rang (cond ((equal tipus 'base) 64)
                       (t 20)))
           (celdas (celdas-mapa (cdr mapa)))
           (celdas-visibles (celdas-en-rang coord-real rang celdas)))
        (mapcar (lambda (c) (celda-a-visio c mapa)) celdas-visibles)))

;; celdas-en-rang: Retorna totes les cel·les dins del rang de visió
;; Paràmetres:
;;   coord  - coordenada real (x y) de la unitat
;;   rang   - rang màxim de visió (20 per bolla, 64 per base)
;;   celdas - llista plana de totes les cel·les del mapa
;; Retorna: llista de cel·les dins del rang
(defun celdas-en-rang (coord rang celdas)
    (cond ((null celdas) nil)
          ((<= (d2 coord (celda-coord (car celdas))) rang)
           (cons (car celdas) (celdas-en-rang coord rang (cdr celdas))))
          (t (celdas-en-rang coord rang (cdr celdas)))))

;; d2: Calcula la distància euclidiana al quadrat entre dues coordenades
;; Paràmetres:
;;   coord-a - llista (x y)
;;   coord-b - llista (x y)
;; Retorna: distància^2
(defun d2 (coord-a coord-b)
    (+ (* (- (car coord-a) (car coord-b)) (- (car coord-a) (car coord-b)))
       (* (- (cadr coord-a) (cadr coord-b)) (- (cadr coord-a) (cadr coord-b)))))

;------------------------------------------------------------------------------------------------   



;----------------------------------------------------------------------------------------
;Per treballar amb el desplazament

;; coord-amb-desplacament: Suma el desplaçament global (dx, dy) a una coordenada
;; Paràmetres:
;;   coord - coordenada real (x y)
;;   mapa  - l'estat actual del mapa
;; Retorna: coordenada amb desplaçament (x+dx, y+dy)
(defun coord-amb-desplacament (coord mapa)
    (list (+ (car coord) (estat-dx mapa))
          (+ (cadr coord) (estat-dy mapa))
    )
)

;; coord-real: Resta el desplaçament global (dx, dy) d'una coordenada
;; Paràmetres:
;;   coord - coordenada amb desplaçament (x y)
;;   mapa  - l'estat actual del mapa
;; Retorna: coordenada real (x-dx, y-dy)
(defun coord-real (coord mapa)
    (list (- (car coord) (estat-dx mapa))
          (- (cadr coord) (estat-dy mapa))
    )
)

;------------------------------------------------------------------------------



;-----------------------------------------------------------------------------------------
; Funcions accessores per obtenir elements específics del mapa i les cel·les
; Aquestes funcions simplifica l'accés als diferents camps de l'estructura de dades


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
(defun celda-coord-base (celda) (caddr (cddr (cddr celda))))
(defun celda-coord-bolla (celda) (cadr (cddddr (cddr (cddr celda)))))
(defun celda-coord (celda) (car (reverse celda)))
(defun get-equip-celda (celda) (cadddr celda))
(defun torn (mapa) (car (car mapa)))
(defun equip-actual (mapa) (cond ((= (mod (torn mapa) 2) 0) 'e2) (t 'e1)))

;; funcio de clase
(defun pertany (x l)
    (cond ((null l) nil)
          ((equal x (car l)) t)
          (t (pertany x (cdr l)))))

;-----------------------------------------------------------------------------------------