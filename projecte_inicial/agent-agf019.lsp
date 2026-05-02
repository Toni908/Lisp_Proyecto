;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiant: Antonio Garcia Font.
;; Professor: Miquel Cabot.
;; Assignatura: 21721 – Llenguatges de Programació.
;; Convocatòria: primera convocatòria.
;; Fitxer de l'agent intel·ligent AGF019.
;;
;; == Descripció general ==
;; Aquest agent implementa la lògica per a les unitats de l'equip e1.
;; - BASE: Crea bolles sempre que tingui >= 50 de pintura. Crea aleatoriament
;;   el color de la bolla. Escull la casella adjacent lliure més propera.
;; - BOLLA: Es mou en una direcció aleatoria fins que troba un objectiu, pinta el sol sempre que pot al moure-se. 
;;   Si detecta un laboratori sense capturar o enemic, o una bolla/base enemiga
;;   sense el seu color, s'hi dirigeix per pintar-la.
;;
;; == Estructura de dades rebuda (dades) ==
;;   1. ronda        - nombre de torn (enter)
;;   2. equip        - 'e1 o 'e2
;;   3. pintura      - pintura disponible de l'equip (enter)
;;   4. id           - identificador de la unitat
;;   5. tipus-unitat - 'base o 'bolla
;;   6. coordenada   - (x y) amb desplaçament
;;   7. colors-pintat- llista de colors pintats a la unitat
;;   8. color-propi  - color de la bolla (nil si base)
;;   9. tr-pintar    - temps de recuperació de pintar (nil si base)
;;  10. tr-moure     - temps de recuperació de moure (nil si base)
;;  11. visio        - llista de caselles visibles
;;
;; == Format visió ==
;;   Casella buida:  (coord 'terra color)
;;   Casella lab:    (coord 'terra color 'lab equip)
;;   Casella base:   (coord 'terra color 'base equip colors-pintat nil nil nil)
;;   Casella bolla:  (coord 'terra color 'bolla equip colors-pintat color-propi tr-pintar tr-moure)

(setq rs (make-random-state t)) ;; Inicialització de l'estat aleatori

;; ------------------------------------------------------------
;; Punt d'entrada Principal

;; agent-agf019: funció principal que el controlador crida per cada unitat
;; Paràmetres:
;;   dades - llista amb tota la informació que la unitat pot saber
;; Retorna: llista d'accions a dur a terme
(defun agent-agf019 (dades)
    (let* ((tipus (agent-agf019-tipus dades)))
        (cond
            ((equal tipus 'base) (agent-agf019-accions-base dades))
            ((equal tipus 'bolla) (agent-agf019-accions-bolla dades))
            (t nil)
        )
    )
)


;; ------------------------------------------------------------
;; Dades de la unitat

;; agent-agf019-ronda: retorna la ronda actual
(defun agent-agf019-ronda (dades) (car dades))

;; agent-agf019-equip: retorna l'equip ('e1 o 'e2)
(defun agent-agf019-equip (dades) (cadr dades))

;; agent-agf019-pintura: retorna la pintura disponible de l'equip
(defun agent-agf019-pintura (dades) (caddr dades))

;; agent-agf019-id: retorna l'id de la unitat
(defun agent-agf019-id (dades) (cadddr dades))

;; agent-agf019-tipus: retorna el tipus de la unitat ('base o 'bolla)
(defun agent-agf019-tipus (dades) (car (cddddr dades)))

;; agent-agf019-coord: retorna la coordenada (x y) de la unitat
(defun agent-agf019-coord (dades) (cadr (cddddr dades)))

;; agent-agf019-colors-pintat: retorna la llista de colors pintats a la unitat
(defun agent-agf019-colors-pintat (dades) (caddr (cddddr dades)))

;; agent-agf019-color-propi: retorna el color propi de la bolla (nil si base)
(defun agent-agf019-color-propi (dades) (cadddr (cddddr dades)))

;; agent-agf019-tr-pintar: retorna el tr-pintar (nil si base)
(defun agent-agf019-tr-pintar (dades) (car (cddddr (cddddr dades))))

;; agent-agf019-tr-moure: retorna el tr-moure (nil si base)
(defun agent-agf019-tr-moure (dades) (cadr (cddddr (cddddr dades))))

;; agent-agf019-visio: retorna la llista de caselles visibles
(defun agent-agf019-visio (dades) (caddr (cddddr (cddddr dades))))


;; ------------------------------------------------------------
;; Dades de la visio

;; agent-agf019-cas-coord: coordenada d'una casella de la visió
(defun agent-agf019-cas-coord (cas) (car cas))

;; agent-agf019-cas-tipus-casella: tipus de casella ('terra o 'aigua)
(defun agent-agf019-cas-tipus-casella (cas) (cadr cas))

;; agent-agf019-cas-color-casella: color del sòl de la casella
(defun agent-agf019-cas-color-casella (cas) (caddr cas))

;; agent-agf019-cas-tipus-element: tipus d'element a la casella (nil, 'lab, 'base, 'bolla)
(defun agent-agf019-cas-tipus-element (cas)
    (cond ((>= (agent-agf019-longitud cas) 4) (cadddr cas))
          (t nil)))

;; agent-agf019-cas-equip: equip que controla l'element (nil si no hi ha o lab no capturat)
(defun agent-agf019-cas-equip (cas)
    (cond ((>= (agent-agf019-longitud cas) 5) (car (cddddr cas)))
          (t nil)))

;; agent-agf019-cas-colors-pintat: colors pintats de l'element
(defun agent-agf019-cas-colors-pintat (cas)
    (cond ((>= (agent-agf019-longitud cas) 6) (cadr (cddddr cas)))
          (t nil)))

;; agent-agf019-cas-color-propi-bolla: color propi de la bolla
(defun agent-agf019-cas-color-propi-bolla (cas)
    (cond ((>= (agent-agf019-longitud cas) 7) (caddr (cddddr cas)))
          (t nil)))


;; ------------------------------------------------------------
;; Funcions auxiliars generals

;; agent-agf019-longitud: longitud d'una llista
;; Paràmetres:
;;   l - la llista
(defun agent-agf019-longitud (l)
    (cond ((null l) 0)
          (t (+ 1 (agent-agf019-longitud (cdr l))))))

;; agent-agf019-pertany: comprova si x pertany a la llista l
;; Paràmetres:
;;   x - element a cercar
;;   l - la llista
(defun agent-agf019-pertany (x l)
    (cond ((null l) nil)
          ((equal x (car l)) t)
          (t (agent-agf019-pertany x (cdr l)))))

;; agent-agf019-d2: distància euclidiana al quadrat entre dues coordenades
;; Paràmetres:
;;   a - coordenada (x y)
;;   b - coordenada (x y)
(defun agent-agf019-d2 (a b)
    (+ (* (- (car a) (car b)) (- (car a) (car b)))
       (* (- (cadr a) (cadr b)) (- (cadr a) (cadr b)))))

;; agent-agf019-color-seguent: alterna el color segons la ronda i l'id
;; Estratègia: usa (ronda + id) mod 3 per variar el color entre creacions
;; Paràmetres:
;;   ronda - el torn actual
;;   id    - l'id de la base
(defun agent-agf019-color-seguent (ronda id)
    (let* ((n (random 3 rs)))
        (cond ((= n 0) 'r)
              ((= n 1) 'g)
              (t 'b))))


;; ----------------------------------------------------------
;; Lògica de la base

;; agent-agf019-accions-base: retorna les accions de la base
;; La base crea una bolla si té >= 50 de pintura i hi ha espai adjacent.
;; El color s'alterna per no repetir.
;; Paràmetres:
;;   dades - informació de la unitat base
(defun agent-agf019-accions-base (dades)
    (let* ((pintura  (agent-agf019-pintura dades))
           (coord    (agent-agf019-coord dades))
           (ronda    (agent-agf019-ronda dades))
           (id       (agent-agf019-id dades))
           (visio    (agent-agf019-visio dades))
           (color    (agent-agf019-color-seguent ronda id))
           (dest     (agent-agf019-cercar-lliure coord visio)))
        (cond
            ((< pintura 50) nil)
            ((null dest) nil)
            (t (list (list 'crea-bolla (list color dest))))
        )
    )
)

;; agent-agf019-cercar-lliure: itera la visió buscant una casella adjacent lliure
;; Paràmetres:
;;   coord - coordenada origen
;;   visio - llista de caselles visibles
(defun agent-agf019-cercar-lliure (coord visio)
    (cond
        ((null visio) nil)
        (t
            (let* ((cas (car visio))
                   (cas-coord (agent-agf019-cas-coord cas))
                   (dist (agent-agf019-d2 coord cas-coord))
                   (tipus-cas (agent-agf019-cas-tipus-casella cas))
                   (tipus-elem (agent-agf019-cas-tipus-element cas)))
                (cond
                    ((and (<= dist 2)
                          (> dist 0)
                          (equal tipus-cas 'terra)
                          (null tipus-elem))
                     cas-coord)
                    (t (agent-agf019-cercar-lliure coord (cdr visio)))
                )
            )
        )
    )
)


;; ------------------------------------------------------------
;; Lògica de la bolla

;; agent-agf019-accions-bolla: retorna les accions de la bolla
;; Lògica:
;;  1. Si hi ha un objectiu prioritari visible (lab no propi, bolla enemiga,
;;     base enemiga sense el color propi), s'hi dirigeix i pinta.
;;  2. Si no, es mou en la seva direcció natural (basada en id i ronda).
;;  3. Abans de moure's, pinta el sòl destí si no és del seu color.
;; Paràmetres:
;;   dades - informació de la unitat bolla
(defun agent-agf019-accions-bolla (dades)
    (let* ((coord       (agent-agf019-coord dades))
           (color-propi (agent-agf019-color-propi dades))
           (tr-pintar   (agent-agf019-tr-pintar dades))
           (tr-moure    (agent-agf019-tr-moure dades))
           (equip       (agent-agf019-equip dades))
           (visio       (agent-agf019-visio dades))
           (id          (agent-agf019-id dades))
           (ronda       (agent-agf019-ronda dades))
           (objectiu    (agent-agf019-cerca-objectiu coord color-propi equip visio)))
        (cond
            ;; Hi ha objectiu prioritari: pintar-lo si podem, i moure'ns cap a ell
            ((not (null objectiu))
             (agent-agf019-accions-vers-objectiu coord objectiu color-propi tr-pintar tr-moure visio))
            ;; Sense objectiu: moviment normal + pintar sòl per evitar penalització
            (t
             (agent-agf019-accions-moviment-normal coord color-propi tr-pintar tr-moure id ronda visio))
        )
    )
)

;; agent-agf019-cerca-objectiu: busca el millor objectiu a la visió
;; Prioritat: base enemiga > bolla enemiga > lab enemic o sense capturar
;; Retorna la coordenada de l'objectiu o nil
;; Paràmetres:
;;   coord       - coordenada de la bolla
;;   color-propi - color propi de la bolla
;;   equip       - equip de la bolla
;;   visio       - llista de caselles visibles
(defun agent-agf019-cerca-objectiu (coord color-propi equip visio)
    (let* ((base-enem   (agent-agf019-cerca-base-enemiga coord equip color-propi visio))
           (bolla-enem  (agent-agf019-cerca-bolla-enemiga coord equip color-propi visio))
           (lab-obert   (agent-agf019-cerca-lab coord equip visio)))
        (cond
            ((not (null base-enem))  base-enem)
            ((not (null bolla-enem)) bolla-enem)
            ((not (null lab-obert))  lab-obert)
            (t nil)
        )
    )
)

;; agent-agf019-cerca-base-enemiga: cerca la base enemiga que no té el color propi pintat
;; Paràmetres:
;;   coord       - coordenada de la bolla
;;   equip       - equip de la bolla
;;   color-propi - color propi de la bolla
;;   visio       - caselles visibles
(defun agent-agf019-cerca-base-enemiga (coord equip color-propi visio)
    (cond
        ((null visio) nil)
        (t
            (let* ((cas        (car visio))
                   (tipus-elem (agent-agf019-cas-tipus-element cas))
                   (equip-cas  (agent-agf019-cas-equip cas))
                   (colors-p   (agent-agf019-cas-colors-pintat cas))
                   (cas-coord  (agent-agf019-cas-coord cas)))
                (cond
                    ((and (equal tipus-elem 'base)
                          (not (equal equip-cas equip))
                          (not (agent-agf019-pertany color-propi colors-p)))
                     cas-coord)
                    (t (agent-agf019-cerca-base-enemiga coord equip color-propi (cdr visio)))
                )
            )
        )
    )
)

;; agent-agf019-cerca-bolla-enemiga: cerca una bolla enemiga que no tingui el color propi
;; Paràmetres:
;;   coord       - coordenada de la bolla
;;   equip       - equip de la bolla
;;   color-propi - color propi de la bolla
;;   visio       - caselles visibles
(defun agent-agf019-cerca-bolla-enemiga (coord equip color-propi visio)
    (cond
        ((null visio) nil)
        (t
            (let* ((cas        (car visio))
                   (tipus-elem (agent-agf019-cas-tipus-element cas))
                   (equip-cas  (agent-agf019-cas-equip cas))
                   (colors-p   (agent-agf019-cas-colors-pintat cas))
                   (cas-coord  (agent-agf019-cas-coord cas)))
                (cond
                    ((and (equal tipus-elem 'bolla)
                          (not (equal equip-cas equip))
                          (not (agent-agf019-pertany color-propi colors-p)))
                     cas-coord)
                    (t (agent-agf019-cerca-bolla-enemiga coord equip color-propi (cdr visio)))
                )
            )
        )
    )
)

;; agent-agf019-cerca-lab: cerca un laboratori no capturat o capturat per l'enemic
;; Paràmetres:
;;   coord - coordenada de la bolla
;;   equip - equip de la bolla
;;   visio - caselles visibles
(defun agent-agf019-cerca-lab (coord equip visio)
    (cond
        ((null visio) nil)
        (t
            (let* ((cas        (car visio))
                   (tipus-elem (agent-agf019-cas-tipus-element cas))
                   (equip-cas  (agent-agf019-cas-equip cas))
                   (cas-coord  (agent-agf019-cas-coord cas)))
                (cond
                    ((and (equal tipus-elem 'lab)
                          (not (equal equip-cas equip)))
                     cas-coord)
                    (t (agent-agf019-cerca-lab coord equip (cdr visio)))
                )
            )
        )
    )
)

;; agent-agf019-accions-vers-objectiu: genera accions per anar cap a un objectiu
;; Si l'objectiu està a rang de pintura (d2<=5) i podem pintar, el pintem.
;; Després intentem moure'ns cap a ell.
;; Paràmetres:
;;   coord       - coordenada actual de la bolla
;;   objectiu    - coordenada de l'objectiu
;;   color-propi - color propi de la bolla
;;   tr-pintar   - temps de recuperació de pintar
;;   tr-moure    - temps de recuperació de moure
;;   visio       - caselles visibles
(defun agent-agf019-accions-vers-objectiu (coord objectiu color-propi tr-pintar tr-moure visio)
    (let* ((dist (agent-agf019-d2 coord objectiu))
           (pot-pintar (< tr-pintar 1))
           (pot-moure  (< tr-moure 1.0))
           (dest-mov   (agent-agf019-pas-cap-a coord objectiu))
           (cas-dest   (agent-agf019-buscar-casella dest-mov visio))
           (color-dest (cond ((null cas-dest) nil)
                             (t (agent-agf019-cas-color-casella cas-dest)))))
        (cond
            ;; A rang de pintura: pintem l'objectiu
            ((and (<= dist 5) pot-pintar)
             (cond
                 ;; Si a més podem moure, pintem + movem
                 ((and pot-moure (not (null dest-mov)) (not (null cas-dest))
                    (equal (agent-agf019-cas-tipus-casella cas-dest) 'terra)
                    (null (agent-agf019-cas-tipus-element cas-dest)))
                  (cond
                      ;; El sòl destí no és del nostre color: pintem el suelo primer
                      ((and (not (equal color-dest color-propi))
                            (agent-agf019-d2 coord dest-mov) )
                       (list (list 'pinta objectiu) (list 'pinta dest-mov) (list 'mou dest-mov)))
                      (t
                       (list (list 'pinta objectiu) (list 'mou dest-mov)))
                  ))
                 (t (list (list 'pinta objectiu)))
             ))
            ;; Fora de rang: ens movem cap a l'objectiu (pintant el suelo si cal)
            ;; Si el pas directe esta bloquejat, fem moviment normal (exploracio)
            (pot-moure
             (cond
                 ((and (not (null dest-mov)) (not (null cas-dest))
                       (equal (agent-agf019-cas-tipus-casella cas-dest) 'terra)
                       (null (agent-agf019-cas-tipus-element cas-dest)))
                  (cond
                      ((and pot-pintar (not (equal color-dest color-propi))
                            (<= (agent-agf019-d2 coord dest-mov) 5))
                       (list (list 'pinta dest-mov) (list 'mou dest-mov)))
                      (t (list (list 'mou dest-mov)))
                  ))
                 (t (agent-agf019-accions-moviment-normal
                        coord color-propi tr-pintar tr-moure nil nil visio))
             ))
            (t nil)
        )
    )
)

;; agent-agf019-totes-direccions: retorna les 8 direccions starting from a random offset
(defun agent-agf019-totes-direccions ()
    (let* ((offset (random 8 rs)))
        (agent-agf019-rotar-llista offset
            (list
                (list  1  0) (list  1  1) (list  0  1) (list -1  1)
                (list -1  0) (list -1 -1) (list  0 -1) (list  1 -1)
            )
        )
    )
)

;; agent-agf019-rotar-llista: rota una llista n posicions cap a l'esquerra
(defun agent-agf019-rotar-llista (n l)
    (cond ((= n 0) l)
          (t (agent-agf019-rotar-llista
                (- n 1)
                (append (cdr l) (list (car l)))
              )
          )
    )
)

;; agent-agf019-accions-moviment-normal: prova les 8 direccions sistemàticament
;; començant per una aleatòria
(defun agent-agf019-accions-moviment-normal (coord color-propi tr-pintar tr-moure id ronda visio)
    (let* ((pot-moure  (< tr-moure 1.0))
           (pot-pintar (< tr-pintar 1))
           (dirs       (agent-agf019-totes-direccions)))
        (cond
            ((not pot-moure) nil)
            (t (agent-agf019-prova-direccions coord dirs color-propi pot-pintar visio))
        )
    )
)

;; agent-agf019-prova-direccions: itera les direccions fins trobar una casella vàlida
(defun agent-agf019-prova-direccions (coord dirs color-propi pot-pintar visio)
    (cond
        ((null dirs) nil)
        (t
            (let* ((dest     (agent-agf019-aplica-direccio coord (car dirs)))
                   (cas-dest (agent-agf019-buscar-casella dest visio))
                   (resultat (agent-agf019-generar-moviment coord dest cas-dest color-propi pot-pintar)))
                (cond
                    ((not (null resultat)) resultat)
                    (t (agent-agf019-prova-direccions coord (cdr dirs) color-propi pot-pintar visio))
                )
            )
        )
    )
)

;; agent-agf019-generar-moviment: genera la llista d'accions per moure's a dest
;; Pinta el sòl si no és del color propi (evita penalització x3).
;; Paràmetres:
;;   coord       - coordenada actual
;;   dest        - coordenada destí
;;   cas-dest    - casella destí (de la visió)
;;   color-propi - color propi de la bolla
;;   pot-pintar  - booleà: pot pintar?
(defun agent-agf019-generar-moviment (coord dest cas-dest color-propi pot-pintar)
    (cond
        ((null cas-dest) nil)
        ((not (equal (agent-agf019-cas-tipus-casella cas-dest) 'terra)) nil)
        ((not (null (agent-agf019-cas-tipus-element cas-dest))) nil)
        (t
            (let* ((color-sol (agent-agf019-cas-color-casella cas-dest))
                   (cal-pintar-sol (and pot-pintar
                                        (not (equal color-sol color-propi))
                                        (<= (agent-agf019-d2 coord dest) 5))
                   )
                  )
                (cond
                    (cal-pintar-sol (list (list 'pinta dest) (list 'mou dest)))
                    (t (list (list 'mou dest)))
                )
            )
        )
    )
)

;; agent-agf019-aplica-direccio: suma una direcció a una coordenada
;; Paràmetres:
;;   coord - coordenada (x y)
;;   dir   - vector (dx dy)
(defun agent-agf019-aplica-direccio (coord dir)
    (list (+ (car coord) (car dir))
          (+ (cadr coord) (cadr dir))))

;; agent-agf019-pas-cap-a: retorna la casella adjacent (d2<=2) que s'acosta més a l'objectiu
;; Paràmetres:
;;   origen   - coordenada (x y) de la bolla
;;   objectiu - coordenada (x y) de l'objectiu
(defun agent-agf019-pas-cap-a (origen objectiu)
    (let* ((dx (- (car objectiu) (car origen)))
           (dy (- (cadr objectiu) (cadr origen)))
           (sx (agent-agf019-signe dx))
           (sy (agent-agf019-signe dy)))
        (list (+ (car origen) sx)
              (+ (cadr origen) sy)
        )
    )
)

;; agent-agf019-signe: retorna -1, 0 o 1 segons el signe d'un nombre
;; Paràmetres:
;;   n - nombre enter
(defun agent-agf019-signe (n)
    (cond ((> n 0)  1)
          ((< n 0) -1)
          (t        0)
    )
)

;; agent-agf019-buscar-casella: busca una casella a la visió per coordenada
;; Paràmetres:
;;   coord - coordenada (x y) a cercar
;;   visio - llista de caselles visibles
(defun agent-agf019-buscar-casella (coord visio)
    (cond
        ((null visio) nil)
        ((equal coord (agent-agf019-cas-coord (car visio))) (car visio))
        (t (agent-agf019-buscar-casella coord (cdr visio)))
    )
)
