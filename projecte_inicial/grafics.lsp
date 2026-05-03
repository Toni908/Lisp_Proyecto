;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: Antonio Garcia Font.
;; Professor: Miquel Cabot.
;; Assignatura: 21721 – Llenguatges de Programació.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.

;; == Descripció general ==
;; Aquest archiu te la funcio principal pinta, que pinta un estat del mapa, la qual
;; es pasa per parametre.
;;
;; == Dades Modificables ==
;; -  La dada modificable mes important es mida, a la linea 42, definira la mida del
;;    costat dels quadrats, tot depen d'allo, aquesta mida pot ser modificable
;;    per a poder veure els mapes mes grosos.

;; pinta: Genera la interfície visual principal.
;; Paràmetres:
;;     mapa - l'estructura de dades completa de l'estat del joc.
(defun pinta (mapa)
    (cls)
    (color 0 0 0)
    
    ; Cabecera con info
    (princ "Ronda: ")
    (princ (car (car mapa)))
    
    (move 30 80)
    (princ " | Turn de Equip: ")
    (print (equip-actual mapa))
    
    ; Leyenda equipos
    (princ "Pintura e1: ")
    (princ (cadr (car mapa)))
    
    (princ " | Pintura e2: ")
    (print (caddr (car mapa)))

    (print "Flecha dreta: Seguent Torn")
    (print "Flecha adalt: Acelerar 10 Torn")
    (print "Flecha esquerra: Autoavanzar")
    (print "Flecha abaix: Apagar Joc")
    
    ; Mapa
    (imprimir-files (cdr mapa) 0 10) ;; mapa fila mida, la mida es important, per a mapas grosos deuria ser 6 tot sol pot ser par el numero
    (color 0 0 0)
)

;; imprimir-files: Itera verticalment per dibuixar cada fila del mapa.
;; Paràmetres:
;;     mapa - el mapa sense metadatos
;;     fila - índex de la fila actual (per al multiplicador de fila)
;;     mida - dimensió quadrat.
(defun imprimir-files (mapa fila mida)
    (cond
        ((null mapa) nil)
        (t
            (move 265 (+ 10 (* fila mida)))
            (imprimir-fila (car mapa) 0 mida)
            (imprimir-files (cdr mapa) (+ fila 1) mida)
        )
    )
)

;; imprimir-fila: Itera horitzontalment per dibuixar cada cel·la d'una fila.
;; Paràmetres:
;;     fila     - llista d'elements (caselles) de la fila actual.
;;     columna  - índex de la columna actual.
;;     mida     - dimensió quadrat.
(defun imprimir-fila (fila columna mida)
    (cond
        ((null fila) nil)
        (t
            (moverel mida 0)
            (pinta-terreno (car fila) mida)
            (pinta-unidad (car fila) mida)
            (pinta-marca (car fila) mida)
            (quadrat mida)
            (imprimir-fila (cdr fila) (+ columna 1) mida)
        )
    )
)

;; pinta-marca: Determina si una casella ha de mostrar marques de pintura.
;; Paràmetres:
;;     casella - dades de la casella actual.
;;     mida    - dimensió quadrat.
(defun pinta-marca (casella mida)
    (moverel (/ mida 2) (/ mida 2))
    (cond ( (pertany 'base casella)
            (marcar (cadddr (cdr casella))) 
          )
          ( (pertany 'bolla casella)
            (marcar (cadddr (cdr (cdr casella)))) 
          )
          (t nil)
    )
    (moverel (- (/ mida 2)) (- (/ mida 2)))
)

;; marcar: Dibuixa línies de colors segons les marques actives.
;; Paràmetres:
;;     marcas - llista de símbols de pintura ('b, 'r, 'g).
(defun marcar (marcas)
    (cond ((contiene marcas 'b) 
           (color 80 120 210)
           (linea)
          )
          (t t))
    (cond ((contiene marcas 'r)
           (color 210 80 80)
           (moverel 0 2)
           (linea)
           (moverel 0 -2)
          )
          (t t))
    (cond ((contiene marcas 'g)
            (color 80 185 80)
            (moverel 0 -2)
            (linea)
            (moverel 0 2)
          )
          (t t))
)

;; linea: Funció auxiliar gràfica per dibuixar un traç horitzontal.
(defun linea ()
    (drawrel -2 0)
    (drawrel 5 0)
    (moverel -3 0)
)

;; contiene: Funció lògica de cerca en llistes.
;; Paràmetres:
;;     l - llista on buscar.
;;     e - element a trobar.
(defun contiene (l e)
    (cond ((null l) nil)
          ((equal (car l) e) t)
          (t (contiene (cdr l) e))
    )
)

;; pinta-unidad: Selecciona la forma geomètrica segons l'objecte.
;; Paràmetres:
;;     casella - dades de la casella actual.
;;     mida    - dimensió quadrat.
(defun pinta-unidad (casella mida)
    (cond
        ((equal (caddr casella) 'lab) (triangle mida))
        ((equal (caddr casella) 'bolla) (cercle casella mida))
        (t )
    )
)

;; pinta-terreno: Emplena el fons de la casella línia a línia.
;; Paràmetres:
;;     casella - dades de la casella per obtenir el color.
;;     mida    - dimensió quadrat.
;;     counter - comptador recursiu per a l'alçada.
(defun pinta-terreno (casella mida &optional (counter mida))
    (cond
        ((= counter 0)
            (moverel 0 (- mida)))
        (t
            (color-casella casella)
            (drawrel mida 0)
            (moverel (- mida) 1)
            (pinta-terreno casella mida (- counter 1))
        )
    )
)

;; color-casella: Assigna el color RGB segons l'estat del terreny/base/aigua.
;; Paràmetres:
;;     casella - llista de dades de la cel·la.
(defun color-casella (casella)
    (cond
        ((equal (caddr casella) 'base)
            (cond
                ((equal (cadddr casella) 'e1) (color 255 0 255))
                (t (color 255 255 0))
            )
        )
        ((equal (caddr casella) 'lab)
            (cond
                ((equal (cadddr casella) 'e1) (color 255 0 255))
                ((equal (cadddr casella) 'e2) (color 255 255 0))
                (t (color 0 0 0))
            )
        )
        ((equal (car casella) 'aigua) (color 42 255 255))
        ((equal (cadr casella) 'b) (color 80 120 210))
        ((equal (cadr casella) 'r) (color 210 80 80))
        ((equal (cadr casella) 'g) (color 80 185 80))
        (t (color 211 211 211))
    )
)

;; color-unidad: Defineix el color de la unitat segons la seva pròpia marca.
;; Paràmetres:
;;     casella - dades de la casella que conté la unitat.
(defun color-unidad (casella)
    (cond
        ((equal (cadddr (cdr casella)) 'b) (color 80 120 210))
        ((equal (cadddr (cdr casella)) 'r) (color 210 80 80))
        ((equal (cadddr (cdr casella)) 'g) (color 80 185 80))
        (t t)
    )
)

;; quadrat: Dibuixa el perímetre negre d'una casella.
;; Paràmetres:
;;     mida - dimensió quadrat.
(defun quadrat (mida)
    (color 0 0 0)
    (drawrel 0 mida)
    (drawrel mida 0)
    (drawrel 0 (- mida))
    (drawrel (- mida) 0)
)

;; cercle: Dibuixa una unitat 'bolla' (rombe) i la seva vora d'equip.
;; Paràmetres:
;;     casella - dades de la cel·la.
;;     mida    - dimensió quadrat.
(defun cercle (casella mida)
    (color-unidad casella)
    (moverel (/ mida 2) 0)
    (cercle-fill mida)

    (color-equip casella)
    (drawrel (/ mida 2) (/ mida 2))
    (drawrel (- (/ mida 2)) (/ mida 2))
    (drawrel (- (/ mida 2)) (- (/ mida 2)))
    (drawrel (/ mida 2) (- (/ mida 2)))

    (moverel (- (/ mida 2)) 0)
)

;; color-equip: Assigna color al traç de l'equip (negre o groc).
;; Paràmetres:
;;     casella - celda a pintar
(defun color-equip (casella)
    (cond ((equal (celda-equip casella) 'e1) (color 0 0 0))  
          (t (color 255 255 0))
    )
)

;; cercle-fill: Emplena l'interior del rombe de la unitat.
;; Paràmetres:
;;     mida    - dimensió quadrat.
;;     counter - comptador per a les línies d'emplenat.
(defun cercle-fill (mida &optional (counter mida))
    (cond
        ((= counter 0) 
            (moverel (/ mida 2) (- (/ mida 2)))
        )
        (t
            (drawrel (/ mida 2) (/ mida 2))
            (moverel (- (/ mida 2)) (- (/ mida 2)))
            (cond
                ((= (mod counter 2) 0) (moverel 0 1)) 
                (t (moverel (- 1) 0))
            )
            
            (cercle-fill mida (- counter 1))
        )
    )
)

;; triangle: Dibuixa la representació visual del laboratori, triangles.
;; Paràmetres:
;;     mida - dimensió quadrat.
(defun triangle (mida)
    (color 255 255 255)
    (triangle-fill mida 0)
    (moverel 0 (- mida))
)

;; triangle-fill: Funció recursiva per emplenar el triangle.
;; Paràmetres:
;;   mida - dimensió quadrat.
;;   y    - posició vertical actual de l'emplenat.
(defun triangle-fill (mida y)
    (cond
        ((= y mida) nil)
        (t
            (drawrel (- mida y) 0)
            (moverel (- (- mida y)) 1)
            (triangle-fill mida (+ y 1))
        )
    )
)