(defun pertany (x l)
  (cond ((null l) nil)
        ((equal x (car l)) t)
        (t (pertany x (cdr l)))))

(defun conjunt-correcte (l)
    (cond ((null l) t)
          ((pertany (car l) (cdr l)) nil)
          (t (conjunt-correcte (cdr l)))
          ))

(defun fer-conjunt (l)
    (cond ((null l) nil)
          ((pertany (car l) (cdr l)) (fer-conjunt (cdr l)))
          (t (cons (car l) (fer-conjunt (cdr l))))
          ))

(defun unio (l1 l2)
    (fer-conjunt (append l1 l2)))

(defun interseccio (l1 l2)
    (cond ((null l1) nil)
          ((pertany (car l1) l2) (cons (car l1) (interseccio (cdr l1) l2)))
          (t (interseccio (cdr l1) l2))
          ))

(defun diferencia (l1 l2)
    (cond ((null l1) nil)
          ((pertany (car l1) l2) (diferencia (cdr l1) l2))
          (t (cons (car l1) (diferencia (cdr l1) l2)))
          ))

(defun diferencia-simetrica (l1 l2)
    (unio (diferencia l1 l2) (diferencia l2 l1)))

(defun producte-cartesia (l1 l2)
    (cond ((null l1) nil)
          (t (append (fer-llistes (car l1) l2) (producte-cartesia (cdr l1) l2)))
          ))

(defun fer-llistes (a l)
    (cond ((null l) nil)
          (t (cons (list a (car l)) (fer-llistes a (cdr l))))
          ))