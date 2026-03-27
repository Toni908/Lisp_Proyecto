(defun esborra-si (f l)
    (cond ((null l) nil)
          ((funcall f (car l)) (esborra-si f (cdr l)))
          (t (cons (car l) (esborra-si f (cdr l))))
          ))

(defun es-parell (x)
    (= (mod x 2) 0)
    )

(defun meu-mapcar (f l)
    (cond ((null l) nil)
          (t (cons (funcall f (car l)) (meu-mapcar f (cdr l))))
          ))

(defun suma (x)
    (+ x 1)
    )

; funciones utiles para la practica

(setq fila '( (a 1) (b 2) (c 3) ))

(setq matriu '(( (a 1) (b 2) (c 3) )
               ( (d 4) (e 5) (f 6) )
               ( (g 7) (h 8) (i 9) )
              ))

(defun indexa-fila (fila c)
    (cond ((null fila) nil)
          ((= c 0) (car fila))
          (t (indexa-fila (cdr fila) (- c 1)))
          ))

(defun indexa-matriu (matriu f c)
    (cond ((null matriu) nil)
          ((= f 0) (indexa-fila (car matriu) c))
          (t (indexa-matriu (cdr matriu) (- f 1) c))
          ))

(defun posa-dins-fila (fila c valor)
    (cond ((null fila) nil)
          ((= c 0) (cons valor (cdr fila)))
          (t (cons (car fila) (posa-dins-fila (cdr fila) (- c 1) valor)))
          ))

(defun posa-dins-matriu (matriu f c valor)
    (cond ((null matriu) nil)
          ((= f 0) (cons (posa-dins-fila (car matriu) c valor) (cdr matriu)))
          (t (cons (car matriu) (posa-dins-matriu (cdr matriu) (- f 1) c valor)))
          ))

(defun troba-dins-fila (fila valor &optional (c 0))
    (cond ((null fila) nil)
          ((= valor (caar fila)) c)
          (t (troba-dins-fila (cdr fila) valor (+ c 1)))
          ))

(defun troba-dins-matriu (matriu valor &optional (f 0))
    (cond ((null matriu) nil)
          (t (let ((c (troba-dins-fila (car matriu) valor)))
              (cond (c (list f c)) 
                    (t (troba-dins-matriu (cdr matriu) valor (+ f 1)))
                    )))
          ))

; esto ya no es de la practica creo?

(defun )
