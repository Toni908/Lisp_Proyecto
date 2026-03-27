(defun m ()
    (cls)
    (move 150 100)
    )

(defun cercle (x y radi segments)
    (mover (+ x radi) y )
    (cercle2 x y radi (/ 360 segments) 0))

(defun cercle2 (x y radi pas angle)
    (cond ((< angle 360)
           (drawr (+ x (* radi (cos (radians (+ angle pas)))))
                  (+ y (* radi (sin (radians (+ angle pas))))))
           (cercle2 x y radi pas (+ angle pas)))
           (t t)))

(defun mover (x y)
    (move (round x)
          (round y)))

(defun drawr (x y)
    (draw (roundd x)
        )) #noacabado



