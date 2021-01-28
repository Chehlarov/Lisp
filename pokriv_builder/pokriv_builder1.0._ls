(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(defun pok:z (p)
  (setq	x1 (car p1)
	y1 (cadr p1)
  )
  (setq	x2 (car p2)
	y2 (cadr p2)
  )
  (setq	x (car p)
	y (cadr p)
  )
  (setq	A (abs (- (+ (* x1 y2) (* x y1) (* x2 y))
	     (+ (* x y2) (* x2 y1) (* x1 y))
	  ))
  )
  (setq osn (distance (list (car p1)(cadr p1)) (list (car p2)(cadr p2))))
  (setq h (/ A osn))
  (setq z (* h (/ (sin alpha) (cos alpha) )))
  z
  )

(defun c:zfix()
  ;(setq z 0)
  (setq p1 (getpoint "first point"))
  (setq p2 (getpoint "second point"))
  (setq alpha (getreal "enter inclement in degrees: "))
  (setq alpha (/ (* alpha pi)180))
  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj) )
	(if (= (cdr (assoc 0 objdat)) "3DFACE")
	  (progn
	    (setq objdat
		   (subst
		     (cons '10 (list (nth 1 (assoc '10 objdat)) (nth 2 (assoc '10 objdat)) (pok:z (list (nth 1 (assoc '10 objdat)) (nth 2 (assoc '10 objdat))) )))
		     (assoc '10 objdat)
		     objdat
		     )
		   )
	    (setq objdat
		   (subst
		     (cons '11 (list (nth 1 (assoc '11 objdat)) (nth 2 (assoc '11 objdat)) (pok:z (list (nth 1 (assoc '11 objdat)) (nth 2 (assoc '11 objdat))) )))
		     (assoc '11 objdat)
		     objdat
		     )
		   )
	    (setq objdat
		   (subst
		     (cons '12 (list (nth 1 (assoc '12 objdat)) (nth 2 (assoc '12 objdat)) (pok:z (list (nth 1 (assoc '12 objdat)) (nth 2 (assoc '12 objdat))) )))
		     (assoc '12 objdat)
		     objdat
		     )
		   )
	    (setq objdat
		   (subst
		     (cons '13 (list (nth 1 (assoc '13 objdat)) (nth 2 (assoc '13 objdat)) (pok:z (list (nth 1 (assoc '13 objdat)) (nth 2 (assoc '13 objdat))) )))
		     (assoc '13 objdat)
		     objdat
		     )
		   )
	    
	    (entmod objdat)
	    (entupd obj)
	    )
	  )
	(setq ssi (+ ssi 1))
      )
      
    )
  )
  )