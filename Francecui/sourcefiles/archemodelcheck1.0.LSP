;;;written by Nikolay Chehlarov
;;;Sofia, feb 2008
;;;checks a model for imort in Ansys
(vl-load-com)
(setq tolmin 1e-6)
(setq tol 0.05)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun mc:draw (center)
  (vla-addCircle
    *ModelSpace*
    (vlax-3d-point center)
    1
  )
)

(defun mc:getss	(/ ss)
  (setq ss (ssget))
)

(defun mc:height (p1 p2 p)
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
  (setq osn (distance p1 p2))
  (setq h 0)
  (if (and (> (+ osn tol) (distance p p1))
	   (> (+ osn tol) (distance p p2))
	   )
  (if (/= osn 0) (setq h (/ A osn)))
    ;(if (= osn 0) (setq h (/ tol 2)))
    )
  h
)

(defun mc:entloop (ss p)
  (setq ssj 0)
      (while (setq objj (ssname ss ssj))
	(setq ld (entget objj))
	(if (= (cdr (assoc '0 ld)) "LWPOLYLINE")
	  (progn
	    (setq clp 0)
	    (setq found 0)
	    (repeat (length ld)
	    (if	(= (car (nth clp ld)) 10)
	      (progn
		(if (> found 0)
		  (progn
		    (setq found (+ found 1))
		    (setq cpx (cadr (nth clp ld)))
		    (setq cpy (caddr (nth clp ld)))

		    (setq hh (mc:height (list bpx bpy) (list cpx cpy) p))
		    (if (and (> hh 1e-6) (< hh tol))   ;;;za ansys 1e-6
		      (progn
			(print hh)		
		      (mc:draw p)
			)
		      )
		    
		    (setq bpx cpx)
		    (setq bpy cpy)
		  )
		)
		(if (= found 0)
		  (progn
		    (setq found 1)
		    (setq bpx (cadr (nth clp ld)))
		    (setq bpy (caddr (nth clp ld)))
		    (setq fbpx bpx)
		    (setq fbpy bpy)
		  )
		)

	      )
	    )
	    (setq clp (+ clp 1))
	  )
;;;repeat
	    ;;;proverka ako e zatvorena poliliniqta
	    (if	(= (vla-get-closed (vlax-ename->vla-object objj)) :vlax-true)
	      (progn
		(setq hh (mc:height (list fbpx fbpy) (list cpx cpy) p))
		(if (and (> hh tolmin) (< hh tol))
		  (progn
		    (print hh)
		    (mc:draw p)
		  )
		)
	      )
	    )

	     ;;;end proverka ako e zatvorena poliliniqta
	    
	  )
	)
;;;if (= (cdr (assoc '0 pdata)) "LWPOLYLINE")
	(setq ssj (+ ssj 1))
      )
  )

(defun c:mcheck	()
  (if (setq ss (mc:getss))
    (progn
     ; (setq tol (read (getstring "Enter tolerance ")))
      ;(setq tol 0.001)
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq pd (entget obj))
	(if (= (cdr (assoc '0 pd)) "LWPOLYLINE")
	  (progn
	    (setq cp 0)
	    (repeat (length pd)
	      (if (= (car (nth cp pd)) 10)
		(mc:entloop ss (list (cadr (nth cp pd)) (caddr (nth cp pd))) )

	      )
	      (setq cp (+ cp 1))
	    )
;;;repeat
	  )
	)
	;;;proverka na to4ki
	(if (= (cdr (assoc '0 pd)) "POINT")
	  (progn
		(mc:entloop ss (cdr (assoc '10 pd)) )
	      (setq cp (+ cp 1))
	    )
	  )
	;;;end proverka na to4ki
;;;if (= (cdr (assoc '0 pdata)) "LWPOLYLINE")
	(setq ssi (+ ssi 1))
      )
;;;while
    )
  )
  (princ)
)
