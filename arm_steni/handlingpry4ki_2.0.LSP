;;;written by Nikolay Chehlarov
;;;Sofia, july 2009


(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun hp:getss (/ ss)
  (setq ss (ssget))
)

(defun c:qx()
  (while (not (setq obj (car (entsel)))))
  
  (setq len 0)
  (setq pd (entget  obj))
  (if (/= (cdr (assoc '8 pd)) "COTATION") (alert "The layer is not COTATION!"))
    
	  (setq cp 0)
	  (setq found 0)
	  (repeat (length pd)
	    (if	(= (car (nth cp pd)) 10)
	      (progn
		(if (> found 0)
		  (progn
		    (setq found (+ found 1))
		    (setq cpx (cadr (nth cp pd)))
		    (setq cpy (caddr (nth cp pd)))
		    (setq len  (max
				 (sqrt (+ (* (- cpx bpx) (- cpx bpx))
				(* (- cpy bpy) (- cpy bpy))
				       )
			      )
				 len)
		    )
		    (setq bpx cpx)
		    (setq bpy cpy)
		    
		    
					;(princ "  ")
		  )
		)
		(if (= found 0)
		  (progn
		    (setq found 1)
		    (setq bpx (cadr (nth cp pd)))
		    (setq bpy (caddr (nth cp pd)))
		  )
		)

	      )
	    )
	    (setq cp (+ cp 1))
	  )
	
      (print len)
  (setq e (getint "e= "))
  (print (/ len e))
  (princ "->")
  (setq br (+ (/ len e) 1))
  (setq br (- br (rem br 1)))
  
  (princ br)
;;;za vmykvane
  (while (not (setq obj (car (entsel)))))
  (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "NOMBRE")
		(progn
		  (setq	cdat (subst
			       (cons '1 (rtos br))
			       (assoc 1 cdat)
			       cdat
			     )
		  )
		  (entmod cdat)
		  (entupd catt)
		)
	      )
	      (if (= (cdr (assoc 2 cdat)) "ESPACEMENTS")
		(progn
		  (setq	cdat (subst
			       (cons '1 (rtos e))
			       (assoc 1 cdat)
			       cdat
			     )
		  )
		  (entmod cdat)
		  (entupd catt)
		)
	      )
	       (setq catt (entnext catt))
	    )
  );;;end defun



