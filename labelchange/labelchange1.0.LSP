(vl-load-com)
(defun t:getss (/ ss)
  (setq ss (ssget))
)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun c:tt ()
  (if (setq ss (t:getss))
    (progn
					;(setq char (getstring "Enter first letter: "))
					; (setq char "G")
      (setq ssi 0)

      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj))
	(if (= (cdr (assoc 0 objdat)) "MTEXT")
	  (progn
	    (setq str (cdr (assoc 1 objdat)))
	    (setq str (vl-string-right-trim "}" str))
	    (setq coma (+ (vl-string-position (ascii ";") str) 2))
	    (setq str (substr str coma))
	    (setq space (vl-string-position (ascii " ") str))
	    (setq name (substr str 1 space))
	    (setq dims (substr str (+ space 2)))
					;(print name)
					;(print dims)
	    (setq first (substr str 1 1))
	    (if	(or (= first "L") (= first "C") (= first "P"))
	      (progn
					;slaga textov string
		(print "okkk")
		(setq objb (vla-InsertBlock
			     *ModelSpace*
			     (vlax-3d-point (cdr (assoc 10 objdat)))
			     "label"
			     1
			     1
			     1
			     (cdr (assoc 50 objdat))
			   )
		)
		(setq att (vlax-vla-object->ename objb))
		(setq catt (entnext att))
		(while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
		  (progn
		    (setq cdat (entget catt))
;;;diameter
		    (if	(= (cdr (assoc 2 cdat)) "NAME")
		      (progn
			(setq cdat (subst
				     (cons '1 name)
				     (assoc 1 cdat)
				     cdat
				   )
			)
			(entmod cdat)
			(entupd catt)
		      )
		    )
;;;span
		    (if	(= (cdr (assoc 2 cdat)) "DIMS")
		      (progn
			(setq cdat (subst
				     (cons '1 dims)
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
		)
		;;;;;;;
		;deletting obj
		(entdel obj)

		;;;;;;;
	      )
	    )
	  )
	)
	(setq ssi (+ ssi 1))
      )

    )
  )
  (princ)
)