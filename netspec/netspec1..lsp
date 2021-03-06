;;;written by Nikolay Chehlarov
;;;Sofia, may 2008
;;; This module get table data from Acad sheet and put in text file
;;; note the order of drawing texts in table is imoprtant!!!
;;; works with spec.xls
(setq path "C:/Documents and Settings/DENICA/Desktop/temp/")

(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun n:getss (/ ss)
  (setq ss (ssget))
)

(defun c:netc ()
  (setq	outfile
	 (strcat
	   path
	   "count.txt"
	 )
  )
  (setq f (open outfile "w"))
  (if (setq ss (n:getss))
    (progn
      (setq ssi 0)
      (setq kol	(list
		  (cons 'ST10 0)
		  (cons 'ST20 0)
		  (cons 'ST25 0)
		  (cons 'ST30 0)
		  (cons 'ST35 0)
		  (cons 'ST50 0)
		  (cons 'ST60 0)
		  (cons 'ST15C 0)
		  (cons 'ST25C 0)
		  (cons 'ST25CS 0)
		  (cons 'ST40C 0)
		  (cons 'ST50C 0)
		  (cons 'ST65C 0)
		  (cons 'ST65C 0)
		  (cons 'RAFV 0)
		)
      )
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))		
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (substr (cdr (assoc '2 objdata)) 1 2) "*U")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "ST25")
		(setq r1 (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "DIM")
		(setq r2 (cdr (assoc 1 cdat)))
	      )
	      (setq catt (entnext catt))
	    )
;;;;;;;
	    (setq sppos (vl-string-position (ascii " ") r1))
	    (setq n (substr r1 1 (+ sppos 0)))
	    (setq n (vl-string-translate "," "." n))
	    (setq n (atof n))

;;;ZA KEY DA GO NAPRAQ ILI DO SLEDVA6TIQ INTERVAL AKO IMA AXIS SLED TOVA
					;(setq key (substr r1 (+ sppos 2)))

	    (setq eqpos (vl-string-position (ascii "=") r2))
	    (setq xpos (vl-string-position (ascii "x") r2))

	    (setq L (atof (substr r2 (+ eqpos 2) (- xpos eqpos 1))))

	    (setq B (atof (substr r2 (+ xpos 2))))
;;;;;;;;;;;;;;;;;;;;;;;
	    (setq fl 0)
	    (if	(wcmatch r1 "*ST10*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST10
			       (+ (* n L B 0.0001) (cdr (assoc 'ST10 kol)))
			 )
			 (assoc 'ST10 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST20*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST20
			       (+ (* n L B 0.0001) (cdr (assoc 'ST20 kol)))
			 )
			 (assoc 'ST20 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST25*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25
			       (+ (* n L B 0.0001) (cdr (assoc 'ST25 kol)))
			 )
			 (assoc 'ST25 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST30*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST30
			       (+ (* n L B 0.0001) (cdr (assoc 'ST30 kol)))
			 )
			 (assoc 'ST30 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST35*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST35
			       (+ (* n L B 0.0001) (cdr (assoc 'ST35 kol)))
			 )
			 (assoc 'ST35 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST50*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST50
			       (+ (* n L B 0.0001) (cdr (assoc 'ST50 kol)))
			 )
			 (assoc 'ST50 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST60*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST60
			       (+ (* n L B 0.0001) (cdr (assoc 'ST60 kol)))
			 )
			 (assoc 'ST60 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST15C*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST15C
			       (+ (* n L B 0.0001) (cdr (assoc 'ST15C kol)))
			 )
			 (assoc 'ST15C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST25C*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25C
			       (+ (* n L B 0.0001) (cdr (assoc 'ST25C kol)))
			 )
			 (assoc 'ST25C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST25CS*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25CS
			       (+ (* n L B 0.0001) (cdr (assoc 'ST25CS kol)))
			 )
			 (assoc 'ST25CS kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST40C*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST40C
			       (+ (* n L B 0.0001) (cdr (assoc 'ST40C kol)))
			 )
			 (assoc 'ST40C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST50C*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST50C
			       (+ (* n L B 0.0001) (cdr (assoc 'ST50C kol)))
			 )
			 (assoc 'ST50C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*ST65C*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST65C
			       (+ (* n L B 0.0001) (cdr (assoc 'ST65C kol)))
			 )
			 (assoc 'ST65C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(wcmatch r1 "*RAFV*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'RAFV
			       (+ (* n L B 0.0001) (cdr (assoc 'RAFV kol)))
			 )
			 (assoc 'RAFV kol)
			 kol
		       )
		)
	      )
	    )


	    (print kol)
	    (if (/= fl 1) (alert "error occured!!!"))
	  )
	)
	(setq ssi (+ ssi 1))
      )
     
      
;;;;writing to file

      (setq ssi 0)
      (princ "type	Nombre" f)
      (while (< ssi (length kol))
	(print (car (nth ssi kol)) f)
	(princ "\t" f)
	(princ (cdr (nth ssi kol)) f)
	(setq ssi (+ ssi 1))
	)
;;;;;;end of writing to file
    )
  )
  (close f)
  (princ)
)
