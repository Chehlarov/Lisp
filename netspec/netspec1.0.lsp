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
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	;(print objdata)
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (substr (cdr (assoc '2 objdata)) 1 2) "*U")
	    )
	  (progn
	    (print "okkkk")
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "ST25")
		(setq r1 (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "dim")
		(setq r2 (cdr (assoc 1 cdat)))
	      )
	      (setq catt (entnext catt))
	    )
;;;;;;;
	    (setq sppos (vl-string-position (ascii " ") r1))
	    (setq n  (substr r1 1 (+ sppos 0)))
	    (setq n (vl-string-translate "," "." n))
	    (setq n (atof n))

	    ;;;ZA KEY DA GO NAPRAQ ILI DO SLEDVA6TIQ INTERVAL AKO IMA AXIS SLED TOVA
	    '(setq key (substr r1 (+ sppos 2)))

	    (setq eqpos (vl-string-position (ascii "=") r2))
	    (setq xpos (vl-string-position (ascii "x") r2))
	
	    (setq L (atof (substr r2 (+ eqpos 2) (- xpos eqpos 1))))

	    (setq B (atof (substr r2 (+ xpos 2))))

	    (setq fl 0)
	    (if (wcmatch  r1 "*ST10*")
	      )

	    
	    ;(print B)
	    ;(princ r1 f)
	    ;(princ r2 f)

	    (princ "\n" f)
	  )
	)
	(setq ssi (+ ssi 1))
      )

    )
  )
  (close f)
)
