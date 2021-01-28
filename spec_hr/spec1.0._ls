;;;written by Nikolay Chehlarov
;;;Sofia, may 2008
;;; This module get table data from Acad sheet and put in text file
;;; note the order of drawing texts in table is imoprtant!!!
;;; works with spec.xls
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)


;;;writing to file 2 assoc list and one text
(defun ss:tofile (Fidat Ndat)
   (setq f (open "C:\\Documents and Settings\\Xave\\Desktop\\spec\\blok2.txt" "a"))
      ;(if (setq fname (entsel))
	;(princ (cdr (assoc '1 (entget (car fname)))) f)
      ;)
  (while (not  (setq fname (entsel)))
      )
(princ (cdr (assoc '1 (entget (car fname)))) f)
  
      (princ "\t" f)

  ;;;handling fi
      (setq i 0)
      (while (< i (length Fidat))
	(princ (cdr (nth i Fidat)) f)
	
;	(princ (cdr (nth i Fidat)) )
	
	(princ "\t" f)
	(princ (* (ss:kg (car (nth i Fidat)) )(cdr (nth i Fidat))) f )
	(princ "\t" f)
;	(princ (* (ss:kg (car (nth i Fidat)) )(cdr (nth i Fidat)))  )
	(setq i (+ i 1))
      )

   ;;;handling N
      (setq i 0)
      (while (< i (length Ndat))
	(princ (cdr (nth i Ndat)) f)
	(princ "\t" f)
	(princ (* (ss:kg (car (nth i Ndat)) )(cdr (nth i Ndat))) f )
	(princ "\t" f)
	(setq i (+ i 1))
      )

      (princ "\n" f)

      (if (not (close f))
	(print "file closed properly")
      )
  )


(defun ss:len(str)
  (setq problem 0)
  (if (= (vl-string-position (ascii "(") str) 0) (setq str (vl-string-left-trim "(" str)))
  (if (= (vl-string-position (ascii ")") str) (- (strlen str) 1)) (setq str (vl-string-right-trim ")" str)))
  (setq tickpos (vl-string-position (ascii "~") str))
  (if (not tickpos)
    (progn
      (setq lll (atof str))
    
    (if	(/= (strlen (rtos (atof str))) (strlen str))
      (progn (alert "Problem in length") (setq problem 1))
    )
  ))
  (if (not (not tickpos))
    (progn
      (if ( = (setq l1 (atof (substr str 1 tickpos))) 0) (progn(alert "Problem in length") (setq problem 1)))
      (if ( = (setq l2 (atof (substr str (+ tickpos 2) (- (strlen str) tickpos 1)))) 0) (progn(alert "Problem in length") (setq problem 1)))
    (setq lll (/(+ l1 l2) 2))
    )
    )
  (if (= problem 1) (setq lll 0))
lll
  )
;;; error circle
(defun drawcc (center)
  (alert "problem")
					;(setq center (list 10 10))
  (vla-addCircle
    *ModelSpace*
    (vlax-3d-point center)		; convert to ActiveX-compatible 3D point
    80
  )
)

;;;funkciq ne za gredi
(defun c:ss ()
  (setq probstop 0)
  (setq	Ndat (list (cons '8 0)
		   (cons '10 0)
		   (cons '12 0)
		   (cons '14 0)
		   (cons '16 0)
		   (cons '18 0)
		   (cons '20 0)
		   (cons '22 0)
		   (cons '25 0)
		   (cons '28 0)
	     )
  )
  (setq	Fidat (list (cons '6.5 0)
		    (cons '8 0)
		    (cons '10 0)
		    (cons '12 0)
					;(cons '14 0)
	      )
  )

  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	;;;finding block STEEL-N;;;
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "STEEL-N")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt)) ;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "L")
		(setq L (ss:len (cdr (assoc 1 cdat))))
		)
	      (if (= (cdr (assoc 2 cdat)) "BR")
		(setq BR (atoi (cdr (assoc 1 cdat))))
		)
	      (if (= (cdr (assoc 2 cdat)) "D")
		(setq D (atof (cdr (assoc 1 cdat))))
		)
	      (setq catt (entnext catt))
	      )
	    (if (or (= BR 0) (= L 0) (= (assoc D Ndat) nil)) (progn (setq probstop 1)(drawcc (cdr (assoc '10 objdata))) ))
	    (setq Ndat
		       (subst
			 (cons
			   D
			   (+ (/ (* BR L) 100) (cdr (assoc D Ndat)))
			 )
			 (assoc D Ndat)
			 Ndat
		       )
		)
	  )
	)
	;;;finding block STEEL-NX;;;
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "STEEL-NX")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt)) ;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "L")
		(setq L (ss:len (cdr (assoc 1 cdat))))
		)
	      (if (= (cdr (assoc 2 cdat)) "BR")
		(setq BR (atoi (cdr (assoc 1 cdat))))
		)
	      (if (= (cdr (assoc 2 cdat)) "D")
		(setq D (atof (cdr (assoc 1 cdat))))
		)
	      (setq catt (entnext catt))
	      )
	    (if (or (= BR 0) (= L 0) (= (assoc D Ndat) nil)) (progn (setq probstop 1)(drawcc (cdr (assoc '10 objdata))) ))
	    (setq Ndat
		       (subst
			 (cons
			   D
			   (+ (/ (* BR L) 100) (cdr (assoc D Ndat)))
			 )
			 (assoc D Ndat)
			 Ndat
		       )
		)
	  )
	)
	;;;finding block 1STEEL-FIX;;;
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "1STEEL-FIX")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt)) ;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "L")
		(setq L (ss:len (cdr (assoc 1 cdat))))
		)
	      (if (= (cdr (assoc 2 cdat)) "BR")
		(setq BR (atoi (cdr (assoc 1 cdat))))
		)
	      (if (= (cdr (assoc 2 cdat)) "D")
		(setq D (atof (cdr (assoc 1 cdat))))
		)
	      (setq catt (entnext catt))
	      )
	    (if (or (= BR 0) (= L 0) (= (assoc D Fidat) nil)) (progn (setq probstop 1)(drawcc (cdr (assoc '10 objdata))) ))
	    (setq Fidat
		       (subst
			 (cons
			   D
			   (+ (/ (* BR L) 100) (cdr (assoc D Fidat)))
			 )
			 (assoc D Fidat)
			 Fidat
		       )
		)
	  )
	)


	(setq ssi (+ ssi 1))
      )
;;;end while (setq obj (ssname ss ssi)
    )
;;;end progn
  )
;;;end if (setq ss (arm:getss)
  (print Ndat)
  (print Fidat)
  (if (= probstop 0) (draw))
  (princ)
)
;;;end defun


;;;area
(defun ss:kg (n)
  (if (= n 6.5) (setq ret 0.260))
  (if (= n 8) (setq ret 0.395))
  (if (= n 10) (setq ret 0.617))
  (if (= n 12) (setq ret 0.888))
  (if (= n 14) (setq ret 1.208))
  (if (= n 16) (setq ret 1.578))
  (if (= n 18) (setq ret 1.998))
  (if (= n 20) (setq ret 2.466))
  (if (= n 22) (setq ret 2.984))
  (if (= n 25) (setq ret 3.854))
  (if (= n 28) (setq ret 4.834))
 ret
  )
;;;end of area

;trunc to 1
(defun ss:trunc (num)
  (if (> (rem num 1) 0)
    (setq num (+ (- num (rem num 1)) 1))
  )
  num
)

;;drawing table
(defun draw ()
  (setq start (getpoint "Enter start point"))
  (vla-addLine
    *ModelSpace*
    (vlax-3d-point start)
    (vlax-3d-point
      (list (car start) (- (cadr start) 90))
    )
  )
  (vla-addtext
    *ModelSpace*
    "Дължина"
    (vlax-3d-point
      (list (+ (car start) 10) (- (cadr start) 52))
    )
    15.75
  )
  (vla-addtext
    *ModelSpace*
    "Тегло, кг"
    (vlax-3d-point
      (list (+ (car start) 10) (- (cadr start) 82))
    )
    15.75
  )
  (vla-addLine
    *ModelSpace*
    (vlax-3d-point (list (+ (car start) 100) (cadr start)))
    (vlax-3d-point
      (list (+ (car start) 100) (- (cadr start) 90))
    )
  )
  (setq i 0)
  (setq AI 0)
  (setq curenx (+ (car start) 80))
  (while (setq cfi (nth i Fidat))
    (if	(/= (cdr cfi) 0)
      (progn
	(vla-addtext
	  *ModelSpace*
	  (strcat (chr 244) (rtos (car cfi)))
	  (vlax-3d-point
	    (list (+ curenx 28) (- (cadr start) 22))
	  )
	  15.75
	)
	(vla-addtext
	  *ModelSpace*
	   (rtos (ss:trunc (cdr cfi)))
	  (vlax-3d-point
	    (list (+ curenx 28) (- (cadr start) 52))
	  )
	  15.75
	)
	(vla-addtext
	  *ModelSpace*
	   (rtos (ss:trunc (* (ss:kg (car cfi)) (cdr cfi))))
	  (vlax-3d-point
	    (list (+ curenx 28) (- (cadr start) 82))
	  )
	  15.75
	)

	(vla-addLine
    *ModelSpace*
    (vlax-3d-point (list (+ curenx 80) (cadr start)))
    (vlax-3d-point  (list (+ curenx 80) (- (cadr start) 90))
    )
  )
	(setq curenx (+ curenx 80))
	(setq AI (+ AI (* (ss:kg (car cfi)) (cdr cfi))))
      )
    )
    (setq i (+ i 1))
  )
  (vla-addtext
	  *ModelSpace*
	   (strcat "A-I:"(rtos (ss:trunc AI)) "кг.")
	  (vlax-3d-point
	    (list (- curenx 52) (- (cadr start) 112))
	  )
	  15.75
	)

  ;;;handling N
  (setq i 0)
  (setq AIII 0)
  (while (setq cfi (nth i Ndat))
    (if	(/= (cdr cfi) 0)
      (progn
	(vla-addtext
	  *ModelSpace*
	  (strcat "N" (rtos (car cfi)))
	  (vlax-3d-point
	    (list (+ curenx 28) (- (cadr start) 22))
	  )
	  15.75
	)
	(vla-addtext
	  *ModelSpace*
	   (rtos (ss:trunc (cdr cfi)))
	  (vlax-3d-point
	    (list (+ curenx 28) (- (cadr start) 52))
	  )
	  15.75
	)
	(vla-addtext
	  *ModelSpace*
	   (rtos (ss:trunc (* (ss:kg (car cfi)) (cdr cfi))))
	  (vlax-3d-point
	    (list (+ curenx 28) (- (cadr start) 82))
	  )
	  15.75
	)

	(vla-addLine
    *ModelSpace*
    (vlax-3d-point (list (+ curenx 80) (cadr start)))
    (vlax-3d-point  (list (+ curenx 80) (- (cadr start) 90))
    )
  )
	(setq curenx (+ curenx 80))
	(setq AIII (+ AIII (* (ss:kg (car cfi)) (cdr cfi))))
      )
    )
    (setq i (+ i 1))
  )

  (vla-addtext
	  *ModelSpace*
	   (strcat "A-III:" (rtos (ss:trunc AIII)) "кг.")
	  (vlax-3d-point
	    (list (- curenx 52) (- (cadr start) 112))
	  )
	  15.75
	)
  (vla-addtext
	  *ModelSpace*
	   (strcat "Общо:"(rtos (ss:trunc (+ AI AIII))) "кг.")
	  (vlax-3d-point
	    (list (- curenx 52) (- (cadr start) 142))
	  )
	  15.75
	)

  

  (vla-addLine
    *ModelSpace*
    (vlax-3d-point start)
    (vlax-3d-point  (list curenx  (cadr start)))
    )

  (vla-addLine
    *ModelSpace*
    (vlax-3d-point (list (car start) (- (cadr start) 30)))
    (vlax-3d-point
      (list curenx  (- (cadr start) 30))
    )
  )

  (vla-addLine
    *ModelSpace*
    (vlax-3d-point (list (car start) (- (cadr start) 60)))
    (vlax-3d-point
      (list curenx  (- (cadr start) 60))
    )
  )

  (vla-addLine
    *ModelSpace*
    (vlax-3d-point (list (car start) (- (cadr start) 90)))
    (vlax-3d-point
      (list curenx  (- (cadr start) 90))
    )
  )

  (ss:tofile Fidat Ndat)
    )

  




