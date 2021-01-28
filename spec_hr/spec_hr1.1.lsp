;;;written by Nikolay Chehlarov
;;;Sofia, aug 2009
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

(defun arm:getss (/ ss)
  (setq ss (ssget))
)


;;; error circle
(defun drawcc (center)
  (alert "problem")			
  (vla-addCircle
    *ModelSpace*
    (vlax-3d-point center)	
    80
  )
)
;;;funkciq za opredelqne broq na pry4kite
;;;ako ima problem vra6ta 0
(defun arm:br (str / i sim cbr allbr)
  (setq i 1)
  (while (not (wcmatch (substr str i 1) "#"))
    (setq i (+ i 1))
  )
  (setq str (substr str i))
  
  (setq i 1)
  (setq cbr "")
  (setq allbr 1)
  (while (<= i (+ (strlen str) 1))
    (setq sim (substr str i 1))
    (if	(wcmatch sim "#")
      (setq cbr (strcat cbr sim))
    )
    (if	(or (= sim "x") (= sim ""))
      (progn
	(setq allbr (* allbr (atoi cbr)))
	(setq cbr "")
      )
    )
    (setq i (+ i 1))
  )
  allbr
)

;;;funkciq koqto vra6ta diametyra
(defun arm:d (str / xpos)
  (if (= (vl-string-position (ascii "%%") str) 0) (setq str (vl-string-left-trim "%%" str)))
  (setq xpos (vl-string-position (ascii "x") str))
  (atof (substr str 2 (- xpos 1)))
)

;;;funkciq koqto vra6ta dylginata v metri
;;; vra6ta 0 pri problem
(defun arm:l (str / i sim cl cl1 out)
  (setq cl "")
  (setq i (+ (vl-string-position (ascii "x") str) 2))
  (setq cl "")
  (while (wcmatch (setq sim (substr str i 1)) "#")  
      (setq cl (strcat cl sim))
    
    (setq i (+ i 1))
  )
  (setq out (/ (atof cl) 100))
(if (vl-string-position (ascii "~") str)
  (progn
    (setq cl1 "")
    (setq i (+ (vl-string-position (ascii "~") str) 2))
    (setq cl1 "")
    (while (wcmatch (setq sim (substr str i 1)) "#")
      (setq cl1 (strcat cl1 sim))

      (setq i (+ i 1))
    )
    
    (setq out (/ (+ (/ (atof cl1) 100) out) 2))
  )
)
out
  )



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
	(if  (and (= (cdr (assoc '0 objdata)) "TEXT")
		  (= (cdr (assoc '8 objdata)) "k_dim_arm")
		  (or
		    (vl-string-position (ascii "N") (cdr (assoc '1 objdata)))
		  (vl-string-position (ascii "%%C") (cdr (assoc '1 objdata)))
		    )
	     )
	     (progn
	    (setq str (cdr (assoc '1 objdata)))
	    (if (= (vl-string-position (ascii "%%") str) 0) (setq str (vl-string-left-trim "%%" str)))
	    (setq Npos (vl-string-position (ascii "N") str))
	    (setq Fipos (vl-string-position (ascii "%%C") str))
	    (setq narpos nil)
	    (if (/= Npos nil) (setq marpos Npos))
	    (if (/= Fipos nil) (setq marpos Fipos))
	    (if (not narpos)
	      (progn
	    (setq BR (arm:br (substr str 1 marpos)))

	    (setq D (arm:d (substr str (+ marpos 1))))

	    (setq L (arm:l (substr str (+ marpos 1))))
	    )
	      )
	    
	    ;;;;;; za N
	    (if	(/= Npos nil)
	      (progn
		(if (or (= BR 0) (= L 0) (= (assoc D Ndat) nil))
		  (progn (setq probstop 1)
			 (drawcc (cdr (assoc '10 objdata)))
		  )
		)
		(setq Ndat
		       (subst
			 (cons
			   D
			   (+ (* BR L) (cdr (assoc D Ndat)))
			 )
			 (assoc D Ndat)
			 Ndat
		       )
		)
	      )
	    )
	    ;;;;;;; end za N
	      ;;;;;; za Fi
	    (if	(/= Fipos nil)
	      (progn
		(if (or (= BR 0) (= L 0) (= (assoc D Fidat) nil))
		  (progn (setq probstop 1)
			 (drawcc (cdr (assoc '10 objdata)))
		  )
		)
		(setq Fidat
		       (subst
			 (cons
			   D
			   (+ (* BR L) (cdr (assoc D Fidat)))
			 )
			 (assoc D Fidat)
			 Fidat
		       )
		)
	      )
	    )
	    ;;;;;;; end za Fi
	    
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
  (print)
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
	  (strcat "%%C" (rtos (car cfi)))
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

  ;(ss:tofile Fidat Ndat)
    )

  




