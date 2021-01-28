;;;this lisp loads design moments from ansys files
;;;including parabolla
;;;including drawparabolla
;;;including col
;;;written by Nikolay Chehlarov July 2008
(setq path (strcat (vla-get-path
		     (vla-get-ActiveDocument (vlax-get-acad-object))
		   ) "\\"
	   )
)


(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq *layers*
       (vla-get-layers
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq *app*
       (vla-get-application
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq txth 0.1)


(defun c:pp ()

  (setvar "cmdecho" 0)

;;; NLIST.lis contains node list
  (if (setq fil
	     (open
	       (strcat path "NLIST.lis")
	       "r"
	     )
      )
    (progn
					;pyrviq zapis e 0 taka 4e nomerata otgovrqt s tezi ot ansys
      (print "1.reading NLIST.lis")
      (setq node (list (list 0 0)))
      (while (setq lin (read-line fil))
	(if (and (read lin) (= (type (read lin)) 'int))
	  (progn
					;(PRINT LIN)
	    (if	(vl-string-position (ascii "E") lin)
	      (setq lin
		     (strcat
		       (substr lin
			       1
			       (+ (vl-string-position (ascii "E") lin) 4)
		       )
		       " "
		       (substr lin
			       (+ (vl-string-position (ascii "E") lin) 5)
			       (strlen lin)
		       )
		     )
	      )
	    )

	    (setq lin (vl-string-left-trim " " lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim "0" lin))
	    (setq lin (vl-string-left-trim "." lin))
	    (setq lin (vl-string-left-trim "0" lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq x (read lin))
					;(setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq
	      lin (substr lin
			  (+ (vl-string-position (ascii " ") lin) 2)
			  (strlen lin)
		  )
	    )

	    (if	(vl-string-position (ascii "E") lin)
	      (setq lin
		     (strcat
		       (substr lin
			       1
			       (+ (vl-string-position (ascii "E") lin) 4)
		       )
		       " "
		       (substr lin
			       (+ (vl-string-position (ascii "E") lin) 5)
			       (strlen lin)
		       )
		     )
	      )
	    )

	    (setq lin (vl-string-left-trim "0" lin))
	    (setq lin (vl-string-left-trim "." lin))
	    (setq lin (vl-string-left-trim "0" lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq y (read lin))
	    (setq node
		   (append node (list (list x y))))
	  )
	)
      )
      (close fil)
    )
    (alert "Error - File not opened")
  )

;;; opening elist.lis
;;; getting information for elements and its nodes
  (if (setq fil
	     (open
	       (strcat path "ELIST.lis")
	       "r"
	     )
      )
    (progn
      (print "2.reading ELIST.lis")
      (setq elemnum 0)
      (setq elem (list (list 0 0 0 0)))
      (while (setq lin (read-line fil))
	(if (and (read lin) (= (type (read lin)) 'int))
	  (progn
	    (setq lin (vl-string-left-trim " " lin))
	    (setq elemnum (read lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq n1 (read lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq n2 (read lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq n3 (read lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (setq n4 (read lin))

	    (setq elem (append elem (list (list n1 n2 n3 n4))))
	  )
	)
      )
      (close fil)
    )
    (alert "Error - File not opened")
  )

;;; opening PRETAB.lis
;;; getting information for elements and its nodes
  (if (setq fil
	     (open
	       (strcat path "PRETAB.lis")
	       "r"
	     )
      )
    (progn
      (print "3.reading PRETAB.lis")
      (setq mom (list (list 0 0)))
      (while (setq lin (read-line fil))
	(if (and (read lin) (= (type (read lin)) 'int))
	  (progn
	    (setq lin (vl-string-left-trim " " lin))
	    (setq lin (vl-string-left-trim (rtos (read lin)) lin))
	    (setq lin (vl-string-left-trim " " lin))
	    (if	(vl-string-position (ascii "E") lin)
	      (setq lin
		     (strcat
		       (substr lin
			       1
			       (+ (vl-string-position (ascii "E") lin) 4)
		       )
		       " "
		       (substr lin
			       (+ (vl-string-position (ascii "E") lin) 5)
			       (strlen lin)
		       )
		     )
	      )
	    )
	    (setq Mx (read lin))
	    (setq
	      lin (substr lin
			  (+ (vl-string-position (ascii " ") lin) 2)
			  (strlen lin)
		  )
	    )
	    (setq lin (vl-string-left-trim " " lin))
	    (if	(vl-string-position (ascii "E") lin)
	      (setq lin
		     (strcat
		       (substr lin
			       1
			       (+ (vl-string-position (ascii "E") lin) 4)
		       )
		       " "
		       (substr lin
			       (+ (vl-string-position (ascii "E") lin) 5)
			       (strlen lin)
		       )
		     )
	      )
	    )
	    (setq My (read lin))
	    (setq
	      lin (substr lin
			  (+ (vl-string-position (ascii " ") lin) 2)
			  (strlen lin)
		  )
	    )
	    (setq lin (vl-string-left-trim " " lin))
	    (setq Mxy (read lin))

	    (if	(> Mx 0)
	      (setq Mdx (+ Mx (abs Mxy)))
	      (setq Mdx (- Mx (abs Mxy)))
	    )
	    (if	(> My 0)
	      (setq Mdy (+ My (abs Mxy)))
	      (setq Mdy (- My (abs Mxy)))
	    )
	    (setq mom (append mom (list (list Mdx Mdy))))
	  )
	)
      )
      (close fil)
    )
    (alert "Error - File not opened")
  )



;;;syzdavana na sloeve
  (setq obj (vla-add *layers* "DX"))
  (vla-put-color obj acred)
  (setq obj (vla-add *layers* "DY"))
  (vla-put-color obj acblue)
  (setq obj (vla-add *layers* "GX"))
  (vla-put-color obj acgreen)
  (setq obj (vla-add *layers* "GY"))
  (vla-put-color obj acmagenta)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parametri
  (setq H0 16)
  (setq Rb 1.45)
  (setq Rs 37.5)
  (setq Asmin (* H0 0))
  (setq xr (* 0.559 H0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (princ (strcat "\n H0=" (rtos H0 2 2) " "))
  (princ (strcat "Rb=" (rtos Rb 2 2) " "))
  (princ (strcat "Rs=" (rtos Rs 2 2) " "))
  (princ (strcat "miu min=" (rtos (/ Asmin H0) 2 2) " "))
  (princ (strcat "xr=" (rtos xr 2 2) " "))

  
  
  (print "4.Drawing")
  (setq Mr (* Rb xr (- H0 (/ xr 2))))

  (setq i 1)
  (while (<= i elemnum)

    (setq Mx (* 1 (car (nth i mom))))
    (setq My (* 1 (cadr (nth i mom))))

   ; (vla-put-activelayer *layers* objdx)
    (if	(< (abs Mx) Mr)
      (progn
	(setq x (* H0 (- 1 (sqrt (- 1 (/ (* 2 (abs Mx)) (* Rb H0 H0)))))))
	(setq Ass (max (/ (* Rb 100 x) Rs) Asmin))
;;; sega e kato v scripta na jonko
	(setq ip (list
		   (/(+ (car (nth (car (nth i elem)) node))
		      (car (nth (cadr (nth i elem)) node))
		      (car (nth (caddr (nth i elem)) node))) 3)
		   (/(+ (cadr (nth (car (nth i elem)) node))
		      (cadr (nth (cadr (nth i elem)) node))
		      (cadr (nth (caddr (nth i elem)) node))) 3)
		 )
	)
	(setq objtext (vla-addtext
	  *ModelSpace*
	  (rtos Ass 2 2)
	  (vlax-3d-point ip)
	  txth
	))
      
      (if (> Mx 0)
	(vla-put-layer objtext "GX")
	(vla-put-layer objtext "DX")
	)

	);;;else
      (alert (strcat "very big moment in element " (rtos i)))
    )
    ;;;;;;;;;;;;;;;;;;;;;;
    (if	(< (abs My) Mr)
      (progn
	(setq x (* H0 (- 1 (sqrt (- 1 (/ (* 2 (abs My)) (* Rb H0 H0)))))))
	(setq Ass (max (/ (* Rb 100 x) Rs) Asmin))
;;; nqma smisyl pak da gi smqtam ip
	
	(setq objtext (vla-addtext
	  *ModelSpace*
	  (rtos Ass 2 2)
	  (vlax-3d-point ip)
	  txth
	))
      
      (vla-put-rotation objtext (/ pi 2))
      (if (> My 0)
	(vla-put-layer objtext "GY")
	(vla-put-layer objtext "DY")
	)
	);;;else
      (alert (strcat "very big moment in element " (rtos i)))
    )
    
    (setq i (+ i 1))
  )
  ;;;drawing shells
  (setq sf (getstring "Draw shell elements[Y/N]?"))
  (if (= sf "Y")
    (progn
      (setq i 1)
       (while (<= i elemnum)
	 (setq p1 (nth (car (nth i elem)) node))
	 (setq p2 (nth (cadr (nth i elem)) node))
	 (setq p3 (nth (caddr (nth i elem)) node))
	 (setq p4 (nth (cadddr (nth i elem)) node))
	 (vla-Add3DFace
      *ModelSpace*
      (vlax-3D-Point (list (car p1) (cadr p1) 0))
      (vlax-3D-Point (list (car p2) (cadr p2) 0))
      (vlax-3D-Point (list (car p3) (cadr p3) 0))
      (vlax-3D-Point (list (car p4) (cadr p4) 0))
      )
	 (setq i (+ i 1))
	 );;;while
      )
    )
(vla-ZoomExtents *app*)
)


(defun col:getss (/ ss)
  (setq ss (ssget))
)
(defun c:col ()
  (if (setq ss (col:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (< (abs (atof (cdr (assoc '1 objdata)))) 2.51)
					;	; ;(>= (atof (cdr (assoc '1 objdata))) -150)
	    )
	  (progn
	    (vla-put-color (vlax-ename->vla-object obj) 252)
	    ;(setq objdata (subst
	;		    (cons '62 3)
	;		    (assoc 62 objdata)
	;		    objdata
	;		  )
	 ;   )
	 ;   (entmod objdata)
	 ;   (entupd obj)
	  )
	)
	(setq ssi (+ ssi 1))
      )
    )
  )
)

;;;puts color on text
(defun c:colll ()
  (if (setq ss (col:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (or
		   (< (atof (cdr (assoc '1 objdata))) -1200)
		   (> (atof (cdr (assoc '1 objdata))) 1200)
		 )
					;	; ;(>= (atof (cdr (assoc '1 objdata))) -150)
	    )
	  (progn
	    (setq objdata (subst
			    (cons '62 1)
			    (assoc 62 objdata)
			    objdata
			  )
	    )
	    (entmod objdata)
	    (entupd obj)
	  )
	)
	(setq ssi (+ ssi 1))
      )
    )
  )
)

;;;calculate value between other 3 by parabolic equation; used for edge moemnts
(defun c:par ()
  (while (not (setq ss (entsel "first ")))
  )
  (setq p1 (entget (car ss)))
  (print)
  (while (not (setq ss (entsel "second ")))
  )
  (setq p2 (entget (car ss)))
  (print)
  (while (not (setq ss (entsel "third ")))
    ch
  )
  (setq p3 (entget (car ss)))
  (print)


  (setq	d1 (distance (list (cadr (assoc 10 p1)) (caddr (assoc 10 p1)))
		     (list (cadr (assoc 10 p2)) (caddr (assoc 10 p2)))
	   )
  )
  (setq	d2 (distance (list (cadr (assoc 10 p2)) (caddr (assoc 10 p2)))
		     (list (cadr (assoc 10 p3)) (caddr (assoc 10 p3)))
	   )
  )
  (setq c (atof (cdr (assoc 1 p1))))
  (setq b1 (/ (- (atof (cdr (assoc 1 p2))) c) d1))
  (setq dd (+ d1 d2))
  (setq	a (/
	    (- (atof (cdr (assoc 1 p3))) c (* b1 dd))
	    (- (* dd dd) (* d1 dd))
	  )
  )
  (setq b (- b1 (* a d1)))
  (print a)
  (print b)
  (print c)

  (while (setq p (getpoint))
    (setq
      x	(distance (list (cadr (assoc 10 p1)) (caddr (assoc 10 p1)))
		  (list (car p) (cadr p))
	)
    )
					;(setq M (+ (* a x x) (* b x) c))

    (vla-addtext
      *ModelSpace*
      (rtos (Mvalue a b c x) 2 2)
      (vlax-3d-point p)
      txth
    )
  )

)


;;;drawing the parabolla
(defun c:dpar ()
  (while (not (setq ss (entsel "first ")))
  )
  (setq p1 (entget (car ss)))
  (print)
  (while (not (setq ss (entsel "second ")))
  )
  (setq p2 (entget (car ss)))
  (print)
  (while (not (setq ss (entsel "third ")))
    ch
  )
  (setq p3 (entget (car ss)))
  (print)


  (setq	d1 (distance (list (cadr (assoc 10 p1)) (caddr (assoc 10 p1)))
		     (list (cadr (assoc 10 p2)) (caddr (assoc 10 p2)))
	   )
  )
  (setq	d2 (distance (list (cadr (assoc 10 p2)) (caddr (assoc 10 p2)))
		     (list (cadr (assoc 10 p3)) (caddr (assoc 10 p3)))
	   )
  )
  (setq c (atof (cdr (assoc 1 p1))))
  (setq b1 (/ (- (atof (cdr (assoc 1 p2))) c) d1))
  (setq dd (+ d1 d2))
  (setq	a (/
	    (- (atof (cdr (assoc 1 p3))) c (* b1 dd))
	    (- (* dd dd) (* d1 dd))
	  )
  )
  (setq b (- b1 (* a d1)))
  (print a)
  (print b)
  (print c)

  (setq step (/ (+ d1 d2) 20))
  (setq x 0)
  (while (<= x (+ d1 d2))
					;(setq M (+ (* a x x) (* b x) c))
    (vla-addpoint
      *ModelSpace*
      (vlax-3d-point
	(list (+ (cadr (assoc 10 p1)) x)
	      (+ (Mvalue a b c x) (caddr (assoc 10 p1)))
	)
      )
    )
    (setq x (+ x step))
  )

)
;;;return the value
(defun Mvalue (a b c x)
  (setq M (+ (* a x x) (* b x) c))
)