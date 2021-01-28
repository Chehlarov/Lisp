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
(defun moml:getss (/ ss)
  (setq ss (ssget))
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; parametri
  (setq H0 64)
  (setq Rb (* 1.45 1.2))
  (setq Rs (* 37.5 1.2))
  (setq Asmin (* H0 0.0))
  (setq xr (* 0.559 H0))
  ;;;za fund plo4a e 0.1
  ;;;za obiknovenna plo4a e 0.15
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;Osnovna mrega
  (setq Asmain 0)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq txth 15)
(setq scale 100)
(setq prec 2)

;;;function for importing moments
(defun c:pp ()
  (ALERT "Using strengthen material Rb * 1.2 and Rs * 1.2")
  (setvar "cmdecho" 0)
  (if (setq fil
	     (open
	       ;"E:/Tsvety_2009/GRAND HOTEL VARNA/Delfin/fund/OSN/input.txt"
	       "E:\\Work_Mania\\GRAND HOTEL VARNA\\Rubin\\Blok_III\\xls\\xls_Kc=0.15\\OSOB\\GA\\input.txt"
	       "r"
	     )
      )
    (progn
;;;syzdavana na sloeve
      (setq obj (vla-add *layers* "DX"))
      (vla-put-color obj acred)
      (setq obj (vla-add *layers* "DY"))
      (vla-put-color obj acblue)
      (setq obj (vla-add *layers* "GX"))
      (vla-put-color obj acgreen)
      (setq obj (vla-add *layers* "GY"))
      (vla-put-color obj acmagenta)
;;;--- Read the first blank line in the x position (A,1)
      (setq lin (read-line fil))
      (setq cntr 1)
      (setq lin (read-line fil))
;;;--- Loop while there is another x coordinate      
      (while (and lin (< cntr 10000) (/= lin ""))
	(print lin)


	(setq x (read lin))
	(if (vl-string-position (ascii "\t") lin)
	  (setq	lin
		 (substr lin
			 (+ (vl-string-position (ascii "\t") lin) 2)
			 (strlen lin)
		 )
	  )
	)

	(setq y (read lin))
	(if (vl-string-position (ascii "\t") lin)
	  (setq	lin
		 (substr lin
			 (+ (vl-string-position (ascii "\t") lin) 2)
			 (strlen lin)
		 )
	  )
	)
;;;;;;;
	(if (and (/= lin "") (/= lin "\t") (vl-string-position (ascii "\t") lin))
	  (progn
	    (setq M11 (read lin))
	    (setq xx
		   (* H0 (- 1 (sqrt (- 1 (/ (* 2 (abs M11)) (* Rb H0 H0))))))
	    )
	    (if	(< xx xr)
	      (progn
		(setq M11Ass (max (/ (* Rb 100 xx) Rs) Asmin))
	      )
	      (print "Too big Moment!!!")
	    )

	    
	    (if	(vl-string-position (ascii "\t") lin)
	      (setq lin
		     (substr lin
			     (+ (vl-string-position (ascii "\t") lin) 2)
			     (strlen lin)
		     )
	      )
	    )

	    (setq M22 (read lin))
	    (setq xx
		   (* H0 (- 1 (sqrt (- 1 (/ (* 2 (abs M22)) (* Rb H0 H0))))))
	    )
	    (if	(< xx xr)
	      (progn
		(setq M22Ass (max (/ (* Rb 100 xx) Rs) Asmin))
	      )
	      (print "Too big Moment!!!")
	    )


	    (if	(>= M11 0)
	      (progn
		(setq
		  obj (vla-addtext
			*ModelSpace*
			(rtos M11Ass 2 prec)
			(vlax-3d-point (list (* x scale) (* y scale)))
			txth
		      )
		)
		(vla-put-layer obj "DX")
	      )
	      (progn
		(setq
		  obj (vla-addtext
			*ModelSpace*
			(rtos M11Ass 2 prec)
			(vlax-3d-point (list (* x scale) (* y scale)))
			txth
		      )
		)
		(vla-put-layer obj "GX")
	      )
	    )

	    (if	(>= M22 0)
	      (progn
		(setq
		  obj (vla-addtext
			*ModelSpace*
			(rtos M22Ass 2 prec)
			(vlax-3d-point (list (* x scale) (* y scale)))
			txth
		      )
		)
		(vla-put-layer obj "DY")
		(vla-put-rotation obj (/ pi 2))
	      )
	      (progn
		(setq
		  obj (vla-addtext
			*ModelSpace*
			(rtos M22Ass 2 prec)
			(vlax-3d-point (list (* x scale) (* y scale)))
			txth
		      )
		)
		(vla-put-layer obj "GY")
		(vla-put-rotation obj (/ pi 2))
	      )
	    )

	  )
	)
	(setq cntr (+ cntr 1))
	(setq lin (read-line fil))
      )

      (close fil)
    )
    (alert "Error - File not opened")
  )
  (alert (strcat (itoa cntr) " points created"))
  (setvar "cmdecho" 1)
  (princ)
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
		 (< (abs (atof (cdr (assoc '1 objdata)))) 7.7)
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

(defun c:ac ()

  (if (setq ss (moml:getss))
    (progn
      (setq ssi 0)
      (setq maxvalue 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (> (abs (atof (cdr (assoc '1 objdata)))) maxvalue)
	    )
	  (progn
	    (setq maxvalue (abs (atof (cdr (assoc '1 objdata)))))
	    (setq maxobj obj)
	  )
	)
	(setq ssi (+ ssi 1))
      )
      ;(vla-put-color (vlax-ename->vla-object maxobj) 2)
      (vla-put-lineweight (vlax-ename->vla-object maxobj) acLnWt035)
      ;(print maxvalue)
      ;(princ)
    )
  )
  
  ;(setq M (abs (atof (cdr (assoc 1 (entget (car (entsel))))))))
  (setq Ass maxvalue) 
  
  ;;;offering suggestions

  (setq Ass (- Ass Asmain))
  (if (> Ass 0)
    (progn
   (print (strcat "REQUIRED strengthening " (rtos Asmain 2 2) " + "(rtos Ass 2 2) "cm2/m"))   
     
  (setq diam (list 8 10 12 14 16 20 22 25 28 32 ))
  (setq i 0)
  (setq spos 0)
  (while (nth i diam)
    (setq s 10)
    (while (<= s 20)
      (if (>= (/ (* pi (nth i diam) (nth i diam)) (* 4 s)) Ass)(setq spos s))
      (setq s (+ s 1))
      )
    (if (> spos 0)
      (progn (print (strcat "N" (rtos (nth i diam)) " / " (rtos spos)))
	(princ (rtos (/ (* pi (nth i diam) (nth i diam)) (* 4 spos))))
	)
	)

    (setq i (+ i 1))
    )
  (setq s 10)
  (princ)
)
    (print "No strengthening requiered!")
    )
  
  )