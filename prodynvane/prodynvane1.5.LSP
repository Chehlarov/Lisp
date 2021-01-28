;;;written by Nikolay Chehlarov
;;;sep 2009

(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq pathin "D:/Nikolay3/GRAND/RubinII/plochi_prodynvane/P.txt")
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq fck 20);;;in MPA
  (setq fyk 500)



  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun p:getss (/ ss)
  (setq ss (ssget))
)
;;;pomo6tna funkciq za iz4ertavane perimetyra na prodynvane
(defun p:od (i1 i2 coo di / p1 p2)
  ;;;note i1 and i2 start from 0
  (setq p1 (list (nth (* 2 i1) coo) (nth (+ (* 2 i1) 1) coo)) )
  (setq p2 (list (nth (* 2 i2) coo) (nth (+ (* 2 i2) 1) coo)) )
  
  (setq  alpha (atan
	       (- (cadr p2) (cadr p1))
	       (- (car p2) (car p1))
	       )
	)
  (list
    (+ (car p2) (* di (cos alpha)))
    (+ (cadr p2) (* di (sin alpha)))
    )
  );;;end defun


;;;funkciq za iz4ertavane na perimetyra na prodynvane
(defun p:du1(obj d)
  ;(setq obj (car (entsel "Pick up column")))
  ;(print)
  ;(setq d (getreal "d= "))
  ;;; v zavisimost ot razstoqnieto dali e + ili - ofsetva nadqsno ili nalqvo ot posokata na vyrtene
  ;(setq obju1 (vla-offset (vlax-ename->vla-object obj) (* -2 d)))
  ;;;obju1 e array i trqbva da se vzeme ediiq element osven tova trqbva da se razbie na pove4e to4ki

  (setq obja (vlax-ename->vla-object obj))
  (setq coordi (vla-get-Coordinates obja))
  (setq cc (vlax-safearray->list (vlax-variant-value coordi)))
  (if (/= (length cc) 8) (alert "Apply online on rectangular section!"))
  (setq coordiu1 (vlax-make-safearray vlax-vbDouble '(0 . 15)))
  (vlax-safearray-fill coordiu1
	 (list
	   (car (p:od 0 1 cc d))
	   (cadr (p:od 0 1 cc d))

	   (car (p:od 3 2 cc d))
	   (cadr (p:od 3 2 cc d))

	   (car (p:od 1 2 cc d))
	   (cadr (p:od 1 2 cc d))

	   (car (p:od 0 3 cc d))
	   (cadr (p:od 0 3 cc d))

	   (car (p:od 2 3 cc d))
	   (cadr (p:od 2 3 cc d))

	   (car (p:od 1 0 cc d))
	   (cadr (p:od 1 0 cc d))

	   (car (p:od 3 0 cc d))
	   (cadr (p:od 3 0 cc d))

	   (car (p:od 2 1 cc d))
	   (cadr (p:od 2 1 cc d))
	 )
	)
  (setq	obju1 (vla-AddLightWeightPolyline
	       *ModelSpace*
	       coordiu1
	     )
  )
  (vla-put-closed obju1 :vlax-true)
  (setq basarea (vla-get-area obju1))
  (vla-setbulge obju1 1 (+ (/ (sin (/ pi 8)) (cos (/ pi 8)))))
  (setq basareanew (vla-get-area obju1))
  (if (> basareanew basarea)
    (progn
      (vla-setbulge obju1 1 (+ (/ (sin (/ pi 8)) (cos (/ pi 8)))))
      (vla-setbulge obju1 3 (+ (/ (sin (/ pi 8)) (cos (/ pi 8)))))
      (vla-setbulge obju1 5 (+ (/ (sin (/ pi 8)) (cos (/ pi 8)))))
      (vla-setbulge obju1 7 (+ (/ (sin (/ pi 8)) (cos (/ pi 8)))))
    )
;;;else
    (progn
      (vla-setbulge obju1 1 (- (/ (sin (/ pi 8)) (cos (/ pi 8)))))
      (vla-setbulge obju1 3 (- (/ (sin (/ pi 8)) (cos (/ pi 8)))))
      (vla-setbulge obju1 5 (- (/ (sin (/ pi 8)) (cos (/ pi 8)))))
      (vla-setbulge obju1 7 (- (/ (sin (/ pi 8)) (cos (/ pi 8)))))
    )
  )
  obju1
  );;;defun c:du1


;;;function to draw all perimeters, assign their handles and update u1
(defun c:pu ()
  (setq ss (p:getss))
  (setq ssi 0)
  (while (setq obj (ssname ss ssi))
    (setq objdata (entget obj))
    (if	(and (= (cdr (assoc '0 objdata)) "INSERT")
	     (= (cdr (assoc '2 objdata)) "velem")
	)
      (progn
	(setq obja (vlax-ename->vla-object obj))
	(setq atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes obja))
		       )
	      
	)
	(setq hpl (atof (vla-get-textstring (nth 1 atttable))))
	(setq arm (vla-get-textstring (nth 6 atttable)))
	
	(setq prodob (p:du1
		       (handent (vla-get-textstring (nth 0 atttable)))
		       (* (p:d hpl arm) 2)
		     )
	)
	(vla-put-textstring (nth 12 atttable) (cdr (assoc 5 (entget (vlax-vla-object->ename prodob)))))
	(vla-put-textstring (nth 7 atttable) (rtos (vla-get-length prodob) 2 2))
	
      )
    )
    (setq ssi (+ ssi 1))
  )
)
  ;defun pu
;;;funkciq koqto samo updatva perimetyra sled predpolagaem izrqzvane
(defun c:pupd()
    (setq ss (p:getss))
  (setq ssi 0)
  (while (setq obj (ssname ss ssi))
    (setq objdata (entget obj))
    (if	(and (= (cdr (assoc '0 objdata)) "INSERT")
	     (= (cdr (assoc '2 objdata)) "velem")
	)
      (progn
	(setq obja (vlax-ename->vla-object obj))
	(setq atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes obja))
		       )

	)
	
	(vla-put-textstring
	  (nth 7 atttable)
	  (rtos (vla-get-length (vlax-ename->vla-object (handent (vla-get-textstring (nth 12 atttable))))) 2 2)
	)
      )
      )
    (setq ssi (+ ssi 1))
    )
  );;;end defun c:pupd
(defun c:ploadn	()
  (setq Ndat nil)
  (setq infile (strcat pathin))
  (setq f (open infile "r"))
  (while (setq lin (read-line f))
    (setq knum (read lin))
    (setq knum (vl-string-left-trim "K" knum))
    (setq knum (vl-string-left-trim "Ê" knum))
    (setq knum (atoi knum))
    (vl-string-position (ascii "\t") lin)
    (setq N
	   (max	0
		(atof
		  (substr lin (+ (vl-string-position (ascii "\t") lin) 2))
		)
	   )
    )
    (setq Ndat (append Ndat (list (cons knum N))))
  )
  (close f)

  (setq ss (p:getss))
  (setq ssi 0)
  (while (setq obj (ssname ss ssi))
    (setq objdata (entget obj))
    (if	(and (= (cdr (assoc '0 objdata)) "INSERT")
	     (= (cdr (assoc '2 objdata)) "velem")
	)
      (progn
	(setq obja (vlax-ename->vla-object obj))
	(setq atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes obja))
		       )

	)
	(if
	  (or (= (substr (vla-get-textstring (nth 11 atttable)) 1 1))
	      (= (substr (vla-get-textstring (nth 11 atttable)) 1 1))
	  )


	   (vla-put-textstring
	     (nth 13 atttable)
	     (cdr (assoc (atoi (vla-get-textstring (nth 10 atttable))) Ndat))
	   )
	)
      )
    )
    (setq ssi (+ ssi 1))
  )
)




;;;return in MPA
(defun p:vRdc(d rol / k vRdc vRdcmin)

  (setq k (min (+ 1 (sqrt (/ 20 d))) 2)) ;;;d in cm
  
  (setq vRdc (*
	       (/ 0.18 1.5)
	       k
	       (expt (* 100 fck rol) 0.333333)
	       )
  )
  (setq vRdcmin (*
		  0.035
		  (expt k 1.5)
		  (sqrt fck)
		 )
	)
  (setq vRdc (max vRdc vRdcmin))
  );;;defun
(defun p:d(hpl arm / d1 t1 d2 t2 h1 h2 h)
  (setq d1 (* (atoi (substr arm 1 2)) 0.1))
  (setq t1 (atoi (substr arm 4 2)))

  (setq d2 (* (atoi (substr arm 7 2)) 0.1))
  (setq t2 (atoi (substr arm 10 2)))

  (setq h1 (/ (- (* 2 hpl) (max d1 1.5) (* 0.5 d1) (max d1 1.5) d1 (* 0.5 d2)) 2 ))
  (setq h2 (/ (- (* 2 hpl) (max d2 1.5) (* 0.5 d2) (max d2 1.5) d2 (* 0.5 d1)) 2 ))
  ;;;smqta se pri predpostavka 4e pokritieto e 1.5cm
  (setq h (min h1 h2))
  );;;end defun p:d

(defun c:pcal()

  (setq obj (car (entsel "Select column to calculate")))
  (setq obja (vlax-ename->vla-object obj))

  (setq atttable (vlax-safearray->list (vlax-variant-value (vla-getattributes obja))))



  (setq N (atof (vla-get-textstring (nth 13 atttable))))

  (setq objcolumn  (handent (vla-get-textstring (nth 0 atttable))))

  (setq obju1  (handent (vla-get-textstring (nth 12 atttable))))
  
  (setq a (atof (vla-get-textstring (nth 2 atttable))) )
  (setq b (atof (vla-get-textstring (nth 3 atttable))))

  (setq beta (atof (vla-get-textstring (nth 8 atttable))))

  (setq hpl (atof (vla-get-textstring (nth 1 atttable))))

  (setq u1 (atof (vla-get-textstring (nth 7 atttable))))

  (setq arm (vla-get-textstring (nth 6 atttable)))
  
  (setq d1 (atoi (substr arm 1 2)))
  (setq t1 (atoi (substr arm 4 2)))

  (setq d2 (atoi (substr arm 7 2)))
  (setq t2 (atoi (substr arm 10 2)))

  (setq d (p:d hpl arm))

  (if (= (strlen arm) 11)
  (setq rol (sqrt (*
		    (/ (/ (* pi d1 d1) 400) t1 d)
		  (/ (/ (* pi d2 d2) 400) t2 d)
		    )
		  )
	)
    )
  (if (> (strlen arm) 11)
    (progn
    (setq sqcm (substr arm 13))
    (setq asx (read sqcm))
    (setq asy (read (vl-string-left-trim (rtos asx) sqcm)))
      (setq rol	(sqrt (*
			(/ asx d 100)
			(/ asy d 100)
		      )
		)
      )
    )
  )

  
  (setq rol (min rol 0.02))
  (print)
  (print rol)
  (setq vRdc (p:vRdc d rol))
  (setq vEd (* (/ (* beta N ) (* d u1)) 10 ))
  (setq ratio (/ vEd vRdc))
  (print "d= ")
  (princ d)
  (print ratio)
  (vla-put-textstring (nth 14 atttable) (rtos vEd 2 3))
  (vla-put-textstring (nth 15 atttable) (rtos vRdc 2 3))
  (vla-put-textstring (nth 16 atttable) (rtos ratio 2 3))
  (if (<= ratio 1)(vla-put-color (nth 16 atttable) 3))
  (if (> ratio 1)(vla-put-color (nth 16 atttable) 1))
  ;;;;second part reinforcement required
  (if (> ratio 1)
    (progn
      (print "Reinforcement required!")
      (print (strcat "d= " (rtos d 2 2)))
      (initget "Inernal Edge Corner")
      (setq ctype (getkword "\nEnter column type Inernal Edge Corner"))
      (if (= ctype "Inernal")
	(setq u0 (* 2 (+ a b)))
      )
      (if (= ctype "Edge")
	(setq u0 (min (+ a (* 3 d))
		      (+ a (* 2 b))
		 )
	)
      )
      (if (= ctype "Corner")
	(setq u0 (min (* 3 d)
		      (+ a b)
		 )
	)
      )
      (print (strcat "(Please check) u0= " (rtos u0 2 2)))
      (setq ved0 (/ (* beta N 10)(* u0 d)))
      (setq vRdmax (* 0.5 0.6 (- 1 (* fck 0.004)) (/ fck 1.5) ))
      (print (strcat "ved0= " (rtos ved0 2 3)))
      (print (strcat "vRdmax= " (rtos vRdmax 2 3)))
      (if (> ved0 vRdmax) (print "vRdmax > ved0 Increase column dimensions or d!!!"))

      (setq uoutef (/ (* beta N 10)(* vRdc d)))
      (print (strcat "uoutef= " (rtos uoutef 2 3)))

      ;;;finding write offset distance
      (setq l1 (vla-get-length (vlax-ename->vla-object obju1)))
      (setq objtest (car(vlax-safearray->list(vlax-variant-value (vla-offset (vlax-ename->vla-object obju1) 20)))))
      (setq l2 (vla-get-length  objtest))
      
      (setq klinear (/ (- l2 l1) 20))
      (setq xfroml2  (/ (- uoutef l2) klinear) )
      (setq objuoutef (car(vlax-safearray->list(vlax-variant-value (vla-offset  objtest xfroml2)))))
      (vla-put-color objuoutef acgreen)
      (vla-delete objtest)
      (if (> uoutef (+ (vla-get-length objuoutef) 0.1)) (alert "manually increase uouteff"))
      (if (>= xfroml2 0)
      (setq xouteff (+ (+ (* 2 d) 20) (abs xfroml2)))
	(setq xouteff (+ (- (* 2 d) 20) (abs xfroml2)))
	)
      ;(print xouteff)

      

      (setq sr (* 0.75 d))
      (setq st (* 1.5 d))
      (setq aswmin (/ (* (/ 0.08 1.5) (sqrt fck) sr st) fyk) )
      (print (strcat "aswmin per link (within 2d)= " (rtos aswmin 2 3) "sqcm"))

      (setq fydef (min (/ fyk 1.15) (+ 250 (* 0.25 d 10)) ))
      (setq asw (/ (- vEd (* 0.75 vRdc)) (/ (* 1.5 fydef) sr u1)))
      (print (strcat "asw for each perimeter " (rtos asw 2 3) "sqcm"))
;;;drawing perimeters with reinforcement first 2 are required
      (setq objper (p:du1 objcolumn (* 0.5 d)))
      (vla-put-color objper acblue)
      
     (setq objper (p:du1 objcolumn (* 1.25 d)))
     (vla-put-color objper acblue)

      (setq prad (* 2 d))
    
      (while (< (+ prad (* 0.75 d)) xouteff)
	(setq objper (p:du1 objcolumn prad))
	(vla-put-color objper acblue)
	(setq prad (+ prad (* 0.75 d)))
      )

      
    )
  )
  ;;;; end of second part reinforcement required
  )




(defun c:parm ()

  (if (setq ss (p:getss))
    (progn
      (setq ssi 0)
      (setq maxvaluex 0)
      (setq maxvaluey 0)
      (setq atttable nil)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (= (cdr (assoc '8 objdata)) "GX")
		 (> (abs (atof (cdr (assoc '1 objdata)))) maxvaluex)
	    )
	  (progn
	    (setq maxvaluex (abs (atof (cdr (assoc '1 objdata)))))
	    (setq maxobjx obj)
	  )
	)
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (= (cdr (assoc '8 objdata)) "GY")
		 (> (abs (atof (cdr (assoc '1 objdata)))) maxvaluey)
	    )
	  (progn
	    (setq maxvaluey (abs (atof (cdr (assoc '1 objdata)))))
	    (setq maxobjy obj)
	  )
	)
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "velem")
	    )
	  (progn
	  (setq obja (vlax-ename->vla-object obj))
	  (setq
	    atttable (vlax-safearray->list
		       (vlax-variant-value (vla-getattributes obja))
		     )

	  )
	  )
	)
	(setq ssi (+ ssi 1))
      )
      ;(vla-put-color (vlax-ename->vla-object maxobj) 2)
      
      ;(print maxvalue)
      ;(princ)
    )
  )

  ;;;po X
    (setq Ass maxvaluex)
  (setq diam (list 12 14 16 18 20 22))
  (setq i 0)
  (setq strx "")
  (while (nth i diam)
    (setq s 20)
      (if (>= (/ (* pi (nth i diam) (nth i diam)) (* 4 s)) Ass)
	(progn
	(setq strx (strcat (rtos (nth i diam)) "t20" ))
	(setq i 20);;za da izleze
	)
	)
    (setq i (+ i 1))
    )
  (if (= strx "") (setq strx (strcat (rtos (nth (- i 1) diam)) "t20" )) )

  ;;;po Y
  (setq Ass maxvaluey)
    (setq i 0)
  (setq stry "")
  (while (nth i diam)
    (setq s 20)
      (if (>= (/ (* pi (nth i diam) (nth i diam)) (* 4 s)) Ass)
	(progn
	(setq stry (strcat (rtos (nth i diam)) "t20" ))
	(setq i 20);;za da izleze
	)
	)
    (setq i (+ i 1))
    )
  (if (= stry "") (setq stry (strcat (rtos (nth (- i 1) diam)) "t20" )) )
  (vla-put-textstring (nth 6 atttable) (strcat strx "," stry " " (rtos maxvaluex) " " (rtos maxvaluey) ))
  

    
    
  
  );;;end defun c:parm