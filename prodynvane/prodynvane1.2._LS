;;;written by Nikolay Chehlarov
;;;sep 2009

(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (setq fck 20);;;in MPA



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
	(vla-put-textstring (nth 7 atttable) (vla-get-length prodob))
	
      )
    )
    (setq ssi (+ ssi 1))
  )
)					;defun pu
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
	  (vla-get-length (vlax-ename->vla-object (handent (vla-get-textstring (nth 12 atttable)))))
	)
      )
      )
    (setq ssi (+ ssi 1))
    )
  );;;end defun c:pupd

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
(defun p:d(hpl arm / d1 t1 d2 t2)
  (setq d1 (atoi (substr arm 1 2)))
  (setq t1 (atoi (substr arm 4 2)))

  (setq d2 (atoi (substr arm 7 2)))
  (setq t2 (atoi (substr arm 10 2)))

  ;;;smqta se pri predpostavka 4e pokritieto e 1.5cm
  (setq d (/ (- (* 2 hpl) 3 (* 0.15 (max d1 d2)) (* 0.05 (min d1 d2)) ) 2 ))
  );;;end defun p:d

(defun c:pcal()
  (setq N 225);;;tova 6te se 4ete ot fail sega e samo za debugvane
  (setq obj (car (entsel "Select column to calculate")))
  (setq obja (vlax-ename->vla-object obj))

  (setq atttable (vlax-safearray->list (vlax-variant-value (vla-getattributes obja))))

  ; getting dimensions of column
  (setq a (vla-get-textstring (nth 2 atttable))) 
  (setq b (vla-get-textstring (nth 3 atttable)))

  (setq beta (atof (vla-get-textstring (nth 8 atttable))))

  (setq hpl (atof (vla-get-textstring (nth 1 atttable))))

  (setq u1 (atof (vla-get-textstring (nth 7 atttable))))

  (setq arm (vla-get-textstring (nth 6 atttable)))
  
  (setq d1 (atoi (substr arm 1 2)))
  (setq t1 (atoi (substr arm 4 2)))

  (setq d2 (atoi (substr arm 7 2)))
  (setq t2 (atoi (substr arm 10 2)))

  (setq d (p:d hpl arm))

  
  (setq rol (sqrt (*
		    (/ (/ (* pi d1 d1) 400) t1 d)
		  (/ (/ (* pi d2 d2) 400) t2 d)
		    )
		  )
	)
  (setq rol (min rol 0.02))
  (setq vRdc (p:vRdc d rol))
  (setq vEd (* (/ (* beta N ) (* d u1)) 10 ))
  (print (/ vEd vRdc))
  )