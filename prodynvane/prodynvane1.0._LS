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

(defun c:du1()
  (setq obj (car (entsel "Pick up column")))
  (print)
  (setq d (getreal "d= "))
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
  );;;defun c:du1

(defun p:vRdc(d rol / k vRdc vRdcmin)

  (setq k (min (+ 1 (sqrt 20 d)) 2)) ;;;d in cm
  
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

(defun c:pcal()
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

  (setq d (/ (- (* 2 hpl) 3 (* 0.15 (max d1 d2)) (* 0.05 (min d1 d2)) ) 2 ))

  
  (setq rol (sqrt (*
		    (/ (/ (* pi d1 d1) 400) t1 d)
		  (/ (/ (* pi d2 d2) 400) t2 d)
		    )
		  )
	)
  (setq rol (min rol 0.02))
  (p:vRdc d rol)
  
  )