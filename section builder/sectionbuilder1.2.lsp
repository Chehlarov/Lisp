;;;this lisp builds a partial cross section
;;;written by Nikolay Chehlarov July 2008
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
;;;transforms local coordinates to WCS
(defun sec:t (lcsp cs)
;;; 1 dqsna ks
;;; -1 lqva ks
  (setq tks 1)
  (if (> alpha (/ pi 2))
    (setq tks -1)
  )
  (if (<= alpha (- (/ pi 2)))
    (setq tks -1)
  )
  (list
    (- (+ (car cs) (* (car lcsp) (cos alpha)))
       (* (cadr lcsp) (sin alpha) tks)
    )
    (+ (cadr cs)
       (* (cadr lcsp) (cos alpha) tks)
       (* (car lcsp) (sin alpha))
    )
  )
)

;;;defines wing length
(defun sec:wing	(num)
  (setq num (* num 2))
  (if (> (rem num 5) 0)
    (setq num (+ (- num (rem num 5)) 5))
  )
  (setq num (max 40 num))
  num
)

;;;same as in 2d builder
;;;returns the intersection point of 2 objects
(defun sec:intersec (ob1 ob2 / obj1 obj2 int int1 int2)
  (print ob1)
					;(setq obj1 (vlax-ename->vla-object ob1));used if argument is ename
  (if (= (type ob1) 'ENAME)
    (setq obj1 (vlax-ename->vla-object ob1))
    (setq obj1 ob1)
  )
  (if (= (type ob2) 'ENAME)
    (setq obj2 (vlax-ename->vla-object ob2))
    (setq obj2 ob2)
  )

  (setq	int (vla-IntersectWith
	      obj1
	      obj2
	      acExtendNone
	    )
  )
					; (print(vlax-variant-type int))
  (if (/= (vlax-safearray-get-u-bound (vlax-variant-value int) 1)
	  -1
      )
    (progn
      (setq
	int1 (list
	       (car (vlax-safearray->list (vlax-variant-value int)))
	       (cadr (vlax-safearray->list (vlax-variant-value int)))
	       (caddr (vlax-safearray->list (vlax-variant-value int)))
	     )
      )
					;  (setq
					;	int2 (list
					;	       (nth 3 (vlax-safearray->list (vlax-variant-value int)))
					;	       (nth 4 (vlax-safearray->list (vlax-variant-value int)))
					;	       (nth 5 (vlax-safearray->list (vlax-variant-value int)))
					;	     )
					;     )
      int1
    )
  )
)

;;;main function
(defun c:cs ()
  (setq ps (getpoint "\nfirst point: "))
  (setq pe (getpoint ps "\nsecond point: "))
  (setq alpha (atan (- (cadr pe) (cadr ps)) (- (car pe) (car ps))))

  (if (setq ss (ssget "F" (list ps pe)))
    (progn
					;(print (sslength ss))
      (setq mainline (vla-addline
		       *Modelspace*
		       (vlax-3d-point ps)
		       (vlax-3d-point pe)
		     )
      )
      (setq ssi 0)
      (setq kontur 0)
      (setq gredi nil)
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj))
	(setq layer (cdr (assoc '8 objdat)))
	(if (or	(= (substr layer 1 9) "1K_kontur")
		(= (substr layer 1 9) "1k_kontur")
		(=  layer "1K_dupki")
		(=  layer "1K_stalbi")
	    )
	  (setq kontur obj)
	)
	(if (=  layer "1K_gredi")
	  (setq gredi (append gredi (list obj)))
	)
	(setq ssi (+ ssi 1))
      )
      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;first case only one line
      (if (and (/= kontur 0)
	       (= gredi nil)
	       )
	(progn
	  (setq hf (getreal "\nEnter hf: "))

	  (setq ip (sec:intersec mainline kontur))
	  (setq wing (sec:wing hf))
;;;drawing green lwpolyline
	  (setq pointcc (vlax-make-safearray vlax-vbDouble '(0 . 7)))
	  (vlax-safearray-fill
	    pointcc
	    (list
	      (car (sec:t (list (- wing) 0) ip))
	      (cadr (sec:t (list (- wing) 0) ip))

	      (car (sec:t (list 0 0) ip))
	      (cadr (sec:t (list 0 0) ip))

	      (car (sec:t (list 0 (- hf)) ip))
	      (cadr (sec:t (list 0 (- hf)) ip))

	      (car (sec:t (list (- wing) (- hf)) ip))
	      (cadr (sec:t (list (- wing) (- hf)) ip))
	    )
	  )
	  (setq	objgreen (vla-AddLightWeightPolyline
			   *ModelSpace*
			   pointcc
			 )
	  )
	  (vla-put-layer objgreen "1K_razrez")
;;;drawin grey line
	  (setq	objgrey
		 (vla-addline
		   *Modelspace*
		   (vlax-3D-point (sec:t (list (- wing) 0) ip))
		   (vlax-3D-point (sec:t (list (- wing) (- hf)) ip))
		 )
	  )
	  (vla-put-layer objgrey "1K_razrez-solid")
;;;drawing hatch
	  (setq hboun (vlax-make-safearray vlax-vbObject '(0 . 1)))
	  (vlax-safearray-put-element hboun 0 objgreen)
	  (vlax-safearray-put-element hboun 1 objgrey)
	  (setq	objh (vla-addhatch
		       *ModelSpace*
		       acHatchPatternTypePredefined
		       "Solid"
		       "TRUE"
		     )
	  )
	  (vla-AppendouterLoop objh hboun)
	  (vla-evaluate objh)
	  (vla-put-layer objh "1K_razrez-solid")
;;;drawing dimension
	  (setq	objdim
		 (vla-AddDimRotated
		   *ModelSpace*
		   (vlax-3d-point (sec:t (list (- wing) 0) ip))
		   (vlax-3d-point (sec:t (list (- wing) (- hf)) ip))
		   (vlax-3d-point (sec:t (list (- (+ wing 20)) 0) ip))
		   (+ alpha (/ pi 2))
		 )
	  )
	  (vla-put-layer objdim "1K_koti_razrezi")
	  (vla-put-styleName objdim "RAZR1")

;;;deleting the main line
	  (vla-delete mainline)
	)
;;;progn
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;second case only two line
      (if (and (= (length gredi) 2)
	       (= kontur 0)
	     )
	(progn
	  (setq b1 (vlax-ename->vla-object (car gredi)))
	  (setq b2 (vlax-ename->vla-object (cadr gredi)))
	  (if (and (= (vla-get-linetype b1) "ByLayer")
		   (= (vla-get-linetype b2) "ByLayer")
	      )
	    (progn
	      (setq hf (getreal "\nEnter hf: "))
	      (setq h (getreal "\nEnter h: "))
	      (setq ip1 (sec:intersec mainline b1))
	      (setq ip2 (sec:intersec mainline b2))
	      (setq wing 25)
;;;drawing long green lwpolyline
	      (setq
		pointcc	(vlax-make-safearray vlax-vbDouble '(0 . 11))
	      )
	      (vlax-safearray-fill
		pointcc
		(list
		  (car (sec:t (list (- wing) (- hf)) ip1))
		  (cadr (sec:t (list (- wing) (- hf)) ip1))

		  (car (sec:t (list 0 (- hf)) ip1))
		  (cadr (sec:t (list 0 (- hf)) ip1))

		  (car (sec:t (list 0 (- h)) ip1))
		  (cadr (sec:t (list 0 (- h)) ip1))

		  (car (sec:t (list (distance ip1 ip2) (- h)) ip1))
		  (cadr (sec:t (list (distance ip1 ip2) (- h)) ip1))

		  (car (sec:t (list (distance ip1 ip2) (- hf)) ip1))
		  (cadr (sec:t (list (distance ip1 ip2) (- hf)) ip1))

		  (car (sec:t (list (+ (distance ip1 ip2) wing) (- hf))
			      ip1
		       )
		  )
		  (cadr	(sec:t (list (+ (distance ip1 ip2) wing) (- hf))
			       ip1
			)
		  )
		)
	      )
	      (setq objgreenl (vla-AddLightWeightPolyline
				*ModelSpace*
				pointcc
			      )
	      )
	      (vla-put-layer objgreenl "1K_razrez")
;;;drawing short pw
	      (setq
		pointcc	(vlax-make-safearray vlax-vbDouble '(0 . 3))
	      )
	      (vlax-safearray-fill
		pointcc
		(list
		  (car (sec:t (list (- wing) 0) ip1))
		  (cadr (sec:t (list (- wing) 0) ip1))

		  (car (sec:t (list (+ (distance ip1 ip2) wing) 0) ip1))
		  (cadr	(sec:t (list (+ (distance ip1 ip2) wing) 0) ip1)
		  )
		)
	      )
	      (setq objgreens (vla-AddLightWeightPolyline
				*ModelSpace*
				pointcc
			      )
	      )
	      (vla-put-layer objgreens "1K_razrez")
;;;drawing grey line
	      (setq objgrey1
		     (vla-addline
		       *Modelspace*
		       (vlax-3D-point (sec:t (list (- wing) 0) ip1))
		       (vlax-3D-point (sec:t (list (- wing) (- hf)) ip1))
		     )
	      )
	      (setq objgrey2
		     (vla-addline
		       *Modelspace*
		       (vlax-3D-point
			 (sec:t (list (+ (distance ip1 ip2) wing) 0) ip1)
		       )
		       (vlax-3D-point
			 (sec:t (list (+ (distance ip1 ip2) wing) (- hf)) ip1)
		       )
		     )
	      )
	      (vla-put-layer objgrey1 "1K_razrez-solid")
	      (vla-put-layer objgrey2 "1K_razrez-solid")
;;;drawing hatch
	      (setq hboun (vlax-make-safearray vlax-vbObject '(0 . 3)))
	      (vlax-safearray-put-element hboun 0 objgreenl)
	      (vlax-safearray-put-element hboun 1 objgreens)
	      (vlax-safearray-put-element hboun 2 objgrey1)
	      (vlax-safearray-put-element hboun 3 objgrey2)
	      (setq objh (vla-addhatch
			   *ModelSpace*
			   acHatchPatternTypePredefined
			   "Solid"
			   "TRUE"
			 )
	      )
	      (vla-AppendouterLoop objh hboun)
	      (vla-evaluate objh)
	      (vla-put-layer objh "1K_razrez-solid")
;;;drawing dimension
	      (setq objdim
		     (vla-AddDimRotated
		       *ModelSpace*
		       (vlax-3d-point (sec:t (list (- wing) 0) ip1))
		       (vlax-3d-point (sec:t (list (- wing) (- hf)) ip1))
		       (vlax-3d-point (sec:t (list (- 0 wing 20) 0) ip1))
		       (+ alpha (/ pi 2))
		     )
	      )
	      (vla-put-layer objdim "1K_koti_razrezi")
	      (vla-put-styleName objdim "RAZR1")
	      (setq objdim
		     (vla-AddDimRotated
		       *ModelSpace*
		       (vlax-3d-point (sec:t (list (- wing) (- hf)) ip1))
		       (vlax-3d-point (sec:t (list 0 (- h)) ip1))
		       (vlax-3d-point (sec:t (list (- 0 wing 20) 0) ip1))
		       (+ alpha (/ pi 2))
		     )
	      )
	      (vla-put-layer objdim "1K_koti_razrezi")
	      (vla-put-styleName objdim "RAZR1")
	      (setq objdim
		     (vla-AddDimRotated
		       *ModelSpace*
		       (vlax-3d-point (sec:t (list (- wing) 0) ip1))
		       (vlax-3d-point (sec:t (list 0 (- h)) ip1))
		       (vlax-3d-point (sec:t (list (- 0 wing 40) 0) ip1))
		       (+ alpha (/ pi 2))
		     )
	      )
	      (vla-put-layer objdim "1K_koti_razrezi")
	      (vla-put-styleName objdim "RAZR1")

;;;deleting the main line
	      (vla-delete mainline)
;;;
	    )
	  )
	)
      )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;third case G section
(if (and (or (= (length gredi) 1)
	     (= (length gredi) 2)
	     )
	       (/= kontur 0)
	     )
	(progn
	  (setq b1 (vlax-ename->vla-object (car gredi)))
	  (if (= (length gredi) 2) (setq b2 (vlax-ename->vla-object (cadr gredi))))
	  (setq k1 (vlax-ename->vla-object kontur))
	  (if (and (= (vla-get-linetype b1) "ByLayer")
		   (= (vla-get-linetype b2) "ByLayer")
	      )
	    (progn
	      (setq hf (getreal "\nEnter hf: "))
	      (setq h (getreal "\nEnter h: "))
	      (setq ipb1 (sec:intersec mainline b1))
	      (setq ipb2 (sec:intersec mainline b2))
	      (setq ipk1 (sec:intersec mainline k1))
	      (if (< (distance ipb1 ipk1) 2)
		(setq
		  ip1 ipb2
		  ip2 ipb1)
		)
	      (if (< (distance ipb2 ipk1) 2)
		(setq
		  ip1 ipb1
		  ip2 ipb2
		  alpha (- alpha))
		)
	      (if (and (not (type ip1)) (not (type ip2)))
		(progn
		  (alert "bad section!")
		  (exit)))
	      (setq wing 25)
;;;drawing  green lwpolyline
	      (setq
		pointcc	(vlax-make-safearray vlax-vbDouble '(0 . 11))
	      )
	      (vlax-safearray-fill
		pointcc
		(list
		  (car (sec:t (list wing 0) ip1))
		  (cadr (sec:t (list wing 0) ip1))

		  (car (sec:t (list (- (distance ip1 ip2)) 0) ip1))
		  (cadr (sec:t (list (- (distance ip1 ip2)) 0) ip1))

		  (car (sec:t (list (- (distance ip1 ip2)) (- h)) ip1))
		  (cadr (sec:t (list (- (distance ip1 ip2)) (- h)) ip1))

		  (car (sec:t (list 0 (- h)) ip1))
		  (cadr (sec:t (list 0 (- h)) ip1))

		  (car (sec:t (list 0 (- hf)) ip1))
		  (cadr (sec:t (list 0 (- hf)) ip1))

		  (car (sec:t (list wing (- hf)) ip1))
		  (cadr (sec:t (list wing (- hf)) ip1))
		)
	      )
	      (setq objgreenl (vla-AddLightWeightPolyline
				*ModelSpace*
				pointcc
			      )
	      )
	      (vla-put-layer objgreenl "1K_razrez")

;;;drawing grey line
	      (setq objgrey1
		     (vla-addline
		       *Modelspace*
		       (vlax-3D-point (sec:t (list wing 0) ip1))
		       (vlax-3D-point (sec:t (list wing (- hf)) ip1))
		     )
	      )
	      
	      (vla-put-layer objgrey1 "1K_razrez-solid")
;;;drawing hatch
	      (setq hboun (vlax-make-safearray vlax-vbObject '(0 . 1)))
	      (vlax-safearray-put-element hboun 0 objgreenl)      
	      (vlax-safearray-put-element hboun 1 objgrey1)
	      (setq objh (vla-addhatch
			   *ModelSpace*
			   acHatchPatternTypePredefined
			   "Solid"
			   "TRUE"
			 )
	      )
	      (vla-AppendouterLoop objh hboun)
	      (vla-evaluate objh)
	      (vla-put-layer objh "1K_razrez-solid")
;;;drawing dimension
	      (setq objdim
		     (vla-AddDimRotated
		       *ModelSpace*
		       (vlax-3d-point (sec:t (list wing 0) ip1))
		       (vlax-3d-point (sec:t (list wing (- hf)) ip1))
		       (vlax-3d-point (sec:t (list (+ wing 20) (- hf)) ip1))
		       (+ alpha (/ pi 2))
		     )
	      )
	      (vla-put-layer objdim "1K_koti_razrezi")
	      (vla-put-styleName objdim "RAZR1")
	      (setq objdim
		     (vla-AddDimRotated
		       *ModelSpace*
		       (vlax-3d-point (sec:t (list wing (- hf)) ip1))
		       (vlax-3d-point (sec:t (list 0 (- h)) ip1))
		       (vlax-3d-point (sec:t (list (+ wing 20) (- hf)) ip1))
		       (+ alpha (/ pi 2))
		     )
	      )
	      (vla-put-layer objdim "1K_koti_razrezi")
	      (vla-put-styleName objdim "RAZR1")
	      (setq objdim
		     (vla-AddDimRotated
		       *ModelSpace*
		       (vlax-3d-point (sec:t (list wing 0) ip1))
		       (vlax-3d-point (sec:t (list 0 (- h)) ip1))
		       (vlax-3d-point (sec:t (list (+ wing 40) (- hf)) ip1))
		       (+ alpha (/ pi 2))
		     )
	      )
	      (vla-put-layer objdim "1K_koti_razrezi")
	      (vla-put-styleName objdim "RAZR1")

;;;deleting the main line
	      (vla-delete mainline)
;;;
	    )
	  )
	)
      )
      ;;;
      (if (> (length gredi) 2)
	(alert "Too many lines croosing!!!")
      )
    )
;;;progn
  )
)