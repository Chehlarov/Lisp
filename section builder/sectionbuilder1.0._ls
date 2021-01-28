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
  (list
    (- (+ (car cs) (* (car lcsp) (cos alpha)))
       (* (cadr lcsp) (sin alpha))
    )
    (+ (cadr cs)
       (* (cadr lcsp) (cos alpha))
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
					;(setq obj1 (vlax-ename->vla-object ob1));used if argument is ename
  (setq obj1 ob1)
  (setq obj2 (vlax-ename->vla-object ob2))
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
      (print (sslength ss))
;;;first case only one line
      (if (= (sslength ss) 1)
	(progn
	  (setq hf (getreal "\nEnter hf: "))
	  (setq	mainline (vla-addline
			   *Modelspace*
			   (vlax-3d-point ps)
			   (vlax-3d-point pe)
			 )
	  )
	  (setq ip (sec:intersec mainline (ssname ss 0)))
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
	  (setq objforh (vlax-make-safearray  vlax-vbObject   '(0 . 1)))
	  (vlax-safearray-fill
	    objforh
	    (list
	      objgreen
	      objgrey
	    )
	  )
	  (setq	objh (vla-addhatch
		       *ModelSpace*
		       acHatchPatternTypePredefined
		       "Solid"
		       "FALSE"
		     )
	  )
	  (vla-AppendouterLoop objh objgreen)
	  (vla-evaluate objh)
;;;drawing dimension
	  (setq	objdim
		 (vla-AddDimRotated
		   *ModelSpace*
		   (vlax-3d-point (sec:t (list (- wing) 0) ip))
		   (vlax-3d-point (sec:t (list (- wing) (- hf)) ip))
		   (vlax-3d-point (sec:t (list (- (+ wing 10)) 0) ip))
		   (+ alpha (/ pi 2))
		 )
	  )
	  (vla-put-layer objdim "1K_koti_razrezi")
	  (vla-put-styleName objdim "RAZR1")


	)
;;;progn
      )

;;;second case only two line
      (if (= (sslength ss) 2)
	(print "Hehe wait for newer versions:)")
      )
;;;third case more line
      (if (> (sslength ss) 2)
	(alert "Too many lines croosing!!!")
      )
    )
;;;progn
  )
)