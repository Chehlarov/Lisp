;;;this lisp builds a beam formwork ready for putting rebar
;;;written by Nikolay Chehlarov JAN 2009
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
;;;drafting kota
(defun gr:kota (ip str / pointcc)
  (setq pointcc (vlax-make-safearray vlax-vbDouble '(0 . 7)))
  (vlax-safearray-fill
    pointcc
    (list
      (car ip)
      (+ (cadr ip) 40)

      (car ip)
      (cadr ip)

      (- (car ip) 10)
      (+ (cadr ip) 10)

      (+ (car ip) 58)
      (+ (cadr ip) 10)
    )
  )
  (setq	objl (vla-AddLightWeightPolyline
	       *ModelSpace*
	       pointcc
	     )
  )
  (vla-put-layer objl "KTX")
  ;(setq str (* str 0.01))
  (if (>= str 0)
    (setq str (strcat "+" (rtos str)))
    (setq str (rtos str))
  ) 
  (setq str (vl-string-translate "." "," str))
  (setq pcom (vl-string-position (ascii ",") str))
  (if (= pcom nil)
    (setq str (strcat str ",00"))
    (if	(= (- (strlen str) pcom) 2)
      (setq str (strcat str "0"))
    )
  )
  (if (= pcom 1)
    (setq str (strcat (substr str 1 1) "0" (substr str 2)))
  )
  
  (setq	secttext
	 (vla-addtext
	   *Modelspace*
	   str
	   (vlax-3D-point (list (+ (car ip) 2) (+ (cadr ip) 14)))
	   12
	 )
  )
  (vla-put-layer secttext "KTXTXT")
  (vla-put-stylename secttext "DIM")
  (vla-put-scalefactor secttext 0.85)
)




;;;same as in 2d builder
;;;returns the intersection point of 2 objects
;;;this version returns a list of coordinates for each intersection
(defun gr:intersec (ob1 ob2 / obj1 obj2 int int1 int2 i)
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
  (setq int1 nil)
  (setq i 0)
  ;(print "hereee")
  ;(print (vlax-safearray-get-u-bound (vlax-variant-value int) 1))
  (while (<= i (vlax-safearray-get-u-bound (vlax-variant-value int) 1) )
    (print (nth i (vlax-safearray->list (vlax-variant-value int))))
    (setq int1 (append  int1 (list (list
			      (nth i (vlax-safearray->list (vlax-variant-value int)))
			      (nth (+ i 1) (vlax-safearray->list (vlax-variant-value int)))
			      (nth (+ i 2) (vlax-safearray->list (vlax-variant-value int)))
			      ))
		       )
	  )
    (setq i (+ i 3))
    )
  					
  ;(print int1)
  int1
)

;;;main function
(defun c:gr ()
  (setq ps (getpoint "\nfirst point: "))
  (setq pe (getpoint ps "\nsecond point: "))
  (setq alpha (atan (- (cadr pe) (cadr ps)) (- (car pe) (car ps))))
  (setq	ps (list (- (car ps) (* 1 (cos alpha)))
		 (- (cadr ps) (* 1 (sin alpha)))
	   )
  )
  (setq	pe (list (+ (car pe) (* 1 (cos alpha)))
		 (+ (cadr pe) (* 1 (sin alpha)))
	   )
  )

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
      (setq s1 0)
      (setq s2 0)
      (setq kontur nil)
      (setq h nil)
      (setq heights nil)
      (setq allheights nil)
      (setq dist nil)
     ; (setq gredi nil)
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj))
	(setq layer (cdr (assoc '8 objdat)))
	(if (or	(= layer  "1K_gredi")
		(= layer "1K_kolony")
		(= layer "1K_shajby")
		(= layer "1K_steni")
	    )
	  (progn
	  (setq kontur (append kontur (list obj)))
	  (setq ints (gr:intersec mainline (nth (- (length kontur) 1) kontur)))
	  (if (= (length kontur) 1)
	    (progn
	      (if (< (distance ps (car ints))(distance ps (cadr ints)))
	      (setq firstintoint (car ints))
	      (setq firstintoint (cadr ints))
		)
	      )
	    )
	  ;(setq ssj 0)
	  
	  ;(while (nth ssj ints)
	   ; (setq dist (append dist (list (distance firstintoint (nth ssj ints)))))
	    ;(print "toooo")
	    ;(print dist)
	    ;(setq ssj (+ ssj 1))
	    ;)
	  (setq dist (append dist (list (min (distance firstintoint (nth 0 ints)) (distance firstintoint (nth 1 ints))))))
	  (setq dist (append dist (list (max (distance firstintoint (nth 0 ints)) (distance firstintoint (nth 1 ints))))))
	  ;(print ints)
	  )
	)
	;;;check if section height is available
	(if (= layer "1K_razrez")
	  (progn
	    (setq ints (gr:intersec mainline obj))
	    (if	(= (length ints) 2)
	      (setq h (distance (car ints) (cadr ints)))
	    )
	    (if	(/= (length ints) 2)
	      (progn
		(if (= s1 0)
		  (setq s1 (car ints))
		  (progn
		    (setq s2 (car ints))
		    (setq h (distance s1 s2))
		    (setq s1 0)
		    (setq s2 0)
		  )
		)
	      )
	    )
	  )
	)
	;;; end check if section height is available ;;;
	;;;addin something to the list of heights
	(if (and h (= (rem (length dist) 2) 0))
	  (progn
	  (setq heights (append heights (list h)))
	  (setq h nil)
	  )
	  )

	(if (and (> (length dist) 2)
		 (> (length dist) (+ (* (length heights) 2) 2))
		 )
	  (setq heights (append heights (list 0)))
	  )
	
	(setq ssi (+ ssi 1))
      );;;while (setq obj (ssname ss ssi));;;end
(print heights)
      
;;;getting insertion point for drafting
    (setq ip (getpoint "\ninsertion point: "))
;;;drafting module
;;;drawing outer lwpolyline
(setq pointcc (vlax-make-safearray vlax-vbDouble '(0 . 7)))
(vlax-safearray-fill
  pointcc
  (list
    (car ip)
    (- (cadr ip) 250)

    (car ip)
    (cadr ip)

    (+ (car ip) (nth (- (length dist) 1) dist))
    (cadr ip)

    (+ (car ip) (nth (- (length dist) 1) dist))
    (- (cadr ip) 250)
  )
)
(setq objout (vla-AddLightWeightPolyline
		 *ModelSpace*
		 pointcc
	       )
)
(vla-put-layer objout "GRE")
  ;;;drawing label
 (setq labeltext
		   (vla-addtext
		     *Modelspace*
		     "Гp? - ??/??- 1 бp."
		     (vlax-3D-point
		       (list (- (+ (car ip) (/ (nth (- (length dist) 1) dist) 2) ) 80)
			     (+ (cadr ip) 95)
		       )
		     )
		     13.5
		   )
	    )
	    (vla-put-layer labeltext "GRETXT")
	    (vla-put-stylename labeltext "SWETLI")
      	    (vla-put-scalefactor labeltext 0.75)
      (vla-put-ObliqueAngle labeltext 0.20943951 )
      
  ;;;drawing inner lwpolylines
      (setq i 1)
      (while (nth (+ i 1) dist)
	(setq h (nth (/ (- i 1)2) heights))
	(if (= h 0) (setq h (getreal "enter height")))
	(setq allheights (append allheights (list h)))
	(setq pointcc (vlax-make-safearray vlax-vbDouble '(0 . 7)))
	(vlax-safearray-fill
	  pointcc
	  (list
	    (+ (car ip) (nth i dist))
	    (- (cadr ip) 250)

	    (+ (car ip) (nth i dist))
	    (- (cadr ip) h)

	    (+ (car ip) (nth (+ i 1) dist))
	    (- (cadr ip) h)

	    (+ (car ip) (nth (+ i 1) dist))
	    (- (cadr ip) 250)
	  )
	)
	(setq objint (vla-AddLightWeightPolyline
		       *ModelSpace*
		       pointcc
		     )
	)
	(vla-put-layer objint "GRE")
	(setq i (+ i 2))
      )
;;;drawing main dims
      (setq prev ip)
      (setq i 1)
      (while (nth i dist)
	(setq cur (list	(+ (car ip) (nth i dist))
			(- (cadr ip) 250)
		  )
	)
	(setq objdim
	       (vla-AddDimRotated
		 *ModelSpace*
		 (vlax-3d-point prev)
		 (vlax-3d-point cur)
		 (vlax-3d-point (list (car ip) (- (cadr ip) 120)))
		 0
	       )
	)
	(vla-put-layer objdim "1K_koti_van6ni")
	(vla-put-styleName objdim "RAZR1")
	(setq prev cur)
	;;;drawing red lines and text "1"
	(if (and (= (rem i 2) 1) (/= (- (length dist) 1) i))
	  (progn
	    (setq objred
		   (vla-addline
		     *Modelspace*
		     (vlax-3D-point
		       (list (+ (car ip) (nth i dist) 30) (+ (cadr ip) 5))
		     )
		     (vlax-3D-point
		       (list (+ (car ip) (nth i dist) 30) (+ (cadr ip) 5 12))
		     )
		   )
	    )
	    (vla-put-layer objred "KTX")
	    (vla-mirror
	      objred
	      (vlax-3D-point (list 0 (- (cadr ip) (/ (nth (/ (- i 1)2) allheights) 2))))
	      (vlax-3D-point (list 1 (- (cadr ip) (/ (nth (/ (- i 1)2) allheights) 2))))
	    )

	    (setq secttext
		   (vla-addtext
		     *Modelspace*
		     "1"
		     (vlax-3D-point
		       (list (+ (car ip) (nth i dist) 25) (+ (cadr ip) 7.5))
		     )
		     12
		   )
	    )
	    (vla-put-layer secttext "KTXTXT")
	    (vla-put-stylename secttext "DIM")
	    (vla-put-scalefactor secttext 0.85)
	    (vla-put-rotation secttext (/ pi 2))
	    (vla-mirror
	      secttext
	      (vlax-3D-point (list 0 (- (cadr ip) (/ (nth (/ (- i 1)2) allheights) 2))))
	      (vlax-3D-point (list 1 (- (cadr ip) (/ (nth (/ (- i 1)2) allheights) 2))))
	    )
	  )

	)
;;;drawing red lines and text "1" end;;;

	
	(setq i (+ i 1))
      )

;;;drawing kota
      (setq kota (getreal "Въведи кота: "))
      (gr:kota (list  (+ (car ip) (nth (-(length dist)2) dist) )(cadr ip) ) kota)

;;;end of drafting module
      (vla-delete mainline)
    )
;;;progn
  )
)