;;;this lisp builds a continous dimension
;;;written by Nikolay Chehlarov JAN 2009
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

;;;returns the intersection point of 2 objects
;;;this version returns a list of coordinates for each intersection
(defun sec:intersec (ob1 ob2 / obj1 obj2 int int1 int2 i)
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
(defun c:da ()
  (setq ssl (ssget))
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
      (setq pd (getpoint "\naligment point: "))
      (setq ssi 0)
     ; (setq intss nil)
      (setq ppo nil)
            
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj))
	(setq layer (cdr (assoc '8 objdat)))
	(setq fl 0)
	(setq i 0)
	(while (setq obl (ssname ssl i))
	  (if (= (cdr (assoc '8 (entget obl))) layer)
	    (setq fl 1))
	  (setq i (+ i 1))
	  )
	(if (> fl 0)
	  (progn
	  (setq ints (sec:intersec mainline obj))
	  ;;;sorting ints
	  (setq i 0)
	  (setq intss nil)
	  (while (nth i ints)
	  (setq j 0)
	  (setq curdist 50000)  ;bad practise!!!
	      (while (nth j ints)
		(if (and (/= (nth j ints) 0)
			 (< (distance ps (nth j ints)) curdist)
		    )
		  (progn
		    (setq curdist (distance ps (nth j ints)))
		    (setq indexnearest j)
		    )
		  )
		(setq j (+ j 1))
		)
	  (setq intss (append intss (list (nth indexnearest ints))))
	  (setq ints (subst 0 (nth indexnearest ints) ints))
	    (setq i (+ i 1))
	    )
	  (print intss)
	  ;;;sorting ints end;;;
	  ;;;cycling through points
	  (setq i 0)
	  (if (= ppo nil) (progn
			    (setq ppo (car intss))
			    (setq i 1)
			    )
	    )
	  (while (nth i intss)
	    (setq cpo (nth i intss))
	    ;;;draw
	    (setq objdim
		   (vla-AddDimRotated
		     *ModelSpace*
		     (vlax-3d-point ppo)
		     (vlax-3d-point cpo)
		     (vlax-3d-point pd)
		     alpha
		   )
	    )
	    (vla-put-styleName objdim "RAZR1")
	    ;;;draw end
	    (setq ppo cpo)
	    (setq i (+ i 1))
	    )

	  ;;;cycling through points;;;end
	  )
	)
	(setq ssi (+ ssi 1))
      );;;while (setq obj (ssname ss ssi));;;end

      (vla-delete mainline)
    )
;;;progn
  )
)