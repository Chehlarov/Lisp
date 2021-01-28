;;;this lisp builds a beam formwork ready for putting rebar
;;;written by Nikolay Chehlarov JAN 2009
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
;;;translate from current UCS to WCS
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
;;;(if (/= (vlax-safearray-get-u-bound (vlax-variant-value int) 1)
					;	  -1
;;;    )
;;; (progn
  (setq
    int1 (list
	   (car (vlax-safearray->list (vlax-variant-value int)))
	   (cadr (vlax-safearray->list (vlax-variant-value int)))
	   (caddr (vlax-safearray->list (vlax-variant-value int)))
	 )
  )
;;;)
					;  (setq
					;	int2 (list
					;	       (nth 3 (vlax-safearray->list (vlax-variant-value int)))
					;	       (nth 4 (vlax-safearray->list (vlax-variant-value int)))
					;	       (nth 5 (vlax-safearray->list (vlax-variant-value int)))
					;	     )
					;     )
  int1


)

;;;main function
(defun c:cs ()
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
      (setq kontur nil)
     ; (setq gredi nil)
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj))
	(setq layer (cdr (assoc '8 objdat)))
	(if (or	(= layer  "1K_gredi")
		(= layer "1K_kolony")
		(= layer "1K_stalbi")
		(= layer "1K_steni")
	    )
	  (progn
	  (setq kontur (append kontur (list obj)))
	  (setq ip (sec:intersec mainline (nth ssi kontur)))
	  (PRINT IP)
	  )
	)
	
	(setq ssi (+ ssi 1))
      )

      
;;;
      
    )
;;;progn
  )
)