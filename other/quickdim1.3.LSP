;;;written by Nikolay Chehlarov nov 2009
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(defun qd:getss	(/ ss)
  (setq ss (ssget))
)

(defun c:qd ()
  (if (setq ss (qd:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (or
		   (= (cdr (assoc '0 objdata)) "LWPOLYLINE")
		   (= (cdr (assoc '0 objdata)) "LINE")
		 )
		 (or
		   (= (cdr (assoc 8 objdata)) "0_FLATS")
		   (= (cdr (assoc 8 objdata)) "0_STUB_ARMS")
		 )
	    )
	  (progn

;;;;;;;;;;;11111111111111111111111111111
	    (if	(= (cdr (assoc '0 objdata)) "LINE")
	      (progn
		(setq p1 (cdr (assoc 10 objdata)))
		(setq p2 (cdr (assoc 11 objdata)))
	      )
	    )
	    (if	(= (cdr (assoc '0 objdata)) "LWPOLYLINE")
	      (progn
		(setq i 0)
		(while (< i (length objdata))
		  (if (= (car (nth i objdata)) 10)
		    (progn
		      (setq p1 p2)
		      (setq p2 (cdr (nth i objdata)))
		    )
		  )
		  (setq i (+ i 1))
		)
	      )
	    )

	    (if	(= (- (car p1) (car p2)) 0)
	      (setq alpha (/ pi 2))
	    )
	    (if	(/= (- (car p1) (car p2)) 0)
	      (setq alpha (atan	(/ (- (cadr p1) (cadr p2))
				   (- (car p1) (car p2))
				)
			  )
	      )
	    )

	    (setq obj (vla-AddDimRotated
			*ModelSpace*
			(vlax-3d-point p1)
			(vlax-3d-point p2)
			(vlax-3d-point p2)
			alpha
		      )
	    )
	    (vla-put-layer obj "0_DIMENSIONS")
	    (vla-put-stylename obj "ROMANSfree")



;;;;;;;;;;;11111111111111111111111111111
	  )
	)
	(setq ssi (+ ssi 1))
      )

    )
  )
)

;;;this function draws a table containning lengths of stub arms and flats
(defun c:qdt ()
;;;seting the steps of the table
  (setq stepx 500)
  (setq stepy 250)
  (if (setq ss (qd:getss))
    (progn
      (setq ip (getpoint "Key point of table: "))
      (setq crow ip)
      (setq crowstub ip)
      (setq lflats 0)
      (setq lstub 0)
      ;;;title
	    (setq t1
		   (vla-addtext
		     *Modelspace*
		     "FLATS"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
      (setq t1a
		   (vla-addtext
		     *Modelspace*
		     "FL40x5"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq t2
		   (vla-addtext
		     *Modelspace*
		     "L"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq t3
		   (vla-addtext
		     *Modelspace*
		     "L+40"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (vla-put-layer t1 "0_TEXT")
	    (vla-put-stylename t1 "INDI")
	    (vla-put-alignment t1 acAlignmentMiddleCenter)
	    (vla-put-layer t1a "0_TEXT")
	    (vla-put-stylename t1a "INDI")
	    (vla-put-alignment t1a acAlignmentMiddleCenter)

	    (vla-put-layer t2 "0_TEXT")
	    (vla-put-stylename t2 "INDI")
	    (vla-put-alignment t2 acAlignmentMiddleCenter)
	    (vla-put-layer t3 "0_TEXT")
	    (vla-put-stylename t3 "INDI")
	    (vla-put-alignment t3 acAlignmentMiddleCenter)


	    (vla-put-TextAlignmentPoint
	      t1
	      (vlax-3D-point (list (+ (car ip) (* 0.5 stepx)) (+ (cadr crow) (* 3 stepy))))
	    )
      (vla-put-TextAlignmentPoint
	      t1a
	      (vlax-3D-point (list (+ (car ip) (* 0.5 stepx)) (+ (cadr crow) (* 2 stepy))))
	    )
	    (vla-put-TextAlignmentPoint
	      t2
	      (vlax-3D-point (list (+ (car ip) 0) (+ (cadr crow) (* 1 stepy))))
	    )
	    (vla-put-TextAlignmentPoint
	      t3
	      (vlax-3D-point (list (+ (car ip) (* 1 stepx)) (+ (cadr crow) (* 1 stepy))))
	    )
	    ;;;end title

           ;;;title
	    (setq t1
		   (vla-addtext
		     *Modelspace*
		     "STUB ARMS"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
      (setq t1a
		   (vla-addtext
		     *Modelspace*
		     "L45x5"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq t2
		   (vla-addtext
		     *Modelspace*
		     "L"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq t3
		   (vla-addtext
		     *Modelspace*
		     "L+40"
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (vla-put-layer t1 "0_TEXT")
	    (vla-put-stylename t1 "INDI")
	    (vla-put-alignment t1 acAlignmentMiddleCenter)
      	    (vla-put-layer t1a "0_TEXT")
	    (vla-put-stylename t1a "INDI")
	    (vla-put-alignment t1a acAlignmentMiddleCenter)
	    (vla-put-layer t2 "0_TEXT")
	    (vla-put-stylename t2 "INDI")
	    (vla-put-alignment t2 acAlignmentMiddleCenter)
	    (vla-put-layer t3 "0_TEXT")
	    (vla-put-stylename t3 "INDI")
	    (vla-put-alignment t3 acAlignmentMiddleCenter)

	    (vla-put-TextAlignmentPoint
	      t1
	      (vlax-3D-point (list (+ (car ip) (* 2.5 stepx)) (+ (cadr crow) (* 2 stepy))))
	    )
      (vla-put-TextAlignmentPoint
	      t1a
	      (vlax-3D-point (list (+ (car ip) (* 2.5 stepx)) (+ (cadr crow) (* 3 stepy))))
	    )
	    (vla-put-TextAlignmentPoint
	      t2
	      (vlax-3D-point (list (+ (car ip) (* 2 stepx)) (+ (cadr crow) (* 1 stepy))))
	    )
	    (vla-put-TextAlignmentPoint
	      t3
	      (vlax-3D-point (list (+ (car ip) (* 3 stepx)) (+ (cadr crow) (* 1 stepy))))
	    )
	    ;;;end title
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (or
		   (= (cdr (assoc '0 objdata)) "LWPOLYLINE")
		   (= (cdr (assoc '0 objdata)) "LINE")
		 )
		 (or
		   (= (cdr (assoc 8 objdata)) "0_FLATS")
					; (= (cdr (assoc 8 objdata)) "0_STUB_ARMS")
		 )
	    )
	  (progn
	    
;;;1111111111
	    (setq obja (vlax-ename->vla-object obj))
	    
	    (setq secttext
		   (vla-addtext
		     *Modelspace*
		     (rtos (vla-get-length obja) 2 0)
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq secttext2
		   (vla-addtext
		     *Modelspace*
		     (rtos (+ (vla-get-length obja) 40) 2 0)
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq  lflats (+ lflats (vla-get-length obja) 40))
	    (vla-put-layer secttext "0_TEXT")
	    (vla-put-layer secttext2 "0_TEXT")
	    (vla-put-stylename secttext "INDI")
	    (vla-put-stylename secttext2 "INDI")
	    (vla-put-alignment secttext acAlignmentMiddleCenter)
	    (vla-put-alignment secttext2 acAlignmentMiddleCenter)
	    (vla-put-TextAlignmentPoint
	      secttext
	      (vlax-3D-point crow)
	    )
	    (vla-put-TextAlignmentPoint
	      secttext2
	      (vlax-3D-point (list (+ (car crow) stepx) (cadr crow)))
	    )
	    ;;;drawing box around text
	      (setq pointcc	(vlax-make-safearray vlax-vbDouble '(0 . 7)))
	      (vlax-safearray-fill
		pointcc
		(list
		  (- (car crow) (* 0.5 stepx))
		  (- (cadr crow) (* 0.5 stepy))

		  (+ (car crow) (* 0.5 stepx))
		  (- (cadr crow) (* 0.5 stepy))

		  (+ (car crow) (* 0.5 stepx))
		  (+ (cadr crow) (* 0.5 stepy))

		  (- (car crow) (* 0.5 stepx))
		  (+ (cadr crow) (* 0.5 stepy))
		)
	      )
	      (setq objl (vla-AddLightWeightPolyline
			   *ModelSpace*
			   pointcc
			 )
	      )
	    (vla-put-closed objl actrue)
	    (vlax-safearray-fill
		pointcc
		(list
		  (+ (car crow) (* 0.5 stepx))
		  (- (cadr crow) (* 0.5 stepy))

		  (+ (car crow) (* 1.5 stepx))
		  (- (cadr crow) (* 0.5 stepy))

		  (+ (car crow) (* 1.5 stepx))
		  (+ (cadr crow) (* 0.5 stepy))

		  (+ (car crow) (* 0.5 stepx))
		  (+ (cadr crow) (* 0.5 stepy))
		)
	      )
	      (setq objl (vla-AddLightWeightPolyline
			   *ModelSpace*
			   pointcc
			 )
	      )
	      (vla-put-closed objl actrue)
	    	    ;;;end drawing boxex around text
	    (setq crow (list (car crow) (- (cadr crow) stepy)))

;;;end 1111111111

	  )
	)
	;;;;;;;;;;;;;;;;;;;;
;----------------------------------------
	
	;;;;;;;;;;;;;;;;;;;;

	(if (and (or
		   (= (cdr (assoc '0 objdata)) "LWPOLYLINE")
		   (= (cdr (assoc '0 objdata)) "LINE")
		 )
		 (or
		   ;(= (cdr (assoc 8 objdata)) "0_FLATS")
		(= (cdr (assoc 8 objdata)) "0_STUB_ARMS")
		 )
	    )
	  (progn
	    
;;;1111111111
	    (setq obja (vlax-ename->vla-object obj))
	    
	    (setq secttext
		   (vla-addtext
		     *Modelspace*
		     (rtos (vla-get-length obja) 2 0)
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq secttext2
		   (vla-addtext
		     *Modelspace*
		     (rtos (+ (vla-get-length obja) 40) 2 0)
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq  lstub (+ lstub (vla-get-length obja) 40))
	    (vla-put-layer secttext "0_TEXT")
	    (vla-put-layer secttext2 "0_TEXT")
	    (vla-put-stylename secttext "INDI")
	    (vla-put-stylename secttext2 "INDI")
	    (vla-put-alignment secttext acAlignmentMiddleCenter)
	    (vla-put-alignment secttext2 acAlignmentMiddleCenter)
	    (vla-put-TextAlignmentPoint
	      secttext
	      (vlax-3D-point (list (+ (car crowstub) (* 2 stepx)) (cadr crowstub)))
	    )
	    (vla-put-TextAlignmentPoint
	      secttext2
	      (vlax-3D-point (list (+ (car crowstub) (* 3 stepx)) (cadr crowstub)))
	    )
	    ;;;drawing box around text
	      (setq pointcc	(vlax-make-safearray vlax-vbDouble '(0 . 7)))
	      (vlax-safearray-fill
		pointcc
		(list
		  (+ (car crowstub) (* 1.5 stepx))
		  (- (cadr crowstub) (* 0.5 stepy))

		  (+ (car crowstub) (* 2.5 stepx))
		  (- (cadr crowstub) (* 0.5 stepy))

		  (+ (car crowstub) (* 2.5 stepx))
		  (+ (cadr crowstub) (* 0.5 stepy))

		  (+ (car crowstub) (* 1.5 stepx))
		  (+ (cadr crowstub) (* 0.5 stepy))
		)
	      )
	      (setq objl (vla-AddLightWeightPolyline
			   *ModelSpace*
			   pointcc
			 )
	      )
	    (vla-put-closed objl actrue)
	    (vlax-safearray-fill
		pointcc
		(list
		  (+ (car crowstub) (* 2.5 stepx))
		  (- (cadr crowstub) (* 0.5 stepy))

		  (+ (car crowstub) (* 3.5 stepx))
		  (- (cadr crowstub) (* 0.5 stepy))

		  (+ (car crowstub) (* 3.5 stepx))
		  (+ (cadr crowstub) (* 0.5 stepy))

		  (+ (car crowstub) (* 2.5 stepx))
		  (+ (cadr crowstub) (* 0.5 stepy))
		)
	      )
	      (setq objl (vla-AddLightWeightPolyline
			   *ModelSpace*
			   pointcc
			 )
	      )
	      (vla-put-closed objl actrue)
	    	    ;;;end drawing boxex around text
	    (setq crowstub (list (car crowstub) (- (cadr crowstub) stepy)))

;;;end 1111111111

	  )
	)
	(setq ssi (+ ssi 1))
      );;;end while
      ;;setin total lengths and weight
      (setq t1
		   (vla-addtext
		     *Modelspace*
		     (strcat "sumL= " (rtos lflats 2 0) )
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq t2
		   (vla-addtext
		     *Modelspace*
		     (strcat "sumL= " (rtos lstub 2 0) )
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    
	    (vla-put-layer t1 "0_TEXT")
	    (vla-put-stylename t1 "INDI")
	    (vla-put-alignment t1 acAlignmentMiddleCenter)
	    (vla-put-layer t2 "0_TEXT")
	    (vla-put-stylename t2 "INDI")
	    (vla-put-alignment t2 acAlignmentMiddleCenter)

	    (vla-put-TextAlignmentPoint
	      t1
	      (vlax-3D-point (list  (+ (car crow) (* 0.5 stepx))  (cadr crow) ))
	    )
	    (vla-put-TextAlignmentPoint
	      t2
	      (vlax-3D-point (list (+ (car crowstub) (* 2.5 stepx))  (cadr crowstub) ))
	    )
      ;;;;
      (setq t1
		   (vla-addtext
		     *Modelspace*
		     (strcat "summ= " (rtos (* lflats 1.6e-3) 2 2) "kg")
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    (setq t2
		   (vla-addtext
		     *Modelspace*
		     (strcat "summ= " (rtos (* lstub 3.4e-3) 2 2) "kg")
		     (vlax-3D-point (list 0 0))
		     100
		   )
	    )
	    
	    (vla-put-layer t1 "0_TEXT")
	    (vla-put-stylename t1 "INDI")
	    (vla-put-alignment t1 acAlignmentMiddleCenter)
	    (vla-put-layer t2 "0_TEXT")
	    (vla-put-stylename t2 "INDI")
	    (vla-put-alignment t2 acAlignmentMiddleCenter)

	    (vla-put-TextAlignmentPoint
	      t1
	      (vlax-3D-point (list  (+ (car crow) (* 0.5 stepx)) (- (cadr crow) stepy)))
	    )
	    (vla-put-TextAlignmentPoint
	      t2
	      (vlax-3D-point (list (+ (car crowstub) (* 2.5 stepx))  (- (cadr crowstub) stepy) ))
	    )
	    

    )
  )
)