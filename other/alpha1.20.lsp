;;;written by Nikolay Chehlarov oct 2009
;;;lisp za syzdavane na tablica na profilnite gredi megdu 2 6perplata
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
(setq *acad* (vlax-get-acad-object)
*doc* (vla-get-activedocument *acad*)
*util* (vla-get-utility *doc*))

(setq tol 5)

(defun alp:getss (/ ss)
  (setq ss (ssget))
)

;;;normalize the vector
(defun alp:norm (vect / l)
  (setq l (sqrt (+ (* (car vect)(car vect)) (* (cadr vect)(cadr vect)) (* (caddr vect)(caddr vect)))))
  (list (/ (car vect) l) (/ (cadr vect) l) (/ (caddr vect) l))
  )

;;;returns the angel in degrees between 2 panles
;;;hypotesis the normal vector pokazva nevidimata strana na stenata
(defun alp:ang (obja objb / co1 co2 i j probang)
  (setq probang 0)
  (setq objdataa (entget obja))
  (if (/= (cdr (assoc 0 objdataa)) "LWPOLYLINE") (alert "Please select only 2DPolylines"))
  (setq va (alp:norm (cdr (assoc 210 objdataa))))
    
  (setq objdatab (entget objb))
  (if (/= (cdr (assoc 0 objdatab)) "LWPOLYLINE") (alert "Please select only 2DPolylines"))
  (setq vb (alp:norm (cdr (assoc 210 objdatab))))

 ;;;opredelqne na ygyla no bez da se znae kakyv e tipa mu
  (setq cosa
	   (+
	     (* (car va) (car vb))
	     (* (cadr va) (cadr vb))
	     (* (caddr va) (caddr vb))
	   )
    )
  (setq sina (sqrt (- 1 (* cosa cosa))))
  (setq alpha (atan  sina cosa))
  (setq alpha (/ (* alpha 180) pi))
  (if (< alpha 0) (setq alpha (- 180 alpha)))
   ;;;end;;;opredelqne na ygyla no bez da se znae kakyv e tipa mu
;;;opredelqne tipa na ygyla
  (setq co1 (dex:co obja))
  (setq co2 (dex:co objb))
  
  (setq comp (alp:iscommon obja objb 0))
  ;;;finding a vertrex lying in plane co1 perpendicular to the common side
  
  (setq C1 (cadr comp))
  
  
  (setq C2 (caddr comp))
  
;cross product
  (setq	C12 (alp:norm (list
			(- (car C1) (car C2))
			(- (cadr C1) (cadr C2))
			(- (caddr C1) (caddr C2))
		      )
	    )
  )
  (setq	pl1 (alp:norm
	      (list
		(- (* (cadr va) (caddr C12)) (* (caddr va) (cadr C12)))
		(- (* (caddr va) (car C12)) (* (car va) (caddr C12)))
		(- (* (car va) (cadr C12)) (* (cadr va) (car C12)))
	      )
	    )
  )
  (setq	obl1
	 (vla-addline
	   *ModelSpace*
	   (vlax-3d-point
	     (list
	       (* (+ (car C1) (car C2)) 0.5)
	       (* (+ (cadr C1) (cadr C2)) 0.5)
	       (* (+ (caddr C1) (caddr C2)) 0.5)
	     )
	   )
	   (vlax-3d-point
	     (list
	       (+ (* (+ (car C1) (car C2)) 0.5) (* (car pl1) 50000))
	       (+ (* (+ (cadr C1) (cadr C2)) 0.5) (* (cadr pl1) 50000))
	       (+ (* (+ (caddr C1) (caddr C2)) 0.5)
		  (* (caddr pl1) 50000)
	       )
	     )
	   )
	 )
  )
  ;;;checking intersction
(setq	int (vla-IntersectWith
	      obl1
	      (vlax-ename->vla-object obja)
	      acExtendNone
	    )
  )
 (vla-delete obl1)
  (if (<= (vlax-safearray-get-u-bound (vlax-variant-value int) 1) 2)
    (setq pl1 (list
		(- 0 (car pl1))
		(- 0 (cadr pl1))
		(- 0 (caddr pl1))
		)
	  )
    )
  (if (= (vlax-safearray-get-u-bound (vlax-variant-value int) 1) -1)
    (setq probang 1)
    )
  
  
    ;;; end finding a vertrex lying in plane co1 perpendicular to the common side

   ;;;finding a vertrex lying in plane co2 perpendicular to the common side
 
  
  
  
  
  
;cross product
  (setq	C12 (alp:norm (list
			(- (car C1) (car C2))
			(- (cadr C1) (cadr C2))
			(- (caddr C1) (caddr C2))
		      )
	    )
  )
  (setq	pl2 (alp:norm
	      (list
		(- (* (cadr vb) (caddr C12)) (* (caddr vb) (cadr C12)))
		(- (* (caddr vb) (car C12)) (* (car vb) (caddr C12)))
		(- (* (car vb) (cadr C12)) (* (cadr vb) (car C12)))
	      )
	    )
  )
  (setq	obl2
	 (vla-addline
	   *ModelSpace*
	   (vlax-3d-point
	     (list
	       (* (+ (car C1) (car C2)) 0.5)
	       (* (+ (cadr C1) (cadr C2)) 0.5)
	       (* (+ (caddr C1) (caddr C2)) 0.5)
	     )
	   )
	   (vlax-3d-point
	     (list
	       (+ (* (+ (car C1) (car C2)) 0.5) (* (car pl2) 50000))
	       (+ (* (+ (cadr C1) (cadr C2)) 0.5) (* (cadr pl2) 50000))
	       (+ (* (+ (caddr C1) (caddr C2)) 0.5)
		  (* (caddr pl2) 50000)
	       )
	     )
	   )
	 )
  )
  ;;;checking intersction
(setq	int (vla-IntersectWith
	      obl2
	      (vlax-ename->vla-object objb)
	      acExtendNone
	    )
  )
  (vla-delete obl2)
  (if (<= (vlax-safearray-get-u-bound (vlax-variant-value int) 1) 2)
    (setq pl2 (list
		(- 0 (car pl2))
		(- 0 (cadr pl2))
		(- 0 (caddr pl2))
		)
	  )
    )
  (if (<= (vlax-safearray-get-u-bound (vlax-variant-value int) 1) -1)
    (setq probang 1)
    )
  
    ;;; end finding a vertrex lying in plane co2 perpendicular to the common side
  (setq C12 (alp:norm (list (- (car C2)(car C1))
			    (- (cadr C2)(cadr C1))
			    (- (caddr C2)(caddr C1))
			    )))
  (setq C21 (alp:norm (list (- (car C1)(car C2))
			    (- (cadr C1)(cadr C2))
			    (- (caddr C1)(caddr C2))
			    )))
  (setq cospl1vb
	   (+
	     (* (car pl1) (car vb))
	     (* (cadr pl1) (cadr vb))
	     (* (caddr pl1) (caddr vb))
	   )
    )
  (setq cospl2va
	   (+
	     (* (car pl2) (car va))
	     (* (cadr pl2) (cadr va))
	     (* (caddr pl2) (caddr va))
	   )
    )
  (if (and (>= cospl1vb 0) (>= cospl2va 0))
    (setq alpha2 (- 180 alpha))
    )
  (if (and (< cospl1vb 0) (< cospl2va 0))
    (setq alpha2 (+ 180 alpha))
    )
  (if (or
	(and (>= cospl1vb 0) (< cospl2va 0))
	(and (< cospl1vb 0) (>= cospl2va 0))
	(= probang 1)
	)
    (setq alpha2 (- 0 alpha))
    )
  ;;;end;;;opredelqne tipa na ygyla 
  alpha2
)
(defun c:ang()
  (princ "Select 2 polylines ")
  (setq ss (ssget))
  (setq obja (ssname ss 0))
  (setq objb (ssname ss 1))
  (print "The angel between the two panels is ")
  (princ (alp:ang obja objb))
  (princ)
  )


;;;function returning the smallest distance between point and polyline object
(defun alp:nearest (point co / i va vb ab A base minh cosvab cosvba fl)
  (setq minh nil)
  (setq va (list (- (car point) (nth 0 co) )
		 (- (cadr point) (nth 1 co) )
		 (- (caddr point) (nth 2 co) )
		 )
	)
(setq i 3)
  (setq fl 1)
  (while (or (< i (length co))  fl )
  (if (< i (length co))
    (setq vb
	   (list (- (car point) (nth i co) )
		 (- (cadr point) (nth (+ i 1) co) )
		 (- (caddr point) (nth (+ i 2) co) )
	   )
    )
;;;else
    (progn
      (setq vb (list (- (car point) (nth 0 co) )
		 (- (cadr point) (nth 1 co) )
		 (- (caddr point) (nth 2 co) )
		 )
      )
      (setq fl nil)
    )
  )
  
    ;;;cross product
    (setq A	
	  (list
	    (- (* (cadr va) (caddr vb)) (* (caddr va) (cadr vb)))
	    (- (* (caddr va) (car vb)) (* (car va) (caddr vb)))
	    (- (* (car va) (cadr vb)) (* (cadr va) (car vb)))
	  
	)
    )
    (setq A (distance (list 0 0 0) A ))

    (setq ab (list
		   (- (car va) (car vb))
		   (- (cadr va) (cadr vb))
		   (- (caddr va) (caddr vb))
		 )
	  )
    (setq base (distance
		 (list 0 0 0)
		 ab
	       )
    )
    (if (/= base 0)
      (setq A (/ A base))
      (setq A (* 100 tol))
      )

        
    (setq cosvab
	    (+
		(* (car va) (car ab))
		(* (cadr va) (cadr ab))
		(* (caddr va) (caddr ab))
	      )
	   
    )

    (if (< cosvab 0) (setq A (distance (list 0 0 0) va)))

    (setq cosvba
	    ( - (+
		(* (car vb) (car ab))
		(* (cadr vb) (cadr ab))
		(* (caddr vb) (caddr ab))
	      )
		)
    )

    (if (< cosvba 0) (setq A (distance (list 0 0 0) vb)))

    
    (if (or (not minh) (< A minh)) (setq minh A))
;;;;;;;;;;;;;;;;;;;;;
    (setq va vb)
    
    (setq i (+ i 3))
  (if (and (> i (length co)) fl) (setq fl 2))
    )
  ;(alert (rtos minh))
  minh
);;end defun 


;;;function to return the length of the common side and the coordinates of the common points
(defun alp:iscommon(obj1 obj2 fldrawline / i j tmp co1 co2 int compoints intco p1 p2 l fl p12 angg fli1 fli2 fldanger)
  (setq co1 (dex:co obj1))
  (setq co2 (dex:co obj2))
    (if (and
	    (< (abs (- (nth 0 co1) (nth (- (length co1) 3) co1))) tol)
	    (< (abs (- (nth 1 co1) (nth (- (length co1) 2) co1))) tol)
	    (< (abs (- (nth 2 co1) (nth (- (length co1) 1) co1))) tol)
	  )
    (setq co1 (cdddr co1))
    )
  (if (and
	    (< (abs (- (nth 0 co2) (nth (- (length co2) 3) co2))) tol)
	    (< (abs (- (nth 1 co2) (nth (- (length co2) 2) co2))) tol)
	    (< (abs (- (nth 2 co2) (nth (- (length co2) 1) co2))) tol)
	  )
    (setq co2 (cdddr co2))
    )
  (setq compoints nil)
  (setq fldanger nil)
  ;;;checking obj1 points
  (setq i 0)
  (setq fli1 nil)
  (setq fli2 nil)
  
   (while (< i (length co1))
     (if (< (alp:nearest
	      (list (nth i co1) (nth (+ i 1) co1) (nth (+ i 2) co1))
	      co2
	    )
	    tol
	 )
       (progn
	 (setq compoints (append compoints
				 (list
				   (list (nth i co1)
					 (nth (+ i 1) co1)
					 (nth (+ i 2) co1)
				   )
				 )
			 )
	 )
	 (if fli1
	   (setq fli2 i)
	   ;;else
	   (setq fli1 i)
	 )
       )
     )
     (setq i (+ i 3))
   )
  (if (and fli1 fli2)
    (if (and (> (abs (- fli1 fli2)) 3)
	     (/= (abs (- fli1 fli2)) (- (length co1) 3))
	     )
      (setq fldanger 1)
      )
    )
   ;;;end checking obj1 points
  
   ;;;checking obj2 points
  (setq i 0)
  (setq fli1 nil)
  (setq fli2 nil)
  (while (< i (length co2))
     (if (< (alp:nearest
	      (list (nth i co2) (nth (+ i 1) co2) (nth (+ i 2) co2))
	      co1
	    )
	    tol
	 )
       (progn
       (setq compoints (append compoints
			       (list
			       (list (nth i co2)
				     (nth (+ i 1) co2)
				     (nth (+ i 2) co2)
			       )
			       )
		       )
       )
       (if fli1
	   (setq fli2 i)
	   ;;else
	   (setq fli1 i)
	 )

       )
     )
     (setq i (+ i 3))
   )
  (if (and fli1 fli2)
    (if (and (> (abs (- fli1 fli2)) 3)
	     (/= (abs (- fli1 fli2)) (- (length co2) 3))
	     )
      (setq fldanger 1)
      )
    )
   ;;;end checking obj2 points
  ;;;sorting and checking for equal coordinates
  (setq tmp compoints)
  (setq compoints (list (car compoints)))
  (setq i 1)
  (while (< i (length tmp))
    (setq j 0)
    (setq fl 1)
    (while (< j (length compoints))
      (if (< (distance (nth i tmp) (nth j compoints)) tol)
	(setq fl 0)
      )
      (setq j (+ j 1))
    )
    (if (= fl 1)
      (setq compoints (append compoints (list (nth i tmp))))
      )
    (setq i (+ i 1))
  )
  ;;;end sorting and checking for equal coordinates


  
  (setq l nil)


  (if (= (length compoints) 2)
    (progn
      (setq p1 (car compoints))
      (setq p2 (cadr compoints))
      (setq p12 (list (- (car p2) (car p1))
		      (- (cadr p2) (cadr p1))
		      (- (caddr p2) (caddr p1))
		      )
	    )
      
     (if (/= fldanger 1)
       (progn
	 (setq l (distance p1 p2))
	 (if (= fldrawline 1)
	   (progn
	     (setq objl	(vla-addline
			  *ModelSpace*
			  (vlax-3d-point p1)
			  (vlax-3d-point p2)
			)
	     )
	     (vla-put-color objl acred)
	     (vla-put-lineweight objl acLnWt050)
	     )
	   )
	 )

	 ;;else
	 (setq l (- 1))
       )
     )
  )

  (if (> (length compoints) 2)
    (progn
      (setq p1 (car compoints))
      (setq p2 (cadr compoints))
      (setq l (- 1))
      )
  )
 
  (princ)
  (list l p1 p2)
  )

;;;with this tool the handle is written to the block (its hiiden attribute)
(defun c:pwmatch ()
  (while (not (setq objd (entget (car (entsel "select plyline")))))
  )
  (if (= (cdr (assoc 0 objd)) "LWPOLYLINE")
    (progn
      (setq hand (cdr (assoc 5 objd)))
      (while (not (setq obj (car (entsel "select block")))))
      (setq objdata (entget obj))
      (if (and (= (cdr (assoc '0 objdata)) "INSERT")
	       (= (cdr (assoc '2 objdata)) "PW")
	  )
	(progn
	  (setq catt (entnext obj))
	  (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	    (setq cdat (entget catt))
;;;getting current attribute
	    (if	(= (cdr (assoc 2 cdat)) "HANPARENT")
	      (progn
		(setq cdat
		       (subst
			 (cons '1 hand)
			 (assoc 1 cdat)
			 cdat
		       )
		)
		(entmod cdat)
		(entupd catt)

	      )
	    )
	    (setq catt (entnext catt))
	  )

	)
;;;else
	(alert "Not possible to write the data")
      )

    )
    (alert "Not polyline")
  )
)

;;;function sorting dotted pairs
(defun alp:dottedpairsort (array / i arrayout maxi)
 (setq i 0)
  (setq maxi 1)
  (while (< i (length array))
    (if (> (car (nth i array)) maxi)
      (setq maxi (car (nth i array)))
      )
    (setq i (+ i 1))
    )
(setq i 1)
(setq arrayout nil)
  (while (<= i maxi)
    (if (assoc i array)
      (setq arrayout (append arrayout (list (assoc i array))))
      )
    (setq i (+ i 1))
    )
  (if (/= (length array) (length arrayout) ) (alert "problem in indexing! Perhaps there are panels with the same numbers"))
  arrayout
  )
;;;end function sorting dotted pairs
(defun c:pwd()
  ;;;seting the steps of the table
  (setq stepx 1500)
  (setq stepy 400)
  ;;;loading data in harray
  (setq harray nil)
  (if (setq ss (alp:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "PW")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))

	      (if (= (cdr (assoc 2 cdat)) "HANPARENT")
		(setq chan (cdr (assoc 1 cdat)))
	      )

	      (if (= (cdr (assoc 2 cdat)) "-")
		(setq cpw (atoi (substr (cdr (assoc 1 cdat)) 2)))
	      )
	      
	      (setq catt (entnext catt))
	    )

	    (setq harray (append harray (list (cons cpw chan))))
;;;;;;;;;
	  )
	  )
	(setq ssi (+ ssi 1))
	)
      )
    )

  (setq harray (alp:dottedpairsort harray))
  (print harray)
  (princ)

    ;;;end ;;;loading data in harray
  (vl-cmdf "ucs" "w")
  (setq ip (getpoint "enter key point of the table: "))
  ;;;setting current layer and ucs
    (setq obj (vla-add *layers* "0_TEXT"))
  (vla-put-color obj 2)
  (vla-put-activelayer (vla-get-ActiveDocument (vlax-get-acad-object)) obj)
        
  (setq i 0)
  (setq j 0)
  (setq crow ip)
  (while (< i (length harray))
    (setq j (+ i 1))
    (while (< j (length harray))
      (if (= 1 1)
	(progn
	 
	  
	  (setq	l (car(alp:iscommon
		    (handent (cdr (nth i harray)))
		    (handent (cdr (nth j harray)))
		    1
		  ))
	  )
	  (if (and (/= l nil) (/= l 0))
	    (progn
	      (setq	angel (alp:ang (handent (cdr (nth i harray)))
			       (handent (cdr (nth j harray)))
		      )
	  )
	  (setq str (strcat
		      "p"
		      (rtos (car (nth i harray)))
		      " - p"
		      (rtos (car (nth j harray)))

		      "  "
		      (rtos l 2 0) "mm/" (rtos angel 2 0) "%%d")
		)
	    
	  (setq	secttext
		 (vla-addtext
		   *Modelspace*
		   str
		   (vlax-3D-point (list 0 0))
		   100
		 )
	  )
   
	  (if (or (< l 0) (< angel 0)) (vla-put-color secttext acred))
	  (vla-put-layer secttext "0_TEXT")
	  (vla-put-stylename secttext "INDI")
					;	  (vla-put-scalefactor secttext 0.85)
	  (vla-put-alignment secttext acAlignmentMiddleCenter)
	  (vla-put-TextAlignmentPoint
	    secttext
	    (vlax-3D-point crow)
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
	      (setq crow (list (car crow) (- (cadr crow) stepy)))
	    );;;end progn
	)
      );;;end progn
	)
      (setq j (+ j 1))
    )
    (setq i (+ i 1))
  )
  );end defun