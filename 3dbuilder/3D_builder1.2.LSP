;;;written by Nikolay Chehlarov
;;;Sofia, june 2008
;;;tazi programa postroqva 6aibi v prostranstvoto ot plana kato se zadava viso4ina
;;;trqbva da se izbroqt viso4inite v lista h
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(defun arm:getss (/ ss)
  (setq ss (ssget))
)
(defun c:3db2 ()
  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (= (cdr (assoc '0 objdata)) "LWPOLYLINE")
	  (progn
	    (setq cp 0)
	    (setq found 0)
	    (repeat (length pd)
	      (if (= (car (nth cp pd)) 10)
		(progn
		  (if (> found 0)
		    (progn
		      (setq found (+ found 1))
		      (setq cpx (cadr (nth cp pd)))
		      (setq cpy (caddr (nth cp pd)))
		      (setq
			len (+ len
			       (sqrt (+	(* (- cpx bpx) (- cpx bpx))
					(* (- cpy bpy) (- cpy bpy))
				     )
			       )
			    )
		      )
		      (setq bpx cpx)
		      (setq bpy cpy)
		      (princ "tuk   ")
		      (princ len)
					;(princ "  ")
		    )
		  )
		  (if (= found 0)
		    (progn
		      (setq found 1)
		      (setq bpx (cadr (nth cp pd)))
		      (setq bpy (caddr (nth cp pd)))
		    )
		  )

		)
	      )
	      (setq cp (+ cp 1))
	    )
	  )

	)
      )
      (setq ssi (+ ssi 1))
    )
  )
)




(defun c:3db ()
  ;;;izborqva viso4inite vkliu4itelno i parvata
  ;(setq h '(-425 -5))
  (setq h '(3.72 6.75))
  (setq p1 (getpoint))
  (setq p2 (getpoint))


  (setq i 0)
  (while (< i (- (length h) 1))
    (vla-Add3DFace
      *ModelSpace*
      (vlax-3D-Point (list (car p1) (cadr p1) (nth i h)))
      (vlax-3D-Point (list (car p2) (cadr p2) (nth i h)))
      (vlax-3D-Point (list (car p2) (cadr p2) (nth (+ i 1) h)))
      (vlax-3D-Point (list (car p1) (cadr p1) (nth (+ i 1) h)))
      )
    (setq i (+ i 1))
    )
)

(defun c:nprop()
  (while (not (setq ob (car (entsel "select region")))))
   (setq msg (strcat "Mass per square meter? "))
  (setq mass (atof (getstring msg) ))
  
  (setq obj (vlax-ename->vla-object ob))
  (setq centroid (vla-get-Centroid obj))
  (setq area (vla-get-Area obj))
  (setq princmom (vla-get-PrincipalMoments  obj)) 
  ;(print (vlax-safearray->list (vlax-variant-value centroid)))
  ;(print (vlax-safearray->list (vlax-variant-value princmom)))
 ; (print area)
  (setq centroidl (list
		    (car (vlax-safearray->list (vlax-variant-value centroid)))
		    (cadr (vlax-safearray->list (vlax-variant-value centroid)))
		    0
		    )
	)
  (setq I (+
	    (car (vlax-safearray->list (vlax-variant-value princmom)))
	    (cadr (vlax-safearray->list (vlax-variant-value princmom))
		  )
	    )
	)
  (vla-addPoint
    *ModelSpace*
    (vlax-3d-point centroidl)
    )
  (vla-addtext
    *ModelSpace*
    (strcat "I= "(rtos (/ (* I mass) 100000000) 1 5) " txm^2")
    (vlax-3d-point centroidl)
    40
  )
  (vla-addtext
    *ModelSpace*
    (strcat "A= " (rtos (/ (* area mass) 10000) 1 5) " t")
    (vlax-3d-point (list
		     (car centroidl)
		     (- (cadr centroidl) 50)
		     ))
    40
  )
  
  )

(defun c:zfix()
  (setq z 0)
  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj) )
	(if (= (cdr (assoc 0 objdat)) "3DFACE")
	  (progn
	    (setq objdat
		   (subst
		     (cons '10 (list (nth 1 (assoc '10 objdat)) (nth 2 (assoc '10 objdat)) z))
		     (assoc '10 objdat)
		     objdat
		     )
		   )
	    (setq objdat
		   (subst
		     (cons '11 (list (nth 1 (assoc '11 objdat)) (nth 2 (assoc '11 objdat)) z))
		     (assoc '11 objdat)
		     objdat
		     )
		   )
	    (setq objdat
		   (subst
		     (cons '12 (list (nth 1 (assoc '12 objdat)) (nth 2 (assoc '12 objdat)) z))
		     (assoc '12 objdat)
		     objdat
		     )
		   )
	    (setq objdat
		   (subst
		     (cons '13 (list (nth 1 (assoc '13 objdat)) (nth 2 (assoc '13 objdat)) z))
		     (assoc '13 objdat)
		     objdat
		     )
		   )
	    
	    (entmod objdat)
	    
	    )
	  )
	(setq ssi (+ ssi 1))
      )
      (entupd objdat)
    )
  )
  )
