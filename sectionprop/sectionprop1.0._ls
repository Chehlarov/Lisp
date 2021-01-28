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