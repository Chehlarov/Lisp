(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun c:nprop()
  (while (not (setq ob (car (entsel "select region")))))
   ;(setq msg (strcat "Mass per square meter? "))
  ;(setq mass (atof (getstring msg) ))
  
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
  (setq I1 (car (vlax-safearray->list (vlax-variant-value princmom))))
  (setq I2 (cadr (vlax-safearray->list (vlax-variant-value princmom))))
	    
	
  (vla-addPoint
    *ModelSpace*
    (vlax-3d-point centroidl)
    )
  (vla-addtext
    *ModelSpace*
    (strcat "I1= "(rtos I1 1 5) " ^4")
    (vlax-3d-point centroidl)
    40
  )
  (vla-addtext
    *ModelSpace*
    (strcat "I2= "(rtos I2 1 5) " ^4")
    (vlax-3d-point (list
		     (car centroidl)
		     (- (cadr centroidl) 50)
		     ))
    40
  )
  
  
  )