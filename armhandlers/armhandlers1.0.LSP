;;;written by Nikolay Chehlarov
;;;Sofia, July 2008
;;;defines the function dz; it puts a dim for series and automaticly fixes its layer and style
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun c:dz ()
  (setq p1 (getpoint "first point"))
  (setq p2 (getpoint "second point"))
  (if (= (- (car p1) (car p2)) 0) (setq alpha (/ pi 2)))
  (if (/= (- (car p1) (car p2)) 0)(setq alpha (atan (/ (- (cadr p1) (cadr p2)) (- (car p1) (car p2)) ))))
  
  (setq	obj (vla-AddDimRotated
	       *ModelSpace*
	       (vlax-3d-point p1)
	       (vlax-3d-point p2)
	       (vlax-3d-point p2)
	       alpha
	     )
  )
  (vla-put-layer obj "1K_arm_koti")
  (vla-put-stylename obj "obseg")
)