;;;written by Nikolay Chehlarov
;;;Sofia, July 2008
;;;defines the function dz; it puts a dim for series and automaticaly fixes its layer and style
;;;function all returns total length of selcted objects
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun all:getss (/ ss)
  (setq ss (ssget))
)

(defun c:all ()
  (if (setq ss (all:getss))
    (progn
      (setq ssi 0)
      (setq len 0)
      (while (setq obj (ssname ss ssi))
	;(setq objdata (entget obj))
	(setq obja (vlax-ename->vla-object obj))
	(setq len (+ len (vla-get-length obja)))

	(setq ssi (+ ssi 1))

	)
      )
    )
  (print len)
  (print)
  )


(defun c:dz ()
  (setq fl 0)
  (dz:dz)
  )
(defun dz:dz ()
  (if (= fl 0) (setq p1 (getpoint "first point")))
  (setq p2 (getpoint p1 "second point"))
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
  (if (<= (abs alpha) (/ pi 4))  (vla-put-layer obj "1K_arm_koti_y") )
  (if (> (abs alpha) (/ pi 4))  (vla-put-layer obj "1K_arm_koti_x") )
  
  (vla-put-stylename obj "obseg")
  (setq p1 p2)
  (setq fl 1)
  (dz:dz)
)