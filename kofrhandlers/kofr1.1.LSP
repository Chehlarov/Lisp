;;;written by Nikolay Chehlarov
;;;Sofia, July 2008
;;;kotacheck ckecks whether the distance between two level marks is equal to the distance written in vertical dim
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun kofr:draw (center)
  (vla-addCircle
    *ModelSpace*
    (vlax-3d-point center)
    100
  )
)

(defun kofr:getss (/ ss)
  (setq ss (ssget))
)

(defun kofr:atofcomma (str)
  (setq str (vl-string-translate "," "." str ))
  (setq num (atof str))
   num
)

(defun c:kc ()
  (if (setq ss (kofr:getss))
    (progn
      (setq ssi 0)
      (setq problems 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if
	  (and (= (cdr (assoc 0 objdata)) "DIMENSION")
	       (=  (cdr (assoc 50 objdata)) (/ pi 2))
	       )
	       
	  
	   (progn
;;;inner cycle
	     (setq ssj 0)
	     (setq k13f 0)
	     (setq k14f 0)
	     (setq kota13 -1000)
	     (setq kota14 -1000)
	     (while (setq objj (ssname ss ssj))
	       (setq objdataj (entget objj))
	       (if (and	(= (cdr (assoc 0 objdataj)) "TEXT")
			(= (cdr (assoc 8 objdataj)) "KTXTXT")
		   )
		 (progn
		   (if (or (= (substr (cdr (assoc 1 objdataj)) 1 1) "+")
			   (= (substr (cdr (assoc 1 objdataj)) 1 1) "-")
			   )
		     (progn
		   (if (and
		       (< (abs (- (cadr (assoc 13 objdata))(cadr (assoc 10 objdataj)) )) 120)
		       (< (abs (- (caddr (assoc 13 objdata))(caddr (assoc 10 objdataj)) )) 50)
			  )
		     (progn
		     (setq kota13 (kofr:atofcomma (cdr (assoc 1 objdataj))))
		     (setq k13f (+ k13f 1))
		     )
		     )
		   (if (and
		       (< (abs (- (cadr (assoc 14 objdata))(cadr (assoc 10 objdataj)) )) 120)
		       (< (abs (- (caddr (assoc 14 objdata))(caddr (assoc 10 objdataj)) )) 50)
			  )
		     (progn
		     (setq kota14 (kofr:atofcomma (cdr (assoc 1 objdataj))))
		     (setq k14f (+ k14f 1))
		     )
		     )
		   )
		   )
		   )
	       )
	       (setq ssj (+ ssj 1))
	     )
	     (if (or (> (abs(- (/ (cdr (assoc 42 objdata))100) (abs (- kota14 kota13)))) 0.0001)
		      (/= k13f 1)
		      (/= k14f 1)
		      )
	       (progn
	       (kofr:draw (cdr (assoc 11 objdata)))
	       (setq problems (+ problems 1))
	       )
	       )
;;;end of inner cycle
	   )
	)
	(setq ssi (+ ssi 1))
      )					;while outer cicle
      
      (print problems)
      (princ " problems found")
      (princ)
    )					;progn
  )					;if
)					;defun