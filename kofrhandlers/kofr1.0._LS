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

(defun c:kotacheck ()
  (if (setq ss (kofr:getss))
    (progn
      (setq ssi 0)
      (setq problems 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if
	  (and (= (cdr (assoc 0 objdata)) "DIMENSION")
	       (= (cadr (assoc 13 objdata)) (cadr (assoc 14 objdata)))
	  )
	   (progn
;;;inner cycle
	     (setq ssj 0)
	     (setq kota13 -1000)
	     (setq kota14 -1000)
	     (while (setq objj (ssname ss ssj))
	       (setq objdataj (entget objj))
	       (if (and	(= (cdr (assoc 0 objdataj)) "TEXT")
			(= (cdr (assoc 8 objdataj)) "kota")
		   )
		 (progn
		   (if (< (distance
			   (cdr (assoc 13 objdata))
			   (cdr (assoc 10 objdataj))
			   )
			  100)
		     (setq kota13 (kofr:atofcomma (cdr (assoc 1 objdataj))))
		     )
		   (if (< (distance
			   (cdr (assoc 14 objdata))
			   (cdr (assoc 10 objdataj))
			   )
			  100)
		     (setq kota14 (kofr:atofcomma (cdr (assoc 1 objdataj))))
		     )
		   
		   )
	       )
	       (setq ssj (+ ssj 1))
	     )
	     (if (/= (cdr (assoc 42 objdata)) (abs (- kota14 kota13)))
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