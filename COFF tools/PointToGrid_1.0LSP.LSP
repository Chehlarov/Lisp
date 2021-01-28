;;;written by Nikolay Chehlarov
;;;Sofia, Jan 2016
;;;Snaps points to grids
(vl-load-com)


(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)



(defun ptg:getss (/ ss)
  (setq ss (ssget))
)


(defun c:ptg	()
  (if (setq ss (ptg:getss))
    (progn

      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq pd (entget obj))	
	;;;proverka na to4ki
	(if (= (cdr (assoc '0 pd)) "POINT")
	  (progn
	    ;;change coordinate of point
	    (setq x (cadr (assoc '10 pd)))
	    (setq y (caddr (assoc '10 pd)))
	    (if (< (abs (rem x 0.01)) 0.005)
	      (setq x  (- x (rem x 0.01)) )
	      (if (>= x 0.0)
	       (setq x (+ (- x (rem x 0.01)) 0.01))
	       (setq x (- (- x (rem x 0.01)) 0.01))
		)	      
	      )
	    (if (< (abs (rem y 0.01)) 0.005)
	      (setq y  (- y (rem y 0.01)) )
	      (if (>= y 0.0)
	       (setq y (+ (- y (rem y 0.01)) 0.01))
	       (setq y (- (- y (rem y 0.01)) 0.01))
		)	      
	      )
	    
	    (setq data (list x y))

	    (setq pd (subst
		 	(cons '10 data)
		 	(assoc 10 pd)
			pd
		   )
	    )
	    
	    (entmod pd)
	    (entupd obj)
	     ;;end change coordinate of point
	    )
	  )
	;;;end proverka na to4ki

	(setq ssi (+ ssi 1))
      )
;;;while
    )
  )
  (princ)
)

(defun c:pc	()
  (if (setq ss (ptg:getss))
    (progn

      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq pd (entget obj))	
	;;;proverka na to4ki
	(if (= (cdr (assoc '0 pd)) "POINT")
	  (progn
	    ;;change coordinate of point
	    (setq x (cadr (assoc '10 pd)))
	    (setq y (caddr (assoc '10 pd)))
	    (if (< (abs (rem x 0.005)) 0.0025)
	      (setq x  (- x (rem x 0.005)) )
	      (if (>= x 0.0)
	       (setq x (+ (- x (rem x 0.005)) 0.005))
	       (setq x (- (- x (rem x 0.005)) 0.005))
		)	      
	      )
	    (if (< (abs (rem y 0.005)) 0.0025)
	      (setq y  (- y (rem y 0.005)) )
	      (if (>= y 0.0)
	       (setq y (+ (- y (rem y 0.005)) 0.005))
	       (setq y (- (- y (rem y 0.005)) 0.005))
		)	      
	      )
	    
	    (setq data (list x y))

	    (setq pd (subst
		 	(cons '10 data)
		 	(assoc 10 pd)
			pd
		   )
	    )
	    
	    (entmod pd)
	    (entupd obj)
	     ;;end change coordinate of point
	    )
	  )
	;;;end proverka na to4ki

	(setq ssi (+ ssi 1))
      )
;;;while
    )
  )
  (princ)
)
