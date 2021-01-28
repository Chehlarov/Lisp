;;;written by Nikolay Chehlarov
;;; opravq stila na dimensiite v zavisimost ot tova dali sa pod 1m ili nad 1m

(defun df:getss (/ ss)
  (setq ss (ssget))
)

(defun c:frdimfix()
  (if (setq ss (df:getss))
    (progn
      
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj))
	(if (and (= (cdr (assoc 0 objdat)) "DIMENSION")
		 (or (= (cdr (assoc 3 objdat)) "dim50-cm")
		     (= (cdr (assoc 3 objdat)) "dim50-m")
		 )
	    )
	  (progn
	    (if	(and (>= (cdr (assoc '42 objdat)) 100)
		     (= (cdr (assoc 3 objdat)) "dim50-cm")
		     )
	      (progn
		(setq objdat
		       (subst
			 (cons '3 "dim50-m")
			 (assoc '3 objdat)
			 objdat
		       )
		)
		(entmod objdat)
		(entupd obj)
	      )
	    )
	    (if	(and (< (cdr (assoc '42 objdat)) 1)
		     (= (cdr (assoc 3 objdat)) "dim50-m")
		     )
	      (progn
		(setq objdat
		       (subst
			 (cons '3 "dim50-cm")
			 (assoc '3 objdat)
			 objdat
		       )
		)
		(entmod objdat)
		(entupd obj)
	      )
	    )
	  );;progn
	)
	(setq ssi (+ ssi 1))
      )

   
  )
    )
  )