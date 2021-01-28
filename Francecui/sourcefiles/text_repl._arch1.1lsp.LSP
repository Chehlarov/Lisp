(defun t:getss (/ ss)
  (setq ss (ssget))
)

(defun c:treplentofr()
  (if (setq ss (t:getss))
    (progn
      ;(setq char (getstring "Enter first letter: "))
      (setq char "G")
      (setq ssi 0)
      
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj) )
	(if (and (= (cdr (assoc 0 objdat)) "TEXT")
		 (= (cdr (assoc 1 objdat)) "Bar")
		 )
	  (progn
	    (setq objdat
		   (subst
		     (cons '1 "Barre")
		     (assoc '1 objdat)
		     objdat
		     )
		   )
	    (entmod objdat)
	    (entupd obj)
	    )
	  )
	(if (and (= (cdr (assoc 0 objdat)) "TEXT")
		 (= (cdr (assoc 1 objdat)) "Shape")
		 )
	  (progn
	    (setq objdat
		   (subst
		     (cons '1 "Forme")
		     (assoc '1 objdat)
		     objdat
		     )
		   )
	    (entmod objdat)
	    (entupd obj)
	    )
	  )
	(setq ssi (+ ssi 1))
	)
      
  )
    )
  )