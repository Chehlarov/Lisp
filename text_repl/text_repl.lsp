(defun t:getss (/ ss)
  (setq ss (ssget))
)

(defun c:trepl()
  (if (setq ss (t:getss))
    (progn
      ;(setq char (getstring "Enter first letter: "))
      (setq char "G")
      (setq ssi 0)
      
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj) )
	(if (= (cdr (assoc 0 objdat)) "TEXT")
	  (progn
	    (setq objdat
		   (subst
		     (cons '1 (strcat char (cdr (assoc '1 objdat) )))
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