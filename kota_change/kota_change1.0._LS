(defun t:getss (/ ss)
  (setq ss (ssget))
)

(defun c:trepl()
  (if (setq ss (t:getss))
    (progn
      ;(setq char (getstring "Enter first letter: "))
      (setq dobavka 110)
      (setq ssi 0)
      
      (while (setq obj (ssname ss ssi))
	(setq objdat (entget obj) )
	(if (= (cdr (assoc 0 objdat)) "TEXT")
	  (progn
	    ;;;ot sloq na kotite na plo4ite
	    (if (= (cdr (assoc 8 objdat)) "0101_STR_NIV")
	      (
		(if (or
		      (= (substr (cdr (assoc '1 objdat)) 1 1) "+")
		      (= (substr (cdr (assoc '1 objdat)) 1 1) "-")
		      )
		  (progn
		    (setq value (read (cdr (assoc '1 objdat))))
		    (setq value (+ value dobavka))
		    )
		  )
	      )
	    ;;;end ot sloq na kotite na plo4ite
	    
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