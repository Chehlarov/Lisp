(defun t:getss (/ ss)
  (setq ss (ssget))
)

(defun c:ty()
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
	    (if (or
		  (= (cdr (assoc 8 objdat)) "0101_HOZ_NIV_TXT")
		  (= (cdr (assoc 8 objdat))
		     "0101_COUPES_RABATTUES"
		  )
		  (= (cdr (assoc 8 objdat)) "0101_HOZ_POUTRES_BN_CORBEAUX_TXT")
		)
	      (progn
	       (setq str (cdr (assoc '1 objdat)))
	       (setq index (vl-string-search "+" str))
	       (if (= index nil) (setq index (vl-string-search "-" str)))
	       (if (/= index nil)
		 (progn
	       (setq i index)
	       (while (< i (strlen str))
		 (if (or
		       (= (substr str (+ i 1) 1) ".")
		       (wcmatch (substr str (+ i 1) 1) "#")
		       )
		   (setq endindex (+ i 1))
		   )
		 (setq i (+ i 1))
		 )
	       (setq kota (read (substr str (+ index 1) endindex)))
	       (setq kota (+ kota dobavka))
	       
	       (setq strkota (rtos kota))

	       (if (< (rem (* kota 100) 100) 0.01)
		 (setq strkota (strcat strkota ".00"))
		 (if (< (rem (* kota 100) 10) 0.1)
		   (setq strkota (strcat strkota "0"))
		 )
	       )


	       (setq out (strcat (substr str 1 index)   strkota  (substr str (+ endindex 1) )   ))


	          (setq objdat
		   (subst
		     (cons '1 out)
		     (assoc '1 objdat)
		     objdat
		     )
		   )
	    (entmod objdat)
	    (entupd obj)
	       );;progn
		 );;if
		);;progn
	      )
	    ;;;end ot sloq na kotite na plo4ite 
	    
	    )
	  )
	(setq ssi (+ ssi 1))
	)
      
  )
    )
  );;;defun