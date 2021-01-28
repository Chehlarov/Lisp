;tozi lisp opravq stila, na koloni
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun rr:getss (/ ss)
  (setq ss (ssget))
)


;reperage columns
(defun c:rp ()
  (if (setq ss (rr:getss))
    (progn
      (setq ssi 0)      
      (while (setq obj (ssname ss ssi))
	(setq cdat (entget obj))		
	(if (and (= (cdr (assoc '0 cdat)) "TEXT")
		 (=  (substr (cdr (assoc '1 cdat)) 1 1)  "P")
	    )
	  (progn
	    (setq x (cadr (assoc '10 cdat)))
	    (setq y (- (caddr (assoc '10 cdat)) 0.20 ))
	   ) ;;progn
  	);;;if
	(setq ssi (+ ssi 1))
      );;while
      
       (setq ssi 0)      
      (while (setq obj (ssname ss ssi))
	(setq cdat (entget obj))		
	(if (and (= (cdr (assoc '0 cdat)) "TEXT")
		 (/=  (substr (cdr (assoc '1 cdat)) 1 1)  "P")
	    )
	  (progn
	    (setq data (list x y))
	    (setq cdat (subst
		 	(cons '10 data)
		 	(assoc 10 cdat)
			cdat
		   )
	    )
	    
	    (entmod cdat)
	    (entupd obj)
	   ) ;;progn
  	);;;if
	
	(setq ssi (+ ssi 1))
      );;while

      
     );;;progn  
);;if
);;;defun...


;reperage fondations
(defun c:rf ()
  (if (setq ss (rr:getss))
    (progn
      (setq ssi 0)      
      (while (setq obj (ssname ss ssi))
	(setq cdat (entget obj))		
	(if (and (= (cdr (assoc '0 cdat)) "TEXT")
		 (=  (substr (cdr (assoc '1 cdat)) 1 1)  "S")
	    )
	  (progn
	    (setq x (cadr (assoc '10 cdat)))
	    (setq y (caddr (assoc '10 cdat)))
	   ) ;;progn
  	);;;if
	(setq ssi (+ ssi 1))
      );;while
      
       (setq ssi 0)      
      (while (setq obj (ssname ss ssi))
	(setq cdat (entget obj))		
	(if (and (= (cdr (assoc '0 cdat)) "TEXT")
		 (=  (substr (cdr (assoc '1 cdat)) 1 1)  "A")
	    )
	  (progn	    
	    (if (= (cdr (assoc '50 cdat)) 0)
	      (setq data (list x (- y 0.20)))
	      )
	    (if (= (cdr (assoc '50 cdat)) (/ pi 2))
	      (setq data (list (+ x 0.20) y ))
	      )
	    
	    
	    (setq cdat (subst
		 	(cons '10 data)
		 	(assoc 10 cdat)
			cdat
		   )
	    )
	    
	    (entmod cdat)
	    (entupd obj)
	   ) ;;progn
  	);;;if
	
	(setq ssi (+ ssi 1))
      );;while

      (setq ssi 0)      
      (while (setq obj (ssname ss ssi))
	(setq cdat (entget obj))		
	(if (and (= (cdr (assoc '0 cdat)) "TEXT")
		 (/=  (substr (cdr (assoc '1 cdat)) 1 1)  "S")
		 (/=  (substr (cdr (assoc '1 cdat)) 1 1)  "A")
	    )
	  (progn	    
	    (if (= (cdr (assoc '50 cdat)) 0)
	      (setq data (list x (- y 0.40)))
	      )
	    (if (= (cdr (assoc '50 cdat)) (/ pi 2))
	      (setq data (list (+ x 0.40) y ))
	      )
	    
	    
	    (setq cdat (subst
		 	(cons '10 data)
		 	(assoc 10 cdat)
			cdat
		   )
	    )
	    
	    (entmod cdat)
	    (entupd obj)
	   ) ;;progn
  	);;;if
	
	(setq ssi (+ ssi 1))
      );;while

      
     );;;progn  
);;if
);;;defun...


;;;defun...