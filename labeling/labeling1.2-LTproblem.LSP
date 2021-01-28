;ne e dovar6en

(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun label:getss (/ ss)
  (setq ss (ssget))
)

(defun c:ll ()
  (if (setq ss (label:getss))
    (progn
      (setq ssi 0)      
      (while (setq obj (ssname ss ssi))
	(setq cdat (entget obj))		
	(if (and (= (cdr (assoc '0 cdat)) "TEXT")
		 (=  (substr (cdr (assoc '1 cdat)) 1 1)  "LT")
	    )
	  (progn
	    (setq x (cadr (assoc '10 cdat)))
	    (setq y (- (caddr (assoc '10 cdat)) 0.20 ))
	   ) ;;progn
  	);;;if
	(setq ssi (+ ssi 1))
      );;while 
     );;;progn  
);;if
)

(defun c:ll ()
  (setq i 5)
  (while (setq point (getpoint))
    (setq text (vla-addtext
      *ModelSpace*
      (strcat "en attente°   " (rtos i 2))
      (vlax-3d-point (list
		       (+ (car point) 21) 
		       (+ (cadr point) 18)
		       )
	)
      8.2
    ) )
    ;(vla-put-rotation text (/ pi 2))
    (setq i (+ i 1))
  )
)

(defun label:gettext ()
  (setq text (car (entsel "Select text: ")))
  (if (or (= text nil) (/= (cdr (assoc 0 (entget text))) "TEXT"))
    (progn
      (princ "No text selected!")
      (label:gettext)
    )
  )
  text
)

(defun c:qq ()
  (setq i 1)
  (while  (setq catt (label:gettext))    
  (setq cdat (entget catt))

  (if (= (cdr (assoc 0 cdat)) "TEXT")
    (progn
      	(setq data (rtos i 2))
      	(if (< i 10)
	  (setq data (strcat "00" data))
	)
      	(if (and (>= i 10) (< i 100))
	  (setq data (strcat "0" data))
	)
      	(setq cdat (subst
		 	(cons '1 data)
		 	(assoc 1 cdat)
			cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      (setq i (+ i 1))
    )
  )
  )
  
)