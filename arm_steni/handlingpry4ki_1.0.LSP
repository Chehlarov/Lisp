;;;written by Nikolay Chehlarov
;;;Sofia, july 2009


(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun hp:getss (/ ss)
  (setq ss (ssget))
)

(defun c:qx()
  (while (not (setq obj (car (entsel)))))
  
  (setq len 0)
  (setq pd (entget  obj))
  (if (/= (cdr (assoc '8 pd)) "COTATION") (alert "The layer is not COTATION!"))
    
	  (setq cp 0)
	  (setq found 0)
	  (repeat (length pd)
	    (if	(= (car (nth cp pd)) 10)
	      (progn
		(if (> found 0)
		  (progn
		    (setq found (+ found 1))
		    (setq cpx (cadr (nth cp pd)))
		    (setq cpy (caddr (nth cp pd)))
		    (setq len  (max
				 (sqrt (+ (* (- cpx bpx) (- cpx bpx))
				(* (- cpy bpy) (- cpy bpy))
				       )
			      )
				 len)
		    )
		    (setq bpx cpx)
		    (setq bpy cpy)
		    
		    
					;(princ "  ")
		  )
		)
		(if (= found 0)
		  (progn
		    (setq found 1)
		    (setq bpx (cadr (nth cp pd)))
		    (setq bpy (caddr (nth cp pd)))
		  )
		)

	      )
	    )
	    (setq cp (+ cp 1))
	  )
	
      (print len)
  (setq e (getint "e= "))
  (print (/ len e))
  (princ "->")
  (setq br (+ (/ len e) 1))
  (setq br (- br (rem br 1)))
  
  (princ br)
;;;za vmykvane
  (while (not (setq obj (car (entsel)))))
  (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "NOMBRE")
		(progn
		  (setq	cdat (subst
			       (cons '1 (rtos br))
			       (assoc 1 cdat)
			       cdat
			     )
		  )
		  (entmod cdat)
		  (entupd catt)
		)
	      )
	      (if (= (cdr (assoc 2 cdat)) "ESPACEMENTS")
		(progn
		  (setq	cdat (subst
			       (cons '1 (rtos e))
			       (assoc 1 cdat)
			       cdat
			     )
		  )
		  (entmod cdat)
		  (entupd catt)
		)
	      )
	       (setq catt (entnext catt))
	    )
  );;;end defun



(defun c:prc()
   (if (setq ss (pr:getss))
    (progn
      (setq ssi 0)
      (setq kol	(list
		  (cons '6 0)
		  (cons '8 0)
		  (cons '10 0)
		  (cons '12 0)
		  (cons '14 0)
		  (cons '16 0)
		  (cons '20 0)
		  (cons '25 0)
		  (cons '32 0)
		)
      )
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (= (cdr (assoc '0 objdata)) "INSERT")
	  (progn
	    (if (or
		  (=  (cdr (assoc '2 objdata))  "Sp\U+00E9cial")
		  (=  (cdr (assoc '2 objdata))  "Barre libre")
		  (=  (cdr (assoc '2 objdata))  "U")
		  (=  (cdr (assoc '2 objdata))  "Epingle")
		  (=  (cdr (assoc '2 objdata))  "Cadre")
		  (=  (cdr (assoc '2 objdata))  "1cr45")
		  (=  (cdr (assoc '2 objdata))  "1cr90")
		  (=  (cdr (assoc '2 objdata))  "2cr45")
		  (=  (cdr (assoc '2 objdata))  "2cr90")
		  )
	      (progn
		;;;namerilo e blok za 4etene i sybira
		(setq catt (entnext obj))
		(while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))

	      (if  (= (cdr (assoc 2 cdat)) "QUANTITE")
		;;;v r1 se pazi imeto na mregata
		(setq q (atof (cdr (assoc 1 cdat))))
	      )
		  
	      (if (= (cdr (assoc 2 cdat)) "NOMBRE")
		;;;v r2 se pazi razmerite na mregata
		(setq n (atof (cdr (assoc 1 cdat))))
	      )
		  
	      (if (= (cdr (assoc 2 cdat)) "NUANCE")
		;;;v r3 se pazi zabelegkata
		(setq HA (atof (cdr (assoc 1 cdat))))
	      )

		  (if (= (cdr (assoc 2 cdat)) "LONGUEUR")
		;;;v r3 se pazi zabelegkata
		(setq l (atof (cdr (assoc 1 cdat))))
	      )
		  
	      (setq catt (entnext catt))
	    )
		;;;ve4e e obhodilo vsi4ki atributi
		(setq kol
		       (subst
			 (cons HA
			       (+ (* q n l) (cdr (assoc HA kol)))
			 )
			 (assoc HA kol)
			 kol
		       )
		)
		

		
		;;;end namerilo e blok za 4etene i sybira
		);;;progn
	      )
	    )
		 
	    )
	
	(setq ssi (+ ssi 1))
	)
      (print kol)
      (princ)
      ;;;napylvane na tablicata
      (setq table (car (entsel "Select table: ")))
      (setq catt (entnext table))
      (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	(setq cdat (entget catt))
	
	(if (= (cdr (assoc 2 cdat)) "L6")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '6 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L8")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '8 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L10")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '10 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L12")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '12 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L14")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '14 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L16")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '16 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L20")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '20 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L25")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '25 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	(if (= (cdr (assoc 2 cdat)) "L32")
	  (progn
	    (setq cdat (subst
			 (cons '1 (rtos (cdr (assoc '32 kol)) 2 1))
			 (assoc 1 cdat)
			 cdat
		       )
	    )
	    (entmod cdat)
	    (entupd catt)
	  )
	)

	
	(setq catt (entnext catt))
	)
      (VL-CMDF "updatefield" table "")
      (VL-CMDF "updatefield" table "")
      (VL-CMDF "updatefield" table "")
    ;  (command "REGEN" )
    ;  (command "REGEN" )
   ;   (command "REGEN" )

      
      ;;;end napylvane na tablicata
      );;;progn
     );;;if
  );;;defun