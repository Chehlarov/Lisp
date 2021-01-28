;;; prodynvane
;;; written by Nikolay Chehlarov
;;; Sofia, june 2008
;;; definirani sa 2 funkcii
;;; prodfile izbirate kolonite koito iskte da zapi6ete vyv file i gotovo
;;; proded obikalq kolonite i pita za beta i perimetyra
;;; prodper izbira se linii i gi zapisva vyv atributa P
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(setq *Space*
       (vla-get-Application
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
;;; this circle is drawn when error is occured at some attribute
(defun draw (center)
  (vla-addCircle
    *ModelSpace*
    (vlax-3d-point center)
    80
  )
)

(defun arm:getss (/ ss)
  (setq ss (ssget))
)
;;;vry6ta palnpoto ime na rabotniq file
(defun nprod:getfilename ()
  (setq	str (vla-get-fullname
	      (vla-get-ActiveDocument (vlax-get-acad-object))
	    )
  )
					;(print str)
  (while (setq inclpos (vl-string-position (ascii "\\") str))
    (setq str (substr str (+ inclpos 2)))
  )
  str
)

;;;zapisva kym file
(defun nprod:tofile (K NK)
  (setq	outfile
	 (strcat
	   "C:/Documents and Settings/Nikolay/Desktop/New Folder (2)/"
	   (nprod:getfilename)
	   ".txt"
	 )
  )
  (setq f (open outfile "w"))

  (setq i 0)
  (while (< i (length K))
    (setq j 0)
    (if	(/= (car (nth i K)) " ")
      (princ "Ê" f)
    )
    (while (< j (length (nth i K)))
      (princ (nth j (nth i K)) f)
      (princ "\t" f)
      (setq j (+ j 1))
    )

    (princ "\n" f)
    (setq i (+ i 1))
  )
  (setq i 0)
  (while (< i (length NK))
    (setq j 0)
    (if	(/= (car (nth i NK)) " ")
      (princ "ÍÊ" f)
    )
    (while (< j (length (nth i NK)))
      (princ (nth j (nth i NK)) f)
      (princ "\t" f)
      (setq j (+ j 1))
    )

    (princ "\n" f)
    (setq i (+ i 1))
  )

  (close f)
)

;;;retunrs only the number of the column searches the integer after "-"
(defun nikprod:num (str)
  (setq dashpos (+ (vl-string-position (ascii "-") str) 2))
					;(setq ddpos ( + (vl-string-position (ascii ":") str) 0))
  (if (= (setq n (atoi (substr str dashpos))) 0)
    (progn (alert "Problem ") (setq problem 1))
  )
  n
)

;;;sortira spisak
(defun nprod:sort (dat n)
  (print "see this :  ")
  (print dat)
  (print "to here  ")
  (setq out nil)
  (setq i n)
  (while (> i 0)
    (setq j 0)
    (setq ima 0)
    (while (< j (length dat))
      (if (= (car (nth j dat)) i)
	(progn
	  (if (= i n)
	    (setq out (list (nth j dat)))
	  )
	  (if (/= i n)
	    (setq out (cons (nth j dat) out))
	  )
	  (setq ima (+ ima 1))
	)
      )
      (setq j (+ j 1))
    )
    (if	(= ima 0)
      (progn
	(if (= i n)
	  (setq out (list " "))
	)
	(if (/= i n)
	  (setq out (cons (list " ") out))
	)
      )
    )
    (if	(> ima 1)
      (alert
	(strcat	"More than one column with same name!!! \nÊ(ÍÊ)"
		(rtos (car (car out)))
	)
      )
    )
					;(alert (rtos (car (car out))))
    (setq i (- i 1))
  )
  out
)

;;;;tazifunkciq vzema dalgnia na linq (linii) i gi slaga v attributa P
(defun c:prodper ()
  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      (setq len 0)
      (while (setq obj (ssname ss ssi))
	(setq obja (vlax-ename->vla-object obj))
	(setq len (+ len (vla-get-length obja)))
	(setq ssi (+ ssi 1))
      )
    )
  )
  (while (not (setq obj (car (entsel "select kolumn blok: ")))))
  (setq objdata (entget obj))
  (if (and (= (cdr (assoc '0 objdata)) "INSERT")
	   (= (cdr (assoc '2 objdata)) "Kolona")
      )
    (progn
      (setq catt (entnext obj))
      (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	(setq cdat (entget catt))
	(if (= (cdr (assoc 2 cdat)) "P")
	  (progn
	    (print (strcat "Medium perimeter: " (rtos len)))
	    (setq cdat (subst
			 (cons '1 (rtos len))
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
    )
  )
)

;;;tazi funkciq requestva informaciq za selectiranite koloni
(defun c:proded	()
  (if (setq ss (arm:getss))
    (progn
       (setq msg "Enter h0: [cm]")
	    (setq h0 (atof (getstring msg)))
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "Kolona")
	    )
	  (progn


	    (print (cdr (assoc '10 objdata)))
	    (vla-ZoomCenter
	      *Space*
	      (vlax-3d-point (cdr (assoc '10 objdata)))
	      700
	    )

	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "NM")
		(print (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "B")
		(setq curB (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "P")
		(setq P (atof (cdr (assoc 1 cdat))))
	      )
	      (if (= (cdr (assoc 2 cdat)) "X")
		(setq x (atof (cdr (assoc 1 cdat))))
	      )
	      (if (= (cdr (assoc 2 cdat)) "Y")
		(setq y (atof (cdr (assoc 1 cdat))))
	      )
	      (setq catt (entnext catt))
	    )
;;;getting user input for beta
	    (setq msg (strcat "Using beta " curB "?"))
	    (setq B (getstring msg))
	    (if	(= B "")
	      (setq B curB)
	    )
;;;geting user input for perimeter
	    
	    (initget 1 "I S L E P")
	    (setq msg (strcat "Is this column [Internal, cut Short, cut Long, Edge, sPecial] with h0="
			      (rtos h0)
		      )
	    )
	    (setq answ (getkword msg))
	    (if	(= answ "P")
	      (draw (cdr (assoc '10 objdata)))
	    )
	    (if	(= answ "I")
	      (setq P (* (+ x y (* 2 h0)) 2))
	    )
	    (if	(= answ "S")
	      (progn
	      (if (<= x y) (setq P  (+ x y y (* 2 h0))))
	      (if (> x y) (setq P  (+ x x y (* 2 h0))))
	      )
	    )
	    (if	(= answ "L")
	      (progn
	      (if (>= x y) (setq P  (+ x y y (* 2 h0))))
	      (if (< x y) (setq P  (+ x x y (* 2 h0))))
	      )
	    )
	    (if	(= answ "E")
	      (setq P  (+ x y  h0)) 
	    )
;;;modifing attribute
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
	      (if (= (cdr (assoc 2 cdat)) "B")
		(progn
		  (setq	cdat (subst
			       (cons '1 B)
			       (assoc 1 cdat)
			       cdat
			     )
		  )
		  (entmod cdat)
		  (entupd catt)
		)
	      )
	      (if (= (cdr (assoc 2 cdat)) "P")
		(progn
		  (setq	cdat (subst
			       (cons '1 (rtos P))
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
	  )
	)
	(setq ssi (+ ssi 1))
      )
    )
  )
)


;;;tazi funkciq zapisva attributite ot kolonite vyv file
(defun c:prodfile ()
  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      (setq store nill)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
;;;finding block ;;
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "Kolona")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      (if (= (cdr (assoc 2 cdat)) "NM")
		(setq name (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "X")
		(setq x (atof (cdr (assoc 1 cdat))))
	      )
	      (if (= (cdr (assoc 2 cdat)) "Y")
		(setq y (atof (cdr (assoc 1 cdat))))
	      )
	      (if (= (cdr (assoc 2 cdat)) "P")
		(setq p (atof (cdr (assoc 1 cdat))))
	      )
	      (if (= (cdr (assoc 2 cdat)) "B")
		(setq b (atof (cdr (assoc 1 cdat))))
	      )

	      (setq catt (entnext catt))
	    )
	    (if	(= ssi 0)
	      (setq store (list (list name x y p b)))
	    )
	    (if	(/= ssi 0)
	      (setq store (cons (list name x y p b) store))
	    )
	  )
	)
	(setq ssi (+ ssi 1))
      )
    )
  )
;;; putting order in name attribute

  (print)
  (setq	i 0
	Kmax 0
	NKmax 0
  )
  (while (< i (length store))
;;;finding K
    (if	(= (substr (car (nth i store)) 1 1) "Ê")
      (progn
	(if (= i 0)
	  (setq	K    (list (cons (nikprod:num (car (nth i store)))
				 (cdr (nth i store))
			   )
		     )
		Kmax (nikprod:num (car (nth i store)))
	  )
	)
	(if (/= i 0)
	  (progn
	    (setq K (cons (cons	(nikprod:num (car (nth i store)))
				(cdr (nth i store))
			  )
			  K
		    )
	    )
	    (if	(< Kmax (nikprod:num (car (nth i store))))
	      (setq Kmax (nikprod:num (car (nth i store))))
	    )
	  )
;;;progn
	)
      )
    )
;;; Finding ÍÊ
    (if	(= (substr (car (nth i store)) 1 2) "ÍÊ")
      (progn
	(if (= i 0)
	  (setq	NK    (list (cons (nikprod:num (car (nth i store)))
				  (cdr (nth i store))
			    )
		      )
		NKmax (nikprod:num (car (nth i store)))
	  )
	)
	(if (/= i 0)
	  (progn
	    (setq NK (cons (cons (nikprod:num (car (nth i store)))
				 (cdr (nth i store))
			   )
			   NK
		     )

	    )
	    (if	(< NKmax (nikprod:num (car (nth i store))))
	      (setq NKmax (nikprod:num (car (nth i store))))
	    )
	  )
	  ;;progn
	)
      )
    )
    (setq i (+ i 1))
  )


;;;sorting


  (setq K (nprod:sort K Kmax))
  (setq NK (nprod:sort NK NKmax))

  (print K)
  (print NK)
  (nprod:tofile K NK)
  (print)
)