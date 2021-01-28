;Incializationg
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)


;;;111 4ertae kryg za problem
(defun sp:drawc	(center)
  (if (= (getvar 'INSUNITS) 5);5 - cm
    (vla-addCircle
      *ModelSpace*
      (vlax-3d-point center)
      200
    )
    ((vla-addCircle
       *ModelSpace*
       (vlax-3d-point center)
       2
     )
    )
  )
)
;;;111 end 4ertae kryg za problem
;;;222
(defun sp:getss (/ ss)
  (setq ss (ssget))
)
;;;222 end
;;;333  selectiong output table for bars or nets
(defun pr:gettable ()
  (setq table (car (entsel "Select table: ")))
  (if (or (= table nil) (/= (cdr (assoc 2 (entget table))) "bartable"))
    (progn
      (princ "Right table not selected!")
      (pr:gettable)
      )
      )
  table
  )

;;;;

(defun ns:gettable ()
  (setq table (car (entsel "Select table: ")))
  (if (or (= table nil) (/= (cdr (assoc 2 (entget table))) "TStable"))
    (progn
      (princ "Right table not selected!")
      (n:gettable)
      )
      )
  table
  )
;;;333 end selectiong output table for bars or nets


;;;444main control unit
;;;4444    4444   4444    4444   4444    
;;;    4444    4444   4444    4444   4444
;;;4444    4444   4444    4444   4444    
;;;    4444    4444   4444    4444   4444
(defun c:sp ()
  (setq
    datatype "bars"
	dialogLoaded T
	dialogShow T
  ) 

  (if (= -1 (setq dcl_id (load_dialog "spectot.dcl")))
    (progn
      (princ "\nCannot load spectot.dcl")
      (setq dialogLoaded nil)
    ) 
  ) 

  (if (and dialogLoaded
	   (not (new_dialog "loadiDialog" dcl_id))
      ) 
    (progn

      (princ "\nCannot show dialog loadiDialog")
      (setq dialogShow nil)
    ) 
  )


  (if (and dialogLoaded dialogShow)
    (progn
      ;; Set the initial state of the tiles
      (set_tile "sp_is" (rtos (/ 6 5.6) 2 4))
      (set_tile "sp_iw" (rtos 1.00 2 2))
      (mode_tile "n_opt" 1)
      
      
      (action_tile
	"sp_bars"
	(strcat
	"(setq datatype \"bars\")"
	"(mode_tile \"n_opt\" 1)"
	)
      )
      (action_tile
	"sp_nets"
	(strcat
	"(setq datatype \"nets\")"
	"(mode_tile \"n_opt\" 0)"
	)
      )

      (action_tile
	"sarche"
	(strcat	"(progn"
		"(setq datatype \"Sarche\")"
		"(done_dialog) (setq UserClick T))"
	)
      )

      (action_tile
	"sarchetot"
	(strcat	"(progn"
		"(setq datatype \"Sarchetot\")"
		"(done_dialog) (setq UserClick T))"
	)
      )
              
      (action_tile "cancel" "(done_dialog) (setq UserClick nil)")
      (action_tile
	"accept"
	(strcat	"(progn"
		"(setq incslab (atof (get_tile \"sp_is\")))"
		"(setq incwall (atof (get_tile \"sp_iw\")))"
		"(setq Fdim (atof (get_tile \"sp_cdim\")))"
		"(setq Fround (atof (get_tile \"sp_r\")))"
		"(done_dialog) (setq UserClick T))"
	)
      )
      
      (start_dialog)

      (unload_dialog dcl_id)


      (if UserClick			; User clicked Ok
	;; Build the resulting data
	(progn
	  (if (= datatype "bars") (sp:prc) )
	  (if (= datatype "nets") (sp:net incslab incwall Fdim Fround))
	  (if (= datatype "Sarche") (sp:arche) )
	  (if (= datatype "Sarchetot") (sp:archetot) )
	 
	)
      )

    )
  )
  Result
) ;_ end of defun

;;;end 444 main control unit
;;;4444    4444   4444    4444   4444    
;;;    4444    4444   4444    4444   4444
;;;4444    4444   4444    4444   4444    
;;;    4444    4444   4444    4444   4444


;;; 4a4a  4a4a  4a4a
;;; 	4a4a  4a4a  4a4a
;;; 4a4a  4a4a  4a4a
;pravi red sys specifikaciq ot arche
(defun sp:arche()
    (if	(setq ss (sp:getss))
      (progn
	
	(setq kol (list
		    (cons '6 0)
		    (cons '8 0)
		    (cons '10 0)
		    (cons '12 0)
		    (cons '14 0)
		    (cons '16 0)
		    (cons '20 0)
		    (cons '25 0)
		    (cons '32 0)
		    (cons '40 0)
		  )
	)
	(setq kolAD (list
		    (cons '6 0)
		    (cons '8 0)
		    (cons '10 0)
		    (cons '12 0)
		    (cons '14 0)
		    (cons '16 0)
		    (cons '20 0)
		    (cons '25 0)
		    (cons '32 0)
		    (cons '40 0)
		  )
	)
	(setq ssi 0)
	(while (setq obj (ssname ss ssi))
	  (setq objdata (entget obj))
	  (if (= (cdr (assoc '0 objdata)) "TEXT")
	    (progn
	      ;;;; looing fo HA
	      (if (wcmatch (cdr (assoc '1 objdata)) "*HA*")
		(progn
		  (setq ssj 0)
		  (while (setq obj2 (ssname ss ssj))
		    (setq objdata2 (entget obj2))
		    (if	(and
			  (= (cdr (assoc '0 objdata2)) "TEXT")
			  (/= (cdr (assoc '5 objdata2))
			      (cdr (assoc '5 objdata))
			  )
			  (< (abs(-	(cadr (cdr (assoc '10 objdata2)))
				(cadr (cdr (assoc '10 objdata)))
			     ))
			     2
			  )
			  (> 	(car (cdr (assoc '10 objdata2)))
				(car (cdr (assoc '10 objdata)))
			  )
			)
		      (progn
			(setq hi (vl-string-search "HA" (cdr (assoc '1 objdata))))
			(setq n (atoi (substr (cdr (assoc '1 objdata)) 1  hi)))
			(setq HA (atoi (substr (cdr (assoc '1 objdata)) (+ hi 3))))
			(setq l (atof (cdr (assoc '1 objdata2))))
			(print l)
			(print n)
			(setq kol
			       (subst
				 (cons HA
				       (+ (* n l 0.01) (cdr (assoc HA kol)))
				 )
				 (assoc HA kol)
				 kol
			       )
			)
		      )
		    )
		    (setq ssj (+ ssj 1))
		  )
		)
	      )
	      ;;;; end looing fo HA

	      ;;;;looing fo AD
	      (if (wcmatch (cdr (assoc '1 objdata)) "*AD*")
		(progn
		  (setq ssj 0)
		  (while (setq obj2 (ssname ss ssj))
		    (setq objdata2 (entget obj2))
		    (if	(and
			  (= (cdr (assoc '0 objdata2)) "TEXT")
			  (/= (cdr (assoc '5 objdata2))
			      (cdr (assoc '5 objdata))
			  )
			  (< (abs(-	(cadr (cdr (assoc '10 objdata2)))
				(cadr (cdr (assoc '10 objdata)))
			     ))
			     2
			  )
			)
		      (progn
			(setq hi (vl-string-search "AD" (cdr (assoc '1 objdata))))
			(setq n (atoi (substr (cdr (assoc '1 objdata)) 1  hi)))
			(setq HA (atoi (substr (cdr (assoc '1 objdata)) (+ hi 3))))
			(setq l (atof (cdr (assoc '1 objdata2))))
			(setq kolAD
			       (subst
				 (cons HA
				       (+ (* n l 0.01) (cdr (assoc HA kolAD)))
				 )
				 (assoc HA kolAD)
				 kolAD
			       )
			)
		      )
		    )
		    (setq ssj (+ ssj 1))
		  )
		)
	      )
	      
	      ;;;; end looing fo AD
	    )

	  )

	  (setq ssi (+ ssi 1))
	);end while
	(print kol)
	(print kolAD)
	;;;writing to smallsp
	(setq ip (getpoint "select insertion of smallsp"))
	(setq ib (vla-insertblock
		   *Modelspace*
		   (vlax-3d-point ip)
		   "Smallsp"
		   1 1 1 0
		 )
	)
	(setq atts (vla-getattributes ib))
	(setq i 0)
	(setq kolstr "")
	(while (nth i kol)
	  (setq kolstr (strcat kolstr (strcat (rtos (car (nth i kol))) "-" (rtos (cdr (nth i kol))) " ")))
	  (setq i (+ i 1))
	  )
	(vla-put-textstring (vlax-safearray-get-element (vlax-variant-value atts) 1) kolstr)
	;;; may be used also - object.SetBlockAttributeValue attdefId, value  
	;;;sega za AD
	(setq i 0)
	(setq kolstr "")
	(while (nth i kolAD)
	  (setq kolstr (strcat kolstr (strcat (rtos (car (nth i kolAD))) "-" (rtos (cdr (nth i kolAD))) " ")))
	  (setq i (+ i 1))
	  )
	(vla-put-textstring (vlax-safearray-get-element (vlax-variant-value atts) 0) kolstr)
	;;; end sega za AD
	;;;end writing to smallsp
	(sp:arche)
	
      )
    )
  
  );defun
;;; end  4a4a  4a4a  4a4a
;;; end     4a4a  4a4a  4a4a
;;;end  4a4a  4a4a  4a4a

;;; 4b4b   4b4b    4b4b
;;; 4b4b   4b4b    4b4b
;;; 4b4b   4b4b    4b4b
(defun sp:archetot()
   (if (setq ss (sp:getss))
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
		  (cons '40 0)
		)
      )
      (setq kolAD	(list
		  (cons '6 0)
		  (cons '8 0)
		  (cons '10 0)
		  (cons '12 0)
		  (cons '14 0)
		  (cons '16 0)
		  (cons '20 0)
		  (cons '25 0)
		  (cons '32 0)
		  (cons '40 0)
		)
      )
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (= (cdr (assoc '0 objdata)) "INSERT")
	  (progn
	    (if (=  (cdr (assoc '2 objdata))  "Smallsp")
	      (progn
		(setq atts (vla-getattributes (vlax-ename->vla-object obj)))
		(setq str (vla-get-textstring (vlax-safearray-get-element (vlax-variant-value atts) 1)))
		(while (and (/= str "") (/= str " "))
			(setq hi (vl-string-search "-" str))
			(setq hs (vl-string-search " " str))
			(setq HA (atoi (substr str 1 hi)))
			(setq  l (atof(substr str (+ hi 2) (- hs hi 1))))

		  (setq	kol
			 (subst
			   (cons HA
				 (+ l (cdr (assoc HA kol)))
			   )
			   (assoc HA kol)
			   kol
			 )
		  )
		  
		  	(setq str (substr str (+ hs 2)))
		  )
		;;;sabralo e vsi4ki HA pry4ki v kol
		(setq str (vla-get-textstring (vlax-safearray-get-element (vlax-variant-value atts) 0)))
		(while (and (/= str "") (/= str " "))
			(setq hi (vl-string-search "-" str))
			(setq hs (vl-string-search " " str))
			(setq HA (atoi (substr str 1 hi)))
			(setq  l (atof(substr str (+ hi 2) (- hs hi 1))))

		  (setq	kolAD
			 (subst
			   (cons HA
				 (+ l (cdr (assoc HA kolAD)))
			   )
			   (assoc HA kolAD)
			   kolAD
			 )
		  )
		  
		  	(setq str (substr str (+ hs 2)))
		  )
		

		);;;progn
	      )
	    )
		 
	    )
	
	(setq ssi (+ ssi 1))
	)
      (print kol)
      (princ)
      ;;;napylvane na tablicata
      (sp:drawpd kol "%%URECAPITULATION DES ACIERS HA")
      (sp:drawpd kolAD "%%URECAPITULATION DES ACIERS AD")
      
      ;;;end napylvane na tablicata
      );;;progn
     );;;if
  )
;;; end 4b4b   4b4b    4b4b
;;; end 4b4b   4b4b    4b4b
;;; end 4b4b   4b4b    4b4b


;;;4c4c 4c4c module for updationg bar table by drawing texts
;;;4c4c 4c4c module for updationg bar table by drawing texts
;;;4c4c 4c4c module for updationg bar table by drawing texts
(defun sp:drawpd(kol title)
  (setq suml 0)
  (setq sump 0)
  ;(vla-put-activetextstyle (vla-get-ActiveDocument (vlax-get-acad-object)) "Table")
  (setq poids(list
		  (cons '6 0.22195352)
		  (cons '8 0.394584)
		  (cons '10 0.6165375)
		  (cons '12 0.887814)
		  (cons '14 1.2084136)
		  (cons '16 1.578336)
		  (cons '20 2.46615)
		  (cons '25 3.853359)
		  (cons '32 6.3133445)
		  (cons '40 9.864)
		)
	 )

  (setq i 0)
  (setq ip (getpoint (strcat "Select insertion point for " title)))
  ;;; slagane na podlogkata
  (setq ib (vla-insertblock
		   *Modelspace*
		   (vlax-3d-point ip)
		   "BaseBarTable2"
		   1 1 1 0
		 )
	)
  ;;;end slagane na podlogkata
  
  (while (nth i kol)
    (setq tobj (vla-AddText
		 *ModelSpace*
		 (rtos (cdr (nth i kol)) 2 1)
		 (vlax-3d-point ip)
		 7.5
	       )
    )
    (vla-put-Alignment tobj acAlignmentBottomCenter)
    (vla-put-TextAlignmentPoint  tobj (vlax-3d-point (list (+ (car ip) 97.5) (- (cadr ip) 38.5 (* i 12.5)) 0)))
    (vla-put-stylename tobj "Table")
    (vla-put-ObliqueAngle  tobj 0)
    (vla-put-scalefactor  tobj 1)
    (setq suml (+ suml (cdr (nth i kol))))

    
    (setq tobj (vla-AddText
		 *ModelSpace*
		 (rtos (* (cdr (nth i kol)) (cdr (nth i poids))) 2 1)
		 (vlax-3d-point ip)
		 7.5
	       )
    )

    (vla-put-Alignment tobj acAlignmentBottomCenter)
    (vla-put-TextAlignmentPoint  tobj (vlax-3d-point (list (+ (car ip) 162.5) (- (cadr ip) 38.5 (* i 12.5)) 0)))
    (vla-put-stylename tobj "Table")
    (vla-put-ObliqueAngle  tobj 0)
    (vla-put-scalefactor  tobj 1)

    (setq sump (+ sump (* (cdr (nth i kol)) (cdr (nth i poids)))))

    
    (setq i (+ i 1))
    )

  ;;;sums and title
      (setq tobj (vla-AddText
		 *ModelSpace*
		 title
		 (vlax-3d-point ip)
		 7.5
	       )
    )
    (vla-put-Alignment tobj acAlignmentBottomCenter)
    (vla-put-TextAlignmentPoint  tobj (vlax-3d-point (list (+ (car ip) 97.5) (+ (cadr ip) 12) 0)))
    (vla-put-stylename tobj "Table")
    (vla-put-ObliqueAngle  tobj 0)
    (vla-put-scalefactor  tobj 1)

  
      (setq tobj (vla-AddText
		 *ModelSpace*
		 (rtos suml 2 1)
		 (vlax-3d-point ip)
		 7.5
	       )
    )
    (vla-put-Alignment tobj acAlignmentBottomCenter)
    (vla-put-TextAlignmentPoint  tobj (vlax-3d-point (list (+ (car ip) 97.5) (- (cadr ip) 166) 0)))
    (vla-put-stylename tobj "Table")
    (vla-put-ObliqueAngle  tobj 0)
    (vla-put-scalefactor  tobj 1)

    (setq tobj (vla-AddText
		 *ModelSpace*
		 (rtos sump 2 1)
		 (vlax-3d-point ip)
		 7.5
	       )
    )
    (vla-put-Alignment tobj acAlignmentBottomCenter)
    (vla-put-TextAlignmentPoint  tobj (vlax-3d-point (list (+ (car ip) 162.5) (- (cadr ip) 166) 0)))
    (vla-put-stylename tobj "Table")
    (vla-put-ObliqueAngle  tobj 0)
    (vla-put-scalefactor  tobj 1)
  
    (setq tobj (vla-AddText
		 *ModelSpace*
		 (rtos
		   (* 200 (sqrt (/ sump (* suml pi 78.5))))
		  2 1)
		 (vlax-3d-point ip)
		 7.5
	       )
    )
    (vla-put-Alignment tobj acAlignmentBottomCenter)
    (vla-put-TextAlignmentPoint  tobj (vlax-3d-point (list (+ (car ip) 97.5) (- (cadr ip) 191) 0)))
    (vla-put-stylename tobj "Table")
    (vla-put-ObliqueAngle  tobj 0)
    (vla-put-scalefactor  tobj 1)
  ;;;end sums
  )

;;;end 4c4c 4c4c module for updationg bar table by drawing texts
;;;end 4c4c 4c4c module for updationg bar table by drawing texts

;;; 555 bar unit 555 bar unit
(defun sp:prc()
   (if (setq ss (sp:getss))
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
		  (cons '40 0)
		)
      )
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (= (cdr (assoc '0 objdata)) "INSERT")
	  (progn
	    (if (or
		  (=  (cdr (assoc '2 objdata))  "Sp\U+00E9cial")
		  (=  (cdr (assoc '2 objdata))  "Sp\\U+00E9cial")
		  (=  (cdr (assoc '2 objdata))  "Spécial")
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
		(if (and (assoc HA kol) (> (* q n l) 0))
		  (setq	kol
			 (subst
			   (cons HA
				 (+ (* q n l) (cdr (assoc HA kol)))
			   )
			   (assoc HA kol)
			   kol
			 )
		  )
;;;else
		  (progn
		    (alert "problem look for circle")
		    (sp:drawc (cdr (assoc '10 objdata)))

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

      (sp:drawpd kol "%%URECAPITULATION DES ACIERS HA")
     
      
      ;;;end napylvane na tablicata
      );;;progn
     );;;if
  )
;;;end  555 bar unit 555 bar unit
;;;end  555 bar unit 555 bar unit
;;;end  555 bar unit 555 bar unit

;;; 666 net unit 666 net unit
;;; 666 net unit 666 net unit
;;; 666 net unit 666 net unit
(defun sp:net (incslab incwall Fdim Fround)
  (if (setq ss (sp:getss))
    (progn
      (setq ssi 0)
      (setq kol	(list
		  (cons 'PAF10 0)
		  (cons 'ST20 0)
		  (cons 'ST25 0)
		  (cons 'ST30 0)
		  (cons 'ST35 0)
		  (cons 'ST50 0)
		  (cons 'ST60 0)
		  (cons 'ST15C 0)
		  (cons 'ST25C 0)
		  (cons 'ST25CS 0)
		  (cons 'ST40C 0)
		  (cons 'ST50C 0)
		  (cons 'ST65C 0)
		  (cons 'PAFV 0)
		)
      )
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))		
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (=  (cdr (assoc '2 objdata))  "mrega")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      ;;;ST 10 v ora se otnasq za dolna armirovka a ST25 za gorna taka sa imenata na attributite
	      (if  (= (cdr (assoc 2 cdat)) "ST10")
		;;;v r1 se pazi imeto na mregata
		(setq r1 (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "DIM")
		;;;v r2 se pazi razmerite na mregata
		(setq r2 (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "NOTE")
		;;;v r3 se pazi zabelegkata
		(setq r3 (cdr (assoc 1 cdat)))
	      )
	      (setq catt (entnext catt))
	    )
;;;;;;;
	    ;;;opravq imeto na mregata
	    (setq sppos (vl-string-position (ascii " ") r1))
	    (if (/= sppos nil) (setq r1 (substr r1 1 (+ sppos 0))))
	    

	    ;;;smqta razmerite
	    (setq r2 (vl-string-left-trim "(" r2))
	    (setq r2 (vl-string-right-trim ")" r2))
	    
	    (setq xpos (vl-string-position (ascii "x") r2))
	(if (/= xpos nil)
	  (progn
	    (setq B (atof (substr r2 1 xpos)))
	    (setq L (atof (substr r2 (+ xpos 2))))
	  )
	  ;else
	  (setq B 0
		L 0)
	)


	    ;;;;opredelq dali e ednostranna ili dvustranna mregata
	    (setq n 0)
	    (print r3)
	    (if (= r3 "par face") (setq n 2))
	    (if (= r3 "axe") (setq n 1))
	    (if (= r3 "ax\\U+00E9") (setq n 1))
	    (if (= r3 "ax\U+00E9") (setq n 1)) 
	    (if (= r3 "axé") (setq n 1))
	    (if (= r3 "") (setq n 1))
	    (if (= r3 " ") (setq n 1))
	    (if (= r3 "ext.face") (setq n 1))
	    (if (= r3 "face ext.") (setq n 1))
	    (if (= r3 "2xface ext.") (setq n 2))
	    (if (= r3 "int.face") (setq n 1))
	    (if (= r3 "face int.") (setq n 1))
	    (if (= r3 "2xface int.") (setq n 2))
	    (if (= r3 "face bas") (setq n 1))
	    (if (= r3 "face haut") (setq n 1))
	    (if (= r3 "face sup") (setq n 1))
	    (if (= r3 "deux lits") (setq n 2))
	    (if (= r3 "1er lit") (setq n 1))
	    (if (= r3 "2e lit") (setq n 1))
	    (if (= r3 "en tiroir") (setq n 2))
	    (if (= r3 "terre") (setq n 1))
	    (if (= r3 "rue") (setq n 1))
	    (if (= r3 "face terre") (setq n 1))
	    (if (= r3 "face rue") (setq n 1))
	    (if (= r3 "face rampe") (setq n 1))
	    (if (= r3 "TS de peau") (setq n 1))

	    
	    (if (<= (* n L B) 0)
	      (progn (princ "problem")
		(alert "problem look for circle")
		(sp:drawc (cdr (assoc '10 objdata)) )
		)
	      )
	    ;;;; proverki dali razmerite na mregata ne sa po-golemi ot dopustimite
	    (setq tmpl L)
	    (setq L (max B L))
	    (setq B (min B tmpl))

	    (if (= Fdim 1)
	      (progn
	    (setq flpd 1)
	    (if	(and (= r1 "PAF10") (> L 420))
		(progn
		  (alert "Very long net! Look for the circle")
		  (sp:drawc (cdr (assoc '10 objdata)))
		  (setq flpd 0)
		)
	      )
	    (if	(and (= r1 "PAFV") (> L 320))
		(progn
		  (alert "Very long net! Look for the circle")
		  (sp:drawc (cdr (assoc '10 objdata)))
		  (setq flpd 0)
		)
	      )

	      (if (and (= flpd 1) (> L 600))
		(progn
		  (alert "Very long net! Look for the circle")
		  (sp:drawc (cdr (assoc '10 objdata)))
		)
	      )
	    
	    (if  (> B 240)
		(progn
		  (alert "Very wide net! Look for the circle")
		  (sp:drawc (cdr (assoc '10 objdata)))
		)
	      )
	    )
	      )
;;;;;;;;;;;;;;;;;;;;;;;
	    (setq fl 0)
	    
	    (if	(= r1 "ST15C")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST15C
			       (+ (/ (* n L B 0.0001 incwall) 9.60) (cdr (assoc 'ST15C kol)))
			 )
			 (assoc 'ST15C kol)
			 kol
		       )
		)
	      )
	    )
	    
	    (if	(and (= r1 "ST25CS") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25CS
			       (+ (/ (* n L B 0.0001 incwall) 7.20) (cdr (assoc 'ST25CS kol)))
			 )
			 (assoc 'ST25CS kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST25C") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25C
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST25C kol)))
			 )
			 (assoc 'ST25C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST40C") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST40C
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST40C kol)))
			 )
			 (assoc 'ST40C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST50C") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST50C
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST50C kol)))
			 )
			 (assoc 'ST50C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST65C") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST65C
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST65C kol)))
			 )
			 (assoc 'ST65C kol)
			 kol
		       )
		)
	      )
	    )
	    
	    (if	(and (= r1 "PAFV") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'PAFV
			       (+ (/ (* n L B 0.0001 incwall) 7.68) (cdr (assoc 'PAFV kol)))
			 )
			 (assoc 'PAFV kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "PAF10") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'PAF10
			       (+ (/ (* n L B 0.0001 incwall) 10.08) (cdr (assoc 'PAF10 kol)))
			 )
			 (assoc 'PAF10 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST20") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST20
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST20 kol)))
			 )
			 (assoc 'ST20 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST25") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST25 kol)))
			 )
			 (assoc 'ST25 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST30") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST30
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST30 kol)))
			 )
			 (assoc 'ST30 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST35")(= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST35
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST35 kol)))
			 )
			 (assoc 'ST35 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST50") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST50
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST50 kol)))
			 )
			 (assoc 'ST50 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (= r1 "ST60") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST60
			       (+ (/ (* n L B 0.0001 incwall) 14.40) (cdr (assoc 'ST60 kol)))
			 )
			 (assoc 'ST60 kol)
			 kol
		       )
		)
	      )
	    )


	    
	    (if	(/= fl 1)
	      (progn
		(alert "problem look for circle")
		(sp:drawc (cdr (assoc '10 objdata)) )
	      )
	    )
	  )
	)
	(setq ssi (+ ssi 1))
      )
     (print kol)

      ;;;11111
      ;;;11111 vzima arm za plochata
      (VL-CMDF "updatefield" ss "")
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))		
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (substr (cdr (assoc '2 objdata)) 1 2) "*U")
	    )
	  (progn
	    (setq catt (entnext obj))
	    (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
	      (setq cdat (entget catt))
;;;getting current attribute
	      ;;;ST 10 v ora se otnasq za dolna armirovka a ST25 za gorna taka sa imenata na attributite
	      (if (or (= (cdr (assoc 2 cdat)) "ST25")
		  (= (cdr (assoc 2 cdat)) "ST10"))
		(setq r1 (cdr (assoc 1 cdat)))
	      )
	      (if (= (cdr (assoc 2 cdat)) "DIM")
		(setq r2 (cdr (assoc 1 cdat)))
	      )
	      (setq catt (entnext catt))
	    )
;;;;;;;
	    (setq sppos (vl-string-position (ascii " ") r1))
	    (setq n (substr r1 1 (+ sppos 0)))
	    (setq n (vl-string-translate "," "." n))
	    (setq n (atof n))

;;;ZA KEY DA GO NAPRAQ ILI DO SLEDVA6TIQ INTERVAL AKO IMA AXIS SLED TOVA
					;(setq key (substr r1 (+ sppos 2)))

	    (setq eqpos (vl-string-position (ascii "=") r2))
	    (setq xpos (vl-string-position (ascii "x") r2))

	    (setq L (atof (substr r2 (+ eqpos 2) (- xpos eqpos 1))))

	    (setq B (atof (substr r2 (+ xpos 2))))
;;;proverka za dylginata
	    (if (= Fdim 1)
	      (progn
	    (setq flpd 1)
	    (if	(and (wcmatch r1 "*PAF10*") (> L 420))
		(progn
		  (alert "Very long net! Look for the circle")
		  (sp:drawc (cdr (assoc '10 objdata)))
		  (setq flpd 0)
		)
	      )
	    (if	(and (wcmatch r1 "PAFV") (> L 320))
		(progn
		  (alert "Very long net! Look for the circle")
		  (sp:drawc (cdr (assoc '10 objdata)))
		  (setq flpd 0)
		)
	      )

	      (if (and (= flpd 1) (> L 600))
		(progn
		  (alert "Very long net! Look for the circle")
		  (sp:drawc (cdr (assoc '10 objdata)))
		)
	      )
	    )
	      )
	    
;;;;;;;;;;;;;;;;;;;;;;;
	    (setq fl 0)
	    
	    (if	(wcmatch r1 "*ST15C*")
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST15C
			       (+ (/ (* n L B 0.0001 incslab)  9.60 ) (cdr (assoc 'ST15C kol)))
			 )
			 (assoc 'ST15C kol)
			 kol
		       )
		)
	      )
	    )
	    
	    (if	(and (wcmatch r1 "*ST25CS*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25CS
			       (+ (/ (* n L B 0.0001 incslab)  7.20 ) (cdr (assoc 'ST25CS kol)))
			 )
			 (assoc 'ST25CS kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST25C*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25C
			       (+ (/ (* n L B 0.0001 incslab)  14.40 ) (cdr (assoc 'ST25C kol)))
			 )
			 (assoc 'ST25C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST40C*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST40C
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST40C kol)))
			 )
			 (assoc 'ST40C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST50C*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST50C
			       (+ (/ (* n L B 0.0001 incslab)  14.40 ) (cdr (assoc 'ST50C kol)))
			 )
			 (assoc 'ST50C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST65C*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST65C
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST65C kol)))
			 )
			 (assoc 'ST65C kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*PAFV*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'RAFV
			       (+ (/ (* n L B 0.0001 incslab) 7.68 ) (cdr (assoc 'RAFV kol)))
			 )
			 (assoc 'RAFV kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*PAF10*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'PAF10
			       (+ (/ (* n L B 0.0001 incslab) 10.08 ) (cdr (assoc 'PAF10 kol)))
			 )
			 (assoc 'PAF10 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST20*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST20
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST20 kol)))
			 )
			 (assoc 'ST20 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST25*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST25
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST25 kol)))
			 )
			 (assoc 'ST25 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST30*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST30
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST30 kol)))
			 )
			 (assoc 'ST30 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST35*")(= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST35
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST35 kol)))
			 )
			 (assoc 'ST35 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST50*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST50
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST50 kol)))
			 )
			 (assoc 'ST50 kol)
			 kol
		       )
		)
	      )
	    )
	    (if	(and (wcmatch r1 "*ST60*") (= fl 0))
	      (progn
		(setq fl (+ fl 1))
		(setq kol
		       (subst
			 (cons 'ST60
			       (+ (/ (* n L B 0.0001 incslab) 14.40 ) (cdr (assoc 'ST60 kol)))
			 )
			 (assoc 'ST60 kol)
			 kol
		       )
		)
	      )
	    )


	    
	    (if (/= fl 1) (alert "error occured!!!"))
	  )
	)
	(setq ssi (+ ssi 1))
      )
      (print kol)
      ;;;11111
      (if (= Fround 1)
	(progn
	  (setq i 0)
	  (while (nth i kol)
	    (if	(/= (cdr (nth i kol)) 0)
	      (progn
		(setq newval (+	(- (cdr (nth i kol))
				   (rem (cdr (nth i kol)) 0.5)
				)
				0.5
			     )
		)
	      
	      (setq kol
		     (subst
		       (cons (car (nth i kol))
			     newval
		       )
		       (nth i kol)
		       kol
		     )
	      )
	    )
	      )
	    (setq i (+ i 1))
	  )
	)
      )
      ;;;11111

      
      ;;;drawing table
      (sp:tableupd kol)
      ;;;end of drawing table

    )
  )

  (princ)
)



;;; 666 end net unit 666 net unit
;;; 666 end net unit 666 net unit
;;; 666 end net unit 666 net unit

;;; 777  module for updating TS table
(defun sp:tableupd (kol)
  (setq table (ns:gettable))
  (setq catt (entnext table))
  (while (/= (cdr (assoc 0 (entget catt))) "SEQEND")
    (setq cdat (entget catt))

    (if	(= (cdr (assoc 2 cdat)) "ST10N")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'PAF10 kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )

    )

    (if	(= (cdr (assoc 2 cdat)) "ST20N")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST20 kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST25N")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST25 kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST30N")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST30 kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST35N")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST35 kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST50N")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST50 kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST60N")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST60 kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST15CN")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST15C kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST25CN")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST25C kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST25CSN")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST25CS kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST40CN")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST40C kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST50CN")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST50C kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "ST65CN")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'ST65C kol)) 2 1))
		     (assoc 1 cdat)
		     cdat
		   )
	)
	(entmod cdat)
	(entupd catt)
      )
    )

    (if	(= (cdr (assoc 2 cdat)) "PAFVN")
      (progn
	(setq cdat (subst
		     (cons '1 (rtos (cdr (assoc 'PAFV kol)) 2 1))
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
  (redraw)
  (VL-CMDF "updatefield" table "")
  (VL-CMDF "updatefield" table "")
  (VL-CMDF "updatefield" table "")
)
;;; 777 end module for updating TS table
;;;   ;;;   ;;;   ;;;   ;;;   
   ;;;   ;;;   ;;;   ;;;   ;;;
;;;   ;;;   ;;;   ;;;   ;;;   