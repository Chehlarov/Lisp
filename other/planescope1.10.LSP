;;;written by Nikolay Chehlarov juli 2009
;;;lisp za namirane na centyr na poliliniq
(vl-load-com)
(arxload (findfile "geom3d.arx"))
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(setq *UCS*
       (vla-get-UserCoordinateSystems
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq *acad* (vlax-get-acad-object)
      *doc*  (vla-get-activedocument *acad*)
      *util* (vla-get-utility *doc*)
)
(setq tol 5)
;;proverka na geometriqta
(defun c:geomcheck ()
  (if (setq ss (alp:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "LWPOLYLINE")
		 (or
		 (= (cdr (assoc 8 objdata)) "0_PLANES")
		 (= (cdr (assoc 8 objdata)) "0_PANELS")
		 )
	    )
	  (progn
	    (setq obja (vlax-ename->vla-object obj))
	    (setq coordi (vla-get-Coordinates obja))
	    (setq coordi
		   (vlax-safearray->list (vlax-variant-value coordi))
	    )

	    
	    (setq newcoo (list (car coordi) (cadr coordi)))
	    
	    (setq i 2)
	    (while (< i (length coordi))
	      (if (and
		(> (distance (list (nth i coordi) (nth (+ i 1) coordi)) 
			     (list (nth (- (length newcoo) 2) newcoo) (nth (- (length newcoo) 1) newcoo))
		   )
		   tol
		)
		(> (distance (list (nth i coordi) (nth (+ i 1) coordi)) 
			     (list (car  newcoo) (cadr  newcoo))
		   )
		   tol
		)
		
		)
		(setq newcoo (append newcoo (list (nth i coordi) (nth (+ i 1) coordi))))

	      )
	      (setq i (+ i 2))
	      )
	    (setq pointcc (vlax-make-safearray vlax-vbDouble (cons 0  (- (length newcoo)1))))
	    (vlax-safearray-fill
		pointcc
		newcoo
	      )
		  

	    (vla-put-coordinates obja pointcc)
	    (vla-put-closed obja actrue)
      ;;;;checking wether the first 3 point lie on one axis and warning if so
	    (if (= (/ (+
	      (*
	      (- (nth 0 newcoo) (nth 2 newcoo))
	      (- (nth 4 newcoo) (nth 2 newcoo))
	      )
	    (*
	      (- (nth 1 newcoo) (nth 3 newcoo))
	      (- (nth 5 newcoo) (nth 3 newcoo))
	      )
	      )
		      (* (distance (list (nth 0 newcoo) (nth 1 newcoo) ) (list (nth 2 newcoo) (nth 3 newcoo) ) )
			 (distance (list (nth 2 newcoo) (nth 3 newcoo) ) (list (nth 4 newcoo) (nth 5 newcoo) ) )
			 )
		      )
	      -1
		   )
	      (progn
	      (alert "First 3 points lie on one line! Look for red object")
	      (vla-put-color obja acred)
	      )
	      )
	      
      ;;;;end checking wether the first 3 point lie on one axis and warning if so
	    
	    );;prgon 
	  );;;if
	  
	(setq ssi (+ ssi 1))
	)

      
      )
    )
  );;;end defun
;;;funkcia koqto vra6ta centyra na tegestta na polinia v WCS i smenq koordiantnata sistema da legi v ravninata na obekta
;;;ako flinv 1 obry6ta normalata na polyliniqta
(defun ps:center (obj flinv)
  (setq objdata (entget obj))
  (setq obja (vlax-ename->vla-object obj))

  (setq cop (vlax-make-safearray vlax-vbObject '(0 . 0)))
  (vlax-safearray-put-element cop 0 obja)
  (setq objreg (vla-addregion *ModelSpace* cop))
  (setq objreg (vlax-variant-value objreg))
  (setq objreg (vlax-safearray-get-element objreg 0))
					
  (setq coordi (dex:co obj))
  (setq n (alp:norm (cdr (assoc 210 objdata))))
  (setq	ox (alp:norm (list
		       (- (nth 3 coordi) (nth 0 coordi))
		       (- (nth 4 coordi) (nth 1 coordi))
		       (- (nth 5 coordi) (nth 2 coordi))
		     )
	   )
  )
;;;cross product
  (setq	ny (alp:norm
	     (list
	       (- (* (cadr n) (caddr ox)) (* (caddr n) (cadr ox)))
	       (- (* (caddr n) (car ox)) (* (car n) (caddr ox)))
	       (- (* (car n) (cadr ox)) (* (cadr n) (car ox)))
	     )
	   )
  )
  ;;;obrytane na ny
  (if (= flinv 1)
  (setq ny (list
	     (- (car ny))
	     (- (cadr ny))
	     (- (caddr ny))
	     )
	)
    )

  (setq	obucs (vla-add
		*UCS*
		(vlax-3d-point
		  (list (nth 0 coordi) (nth 1 coordi) (nth 2 coordi))
		)
		(vlax-3d-point
		  (list	(+ (nth 0 coordi) (* (car ox) 100))
			(+ (nth 1 coordi) (* (cadr ox) 100))
			(+ (nth 2 coordi) (* (caddr ox) 100))
		  )
		)
		(vlax-3d-point
		  (list	(+ (nth 0 coordi) (* (car ny) 100))
			(+ (nth 1 coordi) (* (cadr ny) 100))
			(+ (nth 2 coordi) (* (caddr ny) 100))
		  )
		)
		"myucs"
	      )
  )
  (vla-put-activeUCS
    (vla-get-ActiveDocument (vlax-get-acad-object))
    obucs
  )
  (setq centroid (vla-get-Centroid objreg))
  (vla-delete objreg)
  (setq	centroidl
	 (list
	   (car	(vlax-safearray->list (vlax-variant-value centroid))
	   )
	   (cadr
	     (vlax-safearray->list (vlax-variant-value centroid))
	   )
	   0
	 )
  )

  (setq cg (trans centroidl 1 0))

  cg
);;defun ps:center



;;;main function assigning block references
(defun c:pasign	()
  (if (setq ss (alp:getss))
    (progn
      (setq ssi 0)
      (setq pwindex 1)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "LWPOLYLINE")
		 (= (cdr (assoc 8 objdata)) "0_PLANES")
	    )
	  (progn
;;;;;;;;;;;;11111111
	    (setq objb (vla-insertblock
			 *ModelSpace*
			 (vlax-3d-point (ps:center obj 1))
			 "PW"
			 1
			 1
			 1
			 0
		       )
	    )
	    (setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes objb))
		       )

	    )
	    (vla-put-textstring (nth 1 atttable) (cdr (assoc 5 objdata)))
	    (vla-put-textstring (nth 0 atttable) (strcat "p" (rtos pwindex)))
	    (vla-put-layer objb "0_NUMBERS")
	    (setq pwindex (+ pwindex 1))
;;;;;;;;;;;;11111111
	  )
	)
	(setq ssi (+ ssi 1))
      )
      ;;while
    )
  )
  ;;if getss
)
;;;function returning bottom left and bottoma right point of object
(defun ps:obmm(obj / i minx miny maxx coor cp )
  (setq coor (dex:co (vlax-vla-object->ename obj)))
  
  (setq i 0)
  (setq minx nil)
  (setq miny nil)
  (setq maxx nil)
  (while (nth i coor)
  (setq cp (list (nth i coor) (nth (+ i 1) coor) (nth (+ i 2) coor)))
      
      (if (or (= minx nil) (> (car minx) (car cp)))
	(setq minx  cp)
	)
      (if (or (= miny nil) (> (cadr miny) (cadr cp)))
	(setq miny  cp)
	)
      (if (or (= maxx nil) (< (car maxx) (car cp)))
	(setq maxx cp)
	)
      
      
  (setq i (+ i 3))
    )
  
  ;(print  (list (list (car minx) (cadr miny) ) (list (car maxx) (cadr miny) )))
  
  (list (list (car minx) (cadr miny) ) (list (car maxx) (cadr miny) ))
  )
;;;function returning bottom left and bottoma right point of object
;;;funkciq align
(defun ps:align(objt from to / ss2)
  (vl-cmdf "ucs" "w")
 ; (setvar "cmdecho" 0)
  (setq coofrom (dex:co from))
  (setq cooto (dex:co to))
  (setq ss2 (ssadd))
 (ssadd objt ss2)
  ;;;(setq pt1 (list (nth 0 coofrom) (nth 1 coofrom) (nth 2 coofrom)))
  ;;;(setq pt2 (list (nth 0 cooto) (nth 1 cooto) (nth 2 cooto)))
;;;  (vl-cmdf "line"
;;;	 (list (nth 0 coofrom) (nth 1 coofrom) (nth 2 coofrom))
;;;	 (list (nth 0 cooto) (nth 1 cooto) (nth 2 cooto))
;;;	   ""
;;;	   )
 ; (vla-put-color (vlax-ename->vla-object objt) acred)
  
  (align
   ss2
    
	 (list (nth 0 coofrom) (nth 1 coofrom) (nth 2 coofrom))
	 (list (nth 0 cooto) (nth 1 cooto) (nth 2 cooto))

	 (list (nth 3 coofrom) (nth 4 coofrom) (nth 5 coofrom))
	 (list (nth 3 cooto) (nth 4 cooto) (nth 5 cooto))

	 (list (nth 6 coofrom) (nth 7 coofrom) (nth 8 coofrom))
	 (list (nth 6 cooto) (nth 7 cooto) (nth 8 cooto))
	   
	   )
  
  (princ)
  )
;;;end funkciq align
;;;funkciq za sklopqvane
(defun c:pscope()
  (if (setq ss (alp:getss))
    (progn
      (setq ip (getpoint "Insertion point: "))

      ;;;sorting selection set
      (setq harray nil)
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "PW")
	    )
	  (progn
	    (setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes (vlax-ename->vla-object obj)))
		       )
	    )
	    
	    (setq num (atoi (vl-string-left-trim "-" (substr (vla-get-textstring (nth 0 atttable))  2))))
	    (setq harray (append harray (list (cons num obj))))
	    
	    )
	  )
	(setq ssi (+ ssi 1))
	)
      
(setq harray (alp:dottedpairsort harray))
      
      ;;;end sorting selection set
      (setq i 0)
      (while (setq obj (cdr (nth i harray)))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "INSERT")
		 (= (cdr (assoc '2 objdata)) "PW")
	    )
	  (progn
	    ;;;22222
	    (setq obja (vlax-ename->vla-object obj))
	    ;(setq objtextc  (vla-copy obja))
	    (setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes obja))
		       )
	    )
	    (setq objb (vlax-ename->vla-object(handent (vla-get-textstring (nth 1 atttable)))))
	    
	    (setq objchild  (vla-copy objb))
	    ;(vla-put-color objchild acmagenta)
	    ;(align objchild)
	    (vla-put-normal objchild (vlax-3d-point (list 0 0 -1)))
	    (vla-put-elevation objchild 0)
	    (vla-put-textstring (nth 2 atttable) (cdr (assoc 5 (entget (vlax-vla-object->ename objchild)))))
	    (vla-move objchild
		      (vlax-3d-point (car (ps:obmm objchild)))
		      (vlax-3d-point ip)
		      )
	    (setq ip (cadr (ps:obmm objchild)))
	    ;;;-------
	    (setq objtextc  (vla-copy obja))
	    (ps:align (vlax-vla-object->ename objtextc)
		      (vlax-vla-object->ename objb)
		      (vlax-vla-object->ename objchild)
		      )
	  
	    ;;;22222
	    )
	)
	(setq i (+ i 1))
      )
      ;;end while
    )
  )
)
;;;end funkciq za sklopqvane
;;;   ;;;   ;;;   ;;;   
   ;;;   ;;;   ;;;   ;;;
;;;   ;;;   ;;;   ;;;   
   ;;;   ;;;   ;;;   ;;;
;;;funkciq koqto slaga blokovete na otdelnite par4eta avtomati4no
(defun c:pasign2	()
  (if (setq ss (alp:getss))
    (progn

      ;;;namirane na "PW"bloka trqbva da e selektiran samo edin
      (setq ssi 0)
      (setq name nil
	    parent nil
	    child nil)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc 0 objdata)) "INSERT")
		 (= (cdr (assoc 2 objdata)) "PW")
	    )
	  (progn
	    (if name (alert "More than 1 PW block selected"))
	    (setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes (vlax-ename->vla-object obj)))
		       )
	    )
	    (setq name (vla-get-textstring (nth 0 atttable)))
	    (setq parent (vla-get-textstring (nth 1 atttable)))
	    (setq child (vla-get-textstring (nth 2 atttable)))
	    
	  )
	)
	(setq ssi (+ ssi 1))
      )
      (if (or (not name) (not parent) (not child) ) (alert "Problem finding main index!"))

            ;;;namirane na "PW"bloka trqbva da e selektiran samo edin
      
      (setq ssi 0)
      (setq pwsubindex 1)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "LWPOLYLINE")
		 (= (cdr (assoc 8 objdata)) "0_PANELS")
	    )
	  (progn
;;;;;;;;;;;;11111111
	    (setq objb (vla-insertblock
			 *ModelSpace*
			 (vlax-3d-point (ps:center obj 0))
			 "PWpart"
			 1
			 1
			 1
			 0
		       )
	    )
	    (setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes objb))
		       )

	    )
	    (vla-put-textstring (nth 0 atttable) (strcat name "-"(rtos pwsubindex)))
	    (vla-put-textstring (nth 1 atttable) parent)
	    (vla-put-textstring (nth 2 atttable) child)
	    (vla-put-textstring (nth 3 atttable) (cdr (assoc 5 objdata)))
	    
	    (vla-put-layer objb "0_NUMBERS")
	    (setq pwsubindex (+ pwsubindex 1))
;;;;;;;;;;;;11111111
	  )
	)
	(setq ssi (+ ssi 1))
      )
      ;;while
    )
  )
  ;;if getss
)
;;;funkciq koqto slaga blokovete na otdelnite par4eta avtomati4no
;;;   ;;;   ;;;   ;;;   
   ;;;   ;;;   ;;;   ;;;
;;;   ;;;   ;;;   ;;;   
   ;;;   ;;;   ;;;   ;;;
;;;function putting dimensions on a piece of PW
(defun c:pdim ()
 ; (setq obj (car (entsel "Select object to dimension")))

    (if (setq ss (alp:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc 0 objdata)) "LWPOLYLINE")
		 (= (cdr (assoc 8 objdata)) "0_PANELS")
	    )
	  (progn
      ;;;namirane na "PW"bloka trqbva da e selektiran samo edin
      
      
  (setq obja (vlax-ename->vla-object obj))
  (setq coordi (vla-get-Coordinates obja))
  (setq	coordi
	 (vlax-safearray->list (vlax-variant-value coordi))
  )
  
  (setq objlistt (vl-catch-all-apply 'vla-offset (list obja 1)))
  
  (if (vl-catch-all-error-p objlistt) (alert "problem in offseting"))
  (setq objlistt (car (vlax-safearray->list (vlax-variant-value objlistt))))

  (if (> (vla-get-length obja) (vla-get-length objlistt))
    (setq objlist (vl-catch-all-apply 'vla-offset (list obja -250)))
    ;;;else
    (setq objlist (vl-catch-all-apply 'vla-offset (list obja 250)))
    )
  (vla-delete objlistt)
  
  (if (vl-catch-all-error-p objlist) (alert "problem in offseting"))
  (setq objlist (car (vlax-safearray->list (vlax-variant-value objlist))))
  (setq coordi2 (vla-get-Coordinates objlist))
  (setq	coordi2
	 (vlax-safearray->list (vlax-variant-value coordi2))
  )
  (vla-delete objlist)
  

   (setq p1 (list (nth (- (length coordi) 2) coordi) (nth (- (length coordi) 1) coordi)))
	    
	    (setq i 0)
	    (while (< i (length coordi))
	      (setq p2 (list (nth i coordi) (nth (+ i 1) coordi)))
	      (setq p3 (list (nth i coordi2) (nth (+ i 1) coordi2)))
	      (if (not (car p3)) (alert "Problem PW not closed! Equal firsta and last point"))
	      ;;111---
	      (setq alpha (atan  (- (cadr p1) (cadr p2))
				     (- (car p1) (car p2))
			    )
		)
	      
	      (setq obj	(vla-AddDimRotated
			  *ModelSpace*
			  (vlax-3d-point p1)
			  (vlax-3d-point p2)
			  (vlax-3d-point p3)
			  alpha
			)
	      )
	      (vla-put-layer obj "0_DIMENSIONS")
	      (vla-put-stylename obj "ROMANS")
	      ;;111---
	      (setq p1 p2)
	      (setq i (+ i 2))
	    )
  ;;;check if it is 4 edge PW to put diagonals
  (if (= (length coordi) 8)
    (progn
      (setq p1 (list (nth 0 coordi) (nth 1 coordi)))
      (setq p2 (list (nth 4 coordi) (nth 5 coordi)))
       ;;111---
	      (setq alpha (atan  (- (cadr p1) (cadr p2))
				     (- (car p1) (car p2))
			    )
		)
	      
	      (setq obj	(vla-AddDimRotated
			  *ModelSpace*
			  (vlax-3d-point p1)
			  (vlax-3d-point p2)
			  (vlax-3d-point p2)
			  alpha
			)
	      )
	      (vla-put-layer obj "0_DIMENSIONS")
	      (vla-put-stylename obj "ROMANS")
	      ;;111---
      (setq p1 (list (nth 2 coordi) (nth 3 coordi)))
      (setq p2 (list (nth 6 coordi) (nth 7 coordi)))
       ;;111---
	      (setq alpha (atan  (- (cadr p1) (cadr p2))
				     (- (car p1) (car p2))
			    )
		)
	      
	      (setq obj	(vla-AddDimRotated
			  *ModelSpace*
			  (vlax-3d-point p1)
			  (vlax-3d-point p2)
			  (vlax-3d-point p2)
			  alpha
			)
	      )
	      (vla-put-layer obj "0_DIMENSIONS")
	      (vla-put-stylename obj "ROMANS")
	      ;;111---
      
      )
    )
  ;;;end check if it is 4 edge PW to put diagonals
  );progn
	  );;;if
	(setq ssi (+ ssi 1))
	);;;while
      );;progn
      );;;if
)
;;;function putting dimensions on a piece of PW
;;;   ;;;   ;;;   ;;;   
   ;;;   ;;;   ;;;   ;;;
;;;   ;;;   ;;;   ;;;   
   ;;;   ;;;   ;;;   ;;;
;;;function returniing PWpices to 3D
(defun c:backto3d ()
  (if (setq ss (alp:getss))
    (progn

      ;;;namirane na "PW"bloka trqbva da e selektiran samo edin
      (setq ssi 0)
      (setq name nil
	    parent nil
	    child nil)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc 0 objdata)) "INSERT")
		 (= (cdr (assoc 2 objdata)) "PWpart")
	    )
	  (progn
	    (setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes (vlax-ename->vla-object obj)))
		       )
	    )
	    
	    (setq parent (vla-get-textstring (nth 1 atttable)))
	    (setq child (vla-get-textstring (nth 2 atttable)))
	    (setq me (vla-get-textstring (nth 3 atttable)))

	    (setq me2a  (vla-copy (vlax-ename->vla-object (handent me) )))
	    (ps:align (vlax-vla-object->ename me2a) (handent child) (handent parent))

	    (setq myblock2a  (vla-copy (vlax-ename->vla-object obj)))
	    (ps:align (vlax-vla-object->ename myblock2a) (handent child) (handent parent))
	  )
	)
	(setq ssi (+ ssi 1))
      )
      
      );;;progn
    )
  );;; end defun
;;;function returniing PWpices to 3D