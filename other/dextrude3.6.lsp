;;;written by Nikolay Chehlarov juli 2009
;;;lisp za extrudirane na lwpolilinii po daden vektor
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq *acad* (vlax-get-acad-object)
      *doc*  (vla-get-activedocument *acad*)
      *util* (vla-get-utility *doc*)
)
(setq *layers*
       (vla-get-layers
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

;;;vry6ta koordinatite na vyzlite na poliliniq
(defun dex:co (obj / i out obja coordi)
  (setq out nil)
  (setq obja (vlax-ename->vla-object obj))
  (setq coordi (vla-get-Coordinates obja))
  (setq coordi (vlax-safearray->list (vlax-variant-value coordi)))

  (setq i 0)
  (while (< i (length coordi))
					;(print (nth i coordi))
					;(princ (nth (+ i 1) coordi))
    (setq out (append out
		      (vlax-safearray->list
			(vlax-variant-value
			  (vla-TranslateCoordinates
			    *util*
			    (vlax-3d-point
			      (list
				(nth i coordi)
				(nth (+ i 1) coordi)
				(vla-get-elevation obja)
			      )
			    )
			    acOCS
			    acWorld
			    :vlax-FALSE
			    (vla-get-Normal obja)
			  )
			)
		      )
	      )
    )


    (setq i (+ i 2))

  )
  out
)

(defun dex:getss (/ ss)
  (setq ss (ssget))
)

;get a vector nad returns it's normalized value
(defun dex:norm	(vect / l)
  (setq	l (sqrt	(+ (* (car vect) (car vect))
		   (* (cadr vect) (cadr vect))
		   (* (caddr vect) (caddr vect))
		)
	  )
  )
  (list (/ (car vect) l) (/ (cadr vect) l) (/ (caddr vect) l))
)

(defun c:dex ()
  (setq ss (dex:getss))
  (setq obj (vla-add *layers* "0_SOLID_PANELS"))
  (vla-put-color obj 30)
  (vla-put-activelayer (vla-get-ActiveDocument (vlax-get-acad-object)) obj)
  
  (setq exva (getpoint "enter first point of vector "))
  (setq exvb (getpoint "enter second point of vector " exva))
  (setq	exv (dex:norm (list (- (car exvb) (car exva))
			    (- (cadr exvb) (cadr exva))
			    (- (caddr exvb) (caddr exva))
		      )
	    )
  )
  (setq l (distance exvb exva))
  (setq ssi 0)
  (setq ss2 (ssadd))
  (while (setq obj (ssname ss ssi))
    (setq objdata (entget obj))
    ;;debug
    ;(print objdata)
    (if	(and (= (cdr (assoc 0 objdata)) "LWPOLYLINE")
	     (= (cdr (assoc 8 objdata)) "0_PLANES")
	     )
      (progn
	(setq lwv (dex:norm (cdr (assoc 210 objdata))))
	(setq cosa
	       (+
		 (* (car exv) (car lwv))
		 (* (cadr exv) (cadr lwv))
		 (* (caddr exv) (caddr lwv))
	       )
	)
	(if (< cosa 0)
;;;trqbva da se obyrne posokata mu
	  (setq obj (dex:invert obj))
	)
	
	(setq objc (vla-copy (vlax-ename->vla-object obj)))
	;(vla-put-layer (vlax-ename->vla-object objc) "0_SOLID_PANELS")
	(vla-put-layer (vlax-ename->vla-object obj) "0_SOLID_PANELS")
	(ssadd obj ss2)
      )
    )			
    (setq ssi (+ ssi 1))
  )
  (dex:showe ss2)
  ;(VL-CMDF "_ai_molc" "0_SOLID_PANELS")
  (VL-CMDF "extrude" ss2  "" l "0")
)



;;;programka za otkrivane centyr
;;;priblizitelno!!!
;;;raboti samo ako e UCS!!!!
(defun dex:center (obj)
  (setq obja (vlax-ename->vla-object obj))
  (setq cop (vlax-make-safearray vlax-vbObject '(0 . 0)))
  (vlax-safearray-put-element cop 0 obja)
  (setq objreg (vla-addregion *ModelSpace* cop))
  (setq objreg (vlax-variant-value objreg))
  (setq objreg (vlax-safearray-get-element objreg 0))
  (vla-put-color objreg acred)
  (setq centroid (vla-get-Centroid objreg))
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

					; (vla-delete objreg)


)

;;;programka da slaga vektora na extrudvane
(defun dex:showe ( ss / objdata ssi co p1 p2 objl p1x p1y p1z)
  ;(setq ss (dex:getss))
    ;;;create new layer
    (setq obj (vla-add *layers* "0_NORMALS"))
  (vla-put-color obj 1)
  (setq ssi 0)
  (while (setq obj (ssname ss ssi))
    (setq objdata (entget obj))
    (if	(= (cdr (assoc 0 objdata)) "LWPOLYLINE")
      (progn
	(setq co (dex:co obj))
	(setq i 0)
	(setq p1x 0)
	(setq p1y 0)
	(setq p1z 0)
	(while (< i (length co))
	  (setq p1x (+ p1x (nth i co)))
	  (setq p1y (+ p1y (nth (+ i 1) co)))
	  (setq p1z (+ p1z (nth (+ i 2) co)))
	  (setq i (+ i 3))
	  )
	(setq p1 (list
		   (* (/ p1x (length co)) 3)
		   (* (/ p1y (length co)) 3)
		   (* (/ p1z (length co)) 3)
		 )
	)
	(setq
	  p2 (list
	       (+ (car p1) (* (cadr (assoc 210 objdata)) 1000))
	       (+ (cadr p1) (* (caddr (assoc 210 objdata)) 1000))
	       (+ (caddr p1) (* (cadddr (assoc 210 objdata)) 1000))
	     )
	)

	(setq objl(vla-addline
	  *ModelSpace*
	  (vlax-3d-point p1)
	  (vlax-3d-point p2)
	)
	      )
	(vla-put-layer objl "0_NORMALS")
	(vla-put-lineweight objl acLnWt100 )
      )
    )

    (setq ssi (+ ssi 1))
  )
)

(defun c:she()
  (setq ss (dex:getss))
  (dex:showe ss)
  )




;;;mirrors object 3d in its plane
(defun dex:invert	(obj / objdatqa co p1 p2 p3 out)
  ;(setq obj (car (entsel)))
  (setq objdata (entget obj))
  (setq co (dex:co obj))
  (setq p1 (list (nth 0 co) (nth 1 co) (nth 2 co)))
  (setq p2 (list (nth 3 co) (nth 4 co) (nth 5 co)))
  (setq p3 (list (nth 6 co) (nth 7 co) (nth 8 co)))
  (setq out (vla-mirror3d
    (vlax-ename->vla-object obj)
    (vlax-3d-point p1)
    (vlax-3d-point p2)
    (vlax-3d-point p3)
  )
	)
  (entdel obj)
  (vlax-vla-object->ename out)
)

(defun c:inv ()
  (setq ss (dex:getss))
  (setq ssi 0)
  (while (setq obj (ssname ss ssi))
    (setq objdata (entget obj))
    (if	(and (= (cdr (assoc 0 objdata)) "INSERT")
	     (= (cdr (assoc 2 objdata)) "PW")
	)
      (progn
	(setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes (vlax-ename->vla-object obj)))
		       )
	    )
	
	
      (setq objm (dex:invert (handent (vla-get-textstring (nth 1 atttable))) ))
      (setq name (vla-get-textstring (nth 0 atttable)))

	;;;mirroring the labelitself

	(setq objb (vla-insertblock
			 *ModelSpace*
			 (vlax-3d-point (plane:center objm 1))
			 "PW"
			 1
			 1
			 1
			 0
		       )
	    )
	    (setq
	      atttable (vlax-safearray->list
			 (vlax-variant-value (vla-getattributes  objb))
		       )

	    )
	    (vla-put-textstring (nth 0 atttable) name)
	    (vla-put-textstring (nth 1 atttable) (cdr (assoc 5 (entget objm))))
	    (vla-put-layer objb "0_NUMBERS")
	(vla-delete (vlax-ename->vla-object obj))
	;;;end mirroring label itself
      
      )
    )
    (setq ssi (+ ssi 1))
  )
)

;;;;;;;;;;;;;;
;
;;;;;;;;;;;;;;
(defun c:eex ()
  (vl-cmdf "ucs" "w")
  (princ "Select polylines: ")
  (setq ss (dex:getss))
  (setq obj (vla-add *layers* "0_SOLID_PANELS"))
  (vla-put-color obj 30)
  (vla-put-activelayer (vla-get-ActiveDocument (vlax-get-acad-object)) obj)
  
  (setq l (getreal "Enter depth of PW: "))
  (setq ssi 0)
  (setq ss2 (ssadd))
  (while (setq obj (ssname ss ssi))
    (setq objdata (entget obj))
    
     (if(and (= (cdr (assoc 0 objdata)) "LWPOLYLINE")
	     (= (cdr (assoc 8 objdata)) "0_PLANES")
	     )
      (progn
	(setq objc (vla-copy (vlax-ename->vla-object obj)))
	(vla-put-layer objc "0_SOLID_PANELS")
	(ssadd (vlax-vla-object->ename objc) ss2)
      )
    )			
    (setq ssi (+ ssi 1))
  )
  
  (VL-CMDF "extrude" ss2  "" l "0")
)