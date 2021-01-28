(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq *layers*
       (vla-get-layers
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq txth 20)
(setq scale 100)

(defun treat0 (N x y M11 M22 M12)
  (setq i 0)
  (setq	avgM11 0
	avgM22 0
	avgM12 0
  )
  (while (nth i M11)
    (setq avgM11 (+ avgM11 (nth i M11)))
    (setq avgM22 (+ avgM22 (nth i M22)))
    (setq avgM12 (+ avgM12 (nth i M12)))
    (setq i (+ i 1))
  )
  (setq avgM11 (/ avgM11 i))
  (setq avgM22 (/ avgM22 i))
  (setq avgM12 (/ avgM12 i))
  (vla-addtext
    *ModelSpace*
    (rtos avgM11 2 2)
    (vlax-3d-point (list x y))
    txth
  )
  (vla-addtext
    *ModelSpace*
    (rtos avgM22 2 2)
    (vlax-3d-point (list (+ x 4200) y))
    txth
  )
  (vla-addtext
    *ModelSpace*
    (rtos avgM12 2 2)
    (vlax-3d-point (list (+ x 8400) y))
    txth
  )
)

(defun treat (N x y M11 M22 M12)
  (setq i 0)
  (setq	avgM11 0
	avgM22 0
	avgM12 0
  )
  (while (nth i M11)
    (setq avgM11 (+ avgM11 (nth i M11)))
    (setq avgM22 (+ avgM22 (nth i M22)))
    (setq avgM12 (+ avgM12 (nth i M12)))
    (setq i (+ i 1))
  )
  (setq avgM11 (/ avgM11 i))
  (setq avgM22 (/ avgM22 i))
  (setq avgM12 (/ avgM12 i))

  (setq	str (strcat (rtos N 2)
		    "\t"
		    (rtos x 2)
		    "\t"
		    (rtos y 2)
		    "\t"
		    (rtos avgM11 2 2)
		    "\t"
		    (rtos avgM22 2 2)
		    "\t"
		    (rtos avgM12 2 2)
		    "\n"
	    )
  )
  (princ str tofil)


)


(defun c:pp ()
  (setvar "cmdecho" 0)
  (if (setq fil
	     (open
	       "D:/Nikolay3/vlsip/moment_loader_ETABS/input.txt"
	       "r"
	     )
      )
    (progn
;;;syzdavana na sloeve
      (setq obj (vla-add *layers* "DX"))
      (vla-put-color obj acred)
      (setq obj (vla-add *layers* "DY"))
      (vla-put-color obj acblue)
      (setq obj (vla-add *layers* "GX"))
      (vla-put-color obj acgreen)
      (setq obj (vla-add *layers* "GY"))
      (vla-put-color obj acmagenta)
;;;--- Read the first blank line in the x position (A,1)
      (setq lin (read-line fil))
      (setq cntr 1)
      (setq lin (read-line fil))
;;;--- Loop while there is another x coordinate      
      (while (and lin (< cntr 10000) (/= lin ""))

	(setq x (read lin))
	(setq lin (vl-string-left-trim (rtos x 2) lin))
	(setq lin (vl-string-left-trim "\t" lin))

	(setq y (read lin))
	(setq lin (vl-string-left-trim (rtos y 2) lin))
	(setq lin (vl-string-left-trim "\t" lin))
	;;;;;;;
	(if (/= lin "")
	  (progn
	    (setq M11 (read lin))
	    (setq lin (vl-string-left-trim (rtos M11 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq M22 (read lin))
	    (setq lin (vl-string-left-trim (rtos M22 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))
	    (if	(>= M11 0)
	      (progn
		(setq obj (vla-addtext
		  *ModelSpace*
		  (rtos M11 2 2)
		  (vlax-3d-point (list  (* x scale)  (* y scale)))
		  txth
		))
		(vla-put-layer obj "DX")
	      )
	      (progn
		(setq obj (vla-addtext
		  *ModelSpace*
		  (rtos M11 2 2)
		  (vlax-3d-point (list  (* x scale)  (* y scale)))
		  txth
		))
		(vla-put-layer obj "GX")
	      )
	    )

	    (if	(>= M22 0)
	      (progn
		(setq obj (vla-addtext
		  *ModelSpace*
		  (rtos M22 2 2)
		  (vlax-3d-point (list  (* x scale)  (* y scale)))
		  txth
		))
		(vla-put-layer obj "DY")
	      )
	      (progn
		(setq obj (vla-addtext
		  *ModelSpace*
		  (rtos M22 2 2)
		  (vlax-3d-point (list  (* x scale)  (* y scale)))
		  txth
		))
		(vla-put-layer obj "GY")
	      )
	    )

	  )
	)
	(setq cntr (+ cntr 1))
	(setq lin (read-line fil))
      )

      (close fil)
    )
    (alert "Error - File not opened")
  )
  (alert (strcat (itoa cntr) " points created"))
  (setvar "cmdecho" 1)
  (princ)
)


(defun col:getss (/ ss)
  (setq ss (ssget))
)

(defun c:col ()
  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (> (atof (cdr (assoc '1 objdata))) 400)
					;	; ;(>= (atof (cdr (assoc '1 objdata))) -150)
	    )
	  (progn
	    (setq objdata (subst
			    (cons '62 3)
			    (assoc 62 objdata)
			    objdata
			  )
	    )
	    (entmod objdata)
	    (entupd obj)
	  )
	)
	(setq ssi (+ ssi 1))
      )
    )
  )
)