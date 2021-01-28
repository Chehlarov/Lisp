;;;written by Nikolay Chehlarov
;;;Sofia, feb 2008
;;;checks a model for imort in ETABS
(vl-load-com)
(setq mintol 0.0000000001)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun mc:draw (center)
  (vla-addCircle
    *ModelSpace*
    (vlax-3d-point center)
    1
  )
)

(defun mc:getss	(/ ss)
  (setq ss (ssget))
)

(defun c:mcheck	()
  (if (setq ss (mc:getss))
    (progn
      (setq tol (read (getstring "Enter tolerance ")))
      (setq ssi 0)
      (setq pdb  nil)
      (while (setq obj (ssname ss ssi))
	(setq pd (entget obj))
	(if (= (cdr (assoc '0 pd)) "LWPOLYLINE")
	  (progn
	    (setq cp 0)
	    (repeat (length pd)
	      (if (= (car (nth cp pd)) 10)
		(setq pdb (append  pdb (list  (cdr (nth cp pd)) )))
	      )
	      (setq cp (+ cp 1))
	    );;;repeat
	  )
	);;; za lwpolyline
	(if (= (cdr (assoc '0 pd)) "3DFACE")
	  (progn
	    (setq pdb (append  pdb (list  (cdr (assoc '10 pd)) )))
	    (setq pdb (append  pdb (list  (cdr (assoc '11 pd)) )))
	    (setq pdb (append  pdb (list  (cdr (assoc '12 pd)) )))
	    (setq pdb (append  pdb (list  (cdr (assoc '13 pd)) )))
	    )
	  );;; za 3dface
	
	(if (= (cdr (assoc '0 pd)) "LINE")
	  (progn
	    (setq pdb (append  pdb (list  (cdr (assoc '10 pd)) )))
	    (setq pdb (append  pdb (list  (cdr (assoc '11 pd)) )))
	    )
	  );;;za line
	(setq ssi (+ ssi 1))
      );;;while

(setq ssi 0)
      (repeat (length pdb)
	(setq ssj 0)
	(repeat (length pdb)
	  (setq hh (distance (nth ssi pdb) (nth ssj pdb)))
	  (if (and (> hh mintol) (< hh tol))
	    (progn
	      (print hh)
	      (mc:draw (nth ssj pdb))
	    )
	  )

	  
	  (setq ssj (+ ssj 1))
	  )
	(setq ssi (+ ssi 1))
	)
      
    )
  )
  (princ)
)