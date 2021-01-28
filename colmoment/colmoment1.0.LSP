;;;coloring text with the highest (absolute) value
;;;wrtitten by nikolay Chehlarov July 2008
;;;ordered by Gerry
;;;use function colm
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun colm:getss (/ ss)
  (setq ss (ssget))
)

(defun c:cm ()
  (if (setq ss (colm:getss))
    (progn
      (setq ssi 0)
      (setq maxvalue 0)
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (> (abs (atof (cdr (assoc '1 objdata)))) maxvalue)
	    )
	  (progn
	    (setq maxvalue (abs (atof (cdr (assoc '1 objdata)))))
	    (setq maxobj obj)

	  )
	)
	(setq ssi (+ ssi 1))
      )
      (vla-put-color (vlax-ename->vla-object maxobj) 2)
      (print maxvalue)
      (princ)
    )
  )
)