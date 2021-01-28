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
      (vla-put-lineweight (vlax-ename->vla-object maxobj) acLnWt035)
      (print maxvalue)
      (princ)
    )
  )
)

;;;coloring and substracting values entered by keyboard
(defun c:colus ()
  (if (setq ss (colm:getss))
    (progn
      (setq msg "Enter main reinforcemetn area ")
      (setq limit (atof (getstring msg)))
      
      (setq ssi 0)
      
      (while (setq obj (ssname ss ssi))
	(setq objdata (entget obj))
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (/= (cdr (assoc '1 objdata)) "-")
		 (<= (abs (atof (cdr (assoc '1 objdata)))) limit)
	    )
	  (progn
	    (vla-put-TextString (vlax-ename->vla-object obj) "-")
	    (vla-put-color (vlax-ename->vla-object obj) 5)
	   ; (setq maxvalue (abs (atof (cdr (assoc '1 objdata)))))
	  )
	)
	(if (and (= (cdr (assoc '0 objdata)) "TEXT")
		 (> (abs (atof (cdr (assoc '1 objdata)))) limit)
	    )
	  (progn
	    (vla-put-TextString (vlax-ename->vla-object obj) (rtos (- (abs (atof (cdr (assoc '1 objdata)))) limit) 2 2) )
	    (vla-put-color (vlax-ename->vla-object obj) 5)
	   ; (setq maxvalue (abs (atof (cdr (assoc '1 objdata)))))
	  )
	)
	(setq ssi (+ ssi 1))
      )

      (setq inspoint (getpoint "enter place of anotation text"))
      (setq anot (vla-addtext
	*ModelSpace*
	(strcat "Оснвната мрежа е от " (rtos limit 2 2)  "sq.cm.")
	(vlax-3d-point inspoint)
	20
	))
      (vla-put-color  anot 5)
      (princ)
    )
  )
)