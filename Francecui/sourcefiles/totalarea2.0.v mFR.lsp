(defun tot:getss (/ ss)
  (setq ss (ssget))
)
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(defun c:alllbycol ()
  (if (setq ss (tot:getss))
    (progn
      (setq col (cdr (assoc '62 (entget (car (entsel))))))
      (setq ssi 0)
      (setq len 0)
      (while (setq obj (ssname ss ssi))
	;(setq objdata (entget obj))
	(if (= col (cdr (assoc '62 (entget obj))))
	  (progn
	    (setq obja (vlax-ename->vla-object obj))
	    (setq len (+ len (vla-get-length obja)))
	  )
	)


	(setq ssi (+ ssi 1))

	)
      )
    )
  
  (vla-addtext
    *ModelSpace*
    (rtos len 2 2)
    (vlax-3d-point (getpoint "insert text: "))
    0.20
    )
  (print len)
  (print)
  )