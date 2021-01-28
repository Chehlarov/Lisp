(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(defun c:ll ()
  (setq i 3)
  (while (setq point (getpoint))
    (setq text (vla-addtext
      *ModelSpace*
      (strcat "FEUILLE N°   " (rtos i 2))
      (vlax-3d-point (list
		       (+ (car point) 21) 
		       (+ (cadr point) 18)
		       )
	)
      8.2
    ) )
    ;(vla-put-rotation text (/ pi 2))
    (setq i (+ i 1))
  )
)