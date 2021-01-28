(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(defun c:nsheet ()
  (setq i (getint "Start from: "))
  (while (setq point (getpoint))
    (setq text (vla-addtext
      *ModelSpace*
      (strcat "FEUILLE N° " (rtos i 2))
      (vlax-3d-point (list
		       (+ (car point) 14) 
		       (+ (cadr point) 14)
		       )
	)
      6.5
    ) )
    (vla-put-stylename text "schema")
    (vla-put-layer text "RAMKA")
        
    (setq i (+ i 1))
  )
)