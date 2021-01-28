(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(defun c:ll ()
  (setq i 1)
  (while (setq point (getpoint))
    (setq text (vla-addtext
      *ModelSpace*
      (strcat "" (rtos i 2))
      (vlax-3d-point point)
      15
    ) )
    ;(vla-put-rotation text (/ pi 2))
    (setq i (+ i 1))
  )
)