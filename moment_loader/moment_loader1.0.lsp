(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(setq txth 0.1)

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

  (setq str (strcat (rtos N 2) "\t" (rtos x 2) "\t" (rtos y 2) "\t" (rtos avgM11 2 2) "\t" (rtos avgM22 2 2) "\t" (rtos avgM12 2 2) "\n"))
  (princ str tofil)
)


(defun c:loadm ()

;;;--- Turn the command echo off
  (setvar "cmdecho" 0)


;;;--- See if the text file is available, if so, open it to read
  (if (setq fil (open "E:/Makrosi_Autocad/lis/NLIST.lis" "r" ))
    (progn


;;;--- Ignore the first line by reading in the data and
;;;    doing nothing with it.  Why?  The first line in a
;;;    spreadsheet is usually a label.  This is included
;;;    in the ReadMe.txt file.

;;;--- Read the first blank line in the x position (A,1)
      (setq lin (read-line fil))

;;;--- Set up a counter to count the points
      (setq cntr 1)

      (setq lin (read-line fil))
      (setq N (read lin))
      (setq lin (vl-string-left-trim (rtos N 2) lin))
      (setq lin (vl-string-left-trim "\t" lin))

      (setq x (read lin))
      (setq lin (vl-string-left-trim (rtos x 2) lin))
      (setq lin (vl-string-left-trim "\t" lin))

      (setq y (read lin))
      (setq lin (vl-s\tring-left-trim (rtos y 2) lin))
      (setq lin (vl-string-left-trim "\t" lin))

      (setq M11 (list (read lin)))
      (setq lin (vl-string-left-trim (rtos (car M11) 2) lin))
      (setq lin (vl-string-left-trim "\t" lin))

      (setq M22 (list (read lin)))
      (setq lin (vl-string-left-trim (rtos (car M22) 2) lin))
      (setq lin (vl-string-left-trim "\t" lin))

      (setq M12 (list (read lin)))
      (setq lin (vl-string-left-trim (rtos (car M12) 2) lin))
      (setq lin (vl-string-left-trim "\t" lin))

      (setq lin (read-line fil))
;;;--- Loop while there is another x coordinate      
      (while (and lin (< cntr 4000) (/= lin ""))

	(if (= N (read lin))
	  (progn
	    (setq N (read lin))
	    (setq lin (vl-string-left-trim (rtos N 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq x (read lin))
	    (setq lin (vl-string-left-trim (rtos x 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq y (read lin))
	    (setq lin (vl-string-left-trim (rtos y 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq M11 (cons (read lin) M11))
	    (setq lin (vl-string-left-trim (rtos (car M11) 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq M22 (cons (read lin) M22))

	    (setq lin (vl-string-left-trim (rtos (car M22) 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq M12 (cons (read lin) M12))
	    (setq lin (vl-string-left-trim (rtos (car M12) 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	  )
					;else
	  (progn
	    (treat N x y M11 M22 M12)
;;;draw something or whatever
	    (setq N (read lin))
	    (setq lin (vl-string-left-trim (rtos N 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq x (read lin))
	    (setq lin (vl-string-left-trim (rtos x 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq y (read lin))
	    (setq lin (vl-string-left-trim (rtos y 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq M11 (list (read lin)))
	    (setq lin (vl-string-left-trim (rtos (car M11) 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq M22 (list (read lin)))
	    (setq lin (vl-string-left-trim (rtos (car M22) 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))

	    (setq M12 (list (read lin)))
	    (setq lin (vl-string-left-trim (rtos (car M12) 2) lin))
	    (setq lin (vl-string-left-trim "\t" lin))
	  )
	)				;end if



	(setq cntr (+ cntr 1))
	(setq lin (read-line fil))
	;;debugging
	

      )
      (treat N x y M11 M22 M12)

      
      (close fil)
    )

    (alert "Error - File not opened")
  )

;;;--- Turn the command echo back on
  (setvar "cmdecho" 1)
  (princ)
)

