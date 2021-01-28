(defun c:mu()
  (setq p1 (getpoint "start: "))
  (setq p2 (getpoint p1 "second: "))
  (setq p3 (getpoint p2 "third:  "))
  (setq d1 (distance p1 p2))
  (setq d2 (distance p2 p3))
 ; (setq alpha 0.01)
  (setq alpha (/ (min d1 d2) (max d1 d2)))

  (setq talpha (list 0.4 0.45 0.5 0.55 0.6 0.65 0.7 0.75 0.8 0.85 0.9 0.95 1))
  (setq tmiux (list 0.11
		    0.102
		    0.095
		    0.088
		    0.081
		    0.0745
		    0.068
		    0.062
		    0.056
		    0.051
		    0.046
		    0.041
		    0.037)
	)
  (setq tmiuy (list 0
		    0
		    0
		    0
		    0.305
		    0.369
		    0.436
		    0.509
		    0.595
		    0.685
		    0.778
		    0.887
		    1)
	)
  
  (setq i 1) 
  (while (and (nth i talpha) (> alpha (nth 0 talpha)))
    (if (> (nth i talpha) alpha)
      (progn
	(setq up i)
	(setq do (- i 1))
	(setq i (length talpha))
	)
      )
    (setq i (+ i 1))
    )
  (if (<= alpha (nth 0 talpha))
    (progn
      (setq miux (nth 0 tmiux))
      (setq miuy (nth 0 tmiuy))
    )
    ;else
    (progn
      (setq miux (+ (nth do tmiux) (* (/ (- (nth up tmiux) (nth do tmiux)) (- (nth up talpha) (nth do talpha)) ) (- alpha (nth do talpha)))))
      (setq miuy (+ (nth do tmiuy) (* (/ (- (nth up tmiuy) (nth do tmiuy)) (- (nth up talpha) (nth do talpha)) ) (- alpha (nth do talpha)))))
      )
  )
  (print (strcat "Alpha: " (rtos alpha)))
  (print (strcat "MiuX: " (rtos miux)))
  (print (strcat "MiuY: " (rtos miuy)))
  (princ)
  )