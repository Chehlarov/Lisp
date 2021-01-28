(defun c:mu()
  (setq p1 (getpoint "start: "))
  (setq p2 (getpoint p1 "end: "))
  (setq p3 (getpoint p2 "end: "))
  (setq d1 (distance p1 p2))
  (setq d2 (distance p2 p1))
  (setq alpha (/ (min d1 d2) (max d1 d2)))
  
  )