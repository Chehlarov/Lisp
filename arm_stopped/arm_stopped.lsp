(defun c:dup()
  (setq p1 (getpoint " "))
  (setq p2 (getpoint p1))
  (setq dist (distance p1 p2))
  (print dist)
  (setq diam (getint "diam: "))
  (setq span (getint "span: "))
  (setq areastopped (* (/ (* pi diam diam) 400) (/ dist span) ))
  (setq areareplaement (* 0.75 areastopped))
  (print "�� ����� ������ �� ������� ")
  (princ areareplaement)
  (princ)
  )

(defun c:us()
  (setq p1 (getpoint " "))
  (setq p2 (getpoint p1))
  (setq dist (distance p1 p2))
  (print dist)
  (setq diam (getint "diam: "))
  (setq span (getint "span: "))
  (setq areastopped (* (/ (* pi diam diam) 400) (/ dist span) ))
  (setq areareplaement (* 0.3 areastopped))
  (print "�� �������� ����� ")
  (princ areareplaement)
  (princ)
  )