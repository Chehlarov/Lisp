;;;this lisp changes the loading state of Xrefs
;;;written by Nikolay Chehlarov MART 2009
(defun c:x1 ()
       (x:xx "1")
  )
(defun c:x2 ()
       (x:xx "2")
  )
(defun c:x3 ()
       (x:xx "0")
  )

(defun c:x4 ()
       (x:xx "4")
  )

(defun x:xx(arg / cObj cName)
  (setq xarr nil)
  ;;;check the last Xref on
  (setq cObj(tblnext "BLOCK" T))
  (while cObj
    (setq cName(cdr(assoc 2 cObj)))
    (if
      (and
	 (=(logand(cdr(assoc 70 cObj))32)32)
	 (=(logand(cdr(assoc 70 cObj))4)4)
	 ); end and
      (progn
	(setq vname cName)
      ; (vl-cmdf "_.xref" "_unload" cName)
      ; (vl-cmdf "_.xref" "_reload" cName)
       )
      )
  (setq cObj(tblnext "BLOCK"))
  )
;;;swith all Xrefs off and create xarr
  (setq cObj(tblnext "BLOCK" T))
  (while cObj
    (setq cName(cdr(assoc 2 cObj)))
    (if	(= (logand (cdr (assoc 70 cObj)) 4) 4)
      (progn
	(setq xarr (append xarr (list (list cName))))
      (vl-cmdf "_.xref" "_unload" cName)
      )
    )
  (setq cObj(tblnext "BLOCK"))
  )
  ;;;sort xarr
  
  (setq xarr2 (list "xxx"))
  (setq i 0)
  (while (<= i (length xarr) )
    (setq j 0)
    (while (nth j xarr)
      (if (= i (atoi (substr (car(nth j xarr)) 1 2)))
	(setq xarr2 (append xarr2 (list (nth j xarr))))
	)
      (setq j (+ j 1))
      )
    (setq i (+ i 1))
    )

 ; (print "aaaa    xxxx    aaa ")
;  (print vname)
  (setq i 1)
  (while (nth i xarr2)
    (if (= (car(nth i xarr2)) vname) (setq vi i))
    (setq i (+ i 1))
    )

(print arg)
 (if (= arg "1")
   (if (> vi 1)
     (vl-cmdf "_.xref" "_reload" (car (nth (- vi 1) xarr2)))
     (vl-cmdf "_.xref" "_reload" (car (nth vi xarr2)))
   )
 )
 (if (= arg "2")
   (if (< vi (length xarr))
     (vl-cmdf "_.xref" "_reload" (car (nth (+ vi 1) xarr2)))
     (vl-cmdf "_.xref" "_reload" (car (nth vi xarr2)))
   )
 )
  (if (= arg "4")
      (vl-cmdf "_.xref" "_reload" (car (nth  vi xarr2)))
 )


  (print xarr2)
  
   (princ)
  )