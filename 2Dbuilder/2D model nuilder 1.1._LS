;;;written by Nikolay Chehlarov
;;;Sofia, july 2008
;;;tazi programa otrqzva 3dface elementi ne6to kato trim samo 4e raboti za 3dface
(vl-load-com)

(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

(defun 2db:intersec (ob1 ob2)
  (setq obj1 (vlax-ename->vla-object ob1))
  (setq obj2 (vlax-ename->vla-object ob2))
  (setq	int (vla-IntersectWith
	      obj1
	      obj2
	      acExtendNone
	    )
  )
 ; (print(vlax-variant-type int))
  (if  (/= (vlax-safearray-get-u-bound (vlax-variant-value int) 1) -1)
      (progn
  (setq	int1 (list
	       (car (vlax-safearray->list (vlax-variant-value int)))
	       (cadr (vlax-safearray->list (vlax-variant-value int)))
	       (caddr (vlax-safearray->list (vlax-variant-value int)))
	     )
  )
  (setq	int2 (list
	       (nth 3 (vlax-safearray->list (vlax-variant-value int)))
	       (nth 4 (vlax-safearray->list (vlax-variant-value int)))
	       (nth 5 (vlax-safearray->list (vlax-variant-value int)))
	     )
  )
  (list int1 int2)
  )
    )
)

(defun c:2dt ()
  (while (not (setq obl (car (entsel "select line")))))
  (if (setq ss (arm:getss))
    (progn
      (setq ssi 0)
      (initget 1 "T L B R")
      (setq msg "Cut from [Left, Right, Top, Bottom]")
      (setq answ (getkword msg))
      


      (while (setq ob3dface (ssname ss ssi))
;	(print ob3dface)

	(setq int1 (car (2db:intersec obl ob3dface)))
	(setq int2 (cadr (2db:intersec obl ob3dface)))
	;(print int1)
	;(print int2)
	(if (and int1 int2)
	  (progn
	(if (or (= answ "T") (= answ "B"))
	  (if (> (car int1)
		 (car int2)
	      )
	    (progn
	      (setq temp int1)
	      (setq int1 int2)
	      (setq int2 temp)
	    )
	  )
	)
	(if (or (= answ "L") (= answ "R"))
	  (if (> (cadr int1)
		 (cadr int2)
	      )
	    (progn
	      (setq temp int1)
	      (setq int1 int2)
	      (setq int2 temp)
	    )
	  )
	)

	(setq objdat (entget ob3dface))
;;;;cutting top;;;
	(if (and (= (cdr (assoc 0 objdat)) "3DFACE") (= answ "T"))
	  (progn
	    (if	(= (cadr (assoc '12 objdat)) (car int2))
	      (setq objdat
		     (subst
		       (cons '12 int2)
		       (assoc '12 objdat)
		       objdat
		     )
	      )
	    )
	    (if	(= (cadr (assoc '13 objdat)) (car int1))
	      (setq objdat
		     (subst
		       (cons '13 int1)
		       (assoc '13 objdat)
		       objdat
		     )
	      )
	    )
	    (entmod objdat)
	  )
	)
;;;;cutting bottom;;;
	(if (and (= (cdr (assoc 0 objdat)) "3DFACE") (= answ "B"))
	  (progn
	    (if	(= (cadr (assoc '11 objdat)) (car int2))
	      (setq objdat
		     (subst
		       (cons '11 int2)
		       (assoc '11 objdat)
		       objdat
		     )
	      )
	    )
	    (if	(= (cadr (assoc '10 objdat)) (car int1))
	      (setq objdat
		     (subst
		       (cons '10 int1)
		       (assoc '10 objdat)
		       objdat
		     )
	      )
	    )
	    (entmod objdat)
	  )
	)
;;;cutting left;;;
	(if (and (= (cdr (assoc 0 objdat)) "3DFACE") (= answ "L"))
	  (progn
	    (if	(= (caddr (assoc '10 objdat)) (cadr int1))
	      (setq objdat
		     (subst
		       (cons '10 int1)
		       (assoc '10 objdat)
		       objdat
		     )
	      )
	    )
	    (if	(= (caddr (assoc '13 objdat)) (cadr int2))
	      (setq objdat
		     (subst
		       (cons '13 int2)
		       (assoc '13 objdat)
		       objdat
		     )
	      )
	    )
	    (entmod objdat)
	  )
	)
;;;cutting right;;;
	(if (and (= (cdr (assoc 0 objdat)) "3DFACE") (= answ "R"))
	  (progn
	    (if	(= (caddr (assoc '11 objdat)) (cadr int1))
	      (setq objdat
		     (subst
		       (cons '11 int1)
		       (assoc '11 objdat)
		       objdat
		     )
	      )
	    )
	    (if	(= (caddr (assoc '12 objdat)) (cadr int2))
	      (setq objdat
		     (subst
		       (cons '12 int2)
		       (assoc '12 objdat)
		       objdat
		     )
	      )
	    )
	    (entmod objdat)
	  )
	)
	(entupd ob3dface)
	);progn
	  );if (and int1 int2)
	(setq ssi (+ ssi 1))
	
      )					;while
    )					;progn
  )					;if arm:getss
)