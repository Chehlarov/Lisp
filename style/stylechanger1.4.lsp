;;;writen by Nikolay Chehlarov
;;;sept 2009
(vl-load-com)
(setq *layers*
       (vla-get-layers
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)
(setq eDictionary (vla-GetExtensionDictionary *ModelSpace*))
(setq sent (vla-getobject eDictionary "ACAD_SORTENTS"))
;;;moge i da nqma sy6testvuva6ta sortens table i moge da se nalogi da se naprvii
;;;;;;;;;;;;;
;;not used
(defun c:stylecreate ()

  (setq obj (vla-add *layers* "1K_arm_koti"))
  (vla-put-color obj acblue)
  (vla-put-lineweight obj acLnWt020)

  (setq obj (vla-add *layers* "1K_dupki"))
  (vla-put-color obj acblue)
  (vla-put-lineweight obj acLnWt050)

  (setq obj (vla-add *layers* "1K_gredi"))
  (vla-put-color obj 191)
  (vla-put-lineweight obj acLnWt050)

  (setq obj (vla-add *layers* "1K_kolony"))
  (vla-put-color obj acred)
  (vla-put-lineweight obj acLnWt090)

  (setq obj (vla-add *layers* "1K_kontur"))
  (vla-put-color obj acyellow)
  (vla-put-lineweight obj acLnWt050)
;;;;;;
  (setq obj (vla-add *layers* "1K_koti_koloni"))
  (vla-put-color obj acred)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_nasadeni"))
  (vla-put-color obj acred)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_otvori"))
  (vla-put-color obj 30)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_razrezi"))
  (vla-put-color obj acblue)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_shajby"))
  (vla-put-color obj accyan)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_steni"))
  (vla-put-color obj 134)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_van6ni"))
  (vla-put-color obj 241)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_van6ni2"))
  (vla-put-color obj 181)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_koti_vatre6ni"))
  (vla-put-color obj acyellow)
  (vla-put-lineweight obj acLnWt013)
;;;;;
  (setq obj (vla-add *layers* "1K_nasadeni koloni"))
  (vla-put-color obj 30)
  (vla-put-lineweight obj acLnWt090)

  (setq obj (vla-add *layers* "1K_shajby_2"))
  (vla-put-color obj 216)
  (vla-put-lineweight obj acLnWt050)

  (setq obj (vla-add *layers* "1K_shajby"))
  (vla-put-color obj accyan)
  (vla-put-lineweight obj acLnWt090)

  (setq obj (vla-add *layers* "1K_steni"))
  (vla-put-color obj 134)
  (vla-put-lineweight obj acLnWt050)

  (setq obj (vla-add *layers* "1K_strih"))
  (vla-put-color obj 31)
  (vla-put-lineweight obj acLnWt013)

  (setq obj (vla-add *layers* "1K_strih_steni"))
  (vla-put-color obj 251)
  (vla-put-lineweight obj acLnWt013)

)

;;
					;
					;
					;
					;
(defun s:back (lay / ss1 array ssi obj)
  (if (setq ss1 (ssget "_X" (list (cons '8 lay))))
    (progn
      (setq array (vlax-make-safearray
		    vlax-vbobject
		    (cons '0 (- (sslength ss1) 1))
		  )
      )
      (setq ssi 0)
      (while (setq obj (ssname ss1 ssi))
	(vlax-safearray-put-element
	  array
	  ssi
	  (vlax-ename->vla-object obj)
	)
	(setq ssi (+ ssi 1))
      )
      (vla-movetobottom sent array)
      (print (strcat lay " success!"))
    )
  )
)

(defun s:front (lay / ss1 array ssi obj)
  (if (setq ss1 (ssget "_X" (list (cons '8 lay))))
    (progn
      (setq array (vlax-make-safearray
		    vlax-vbobject
		    (cons '0 (- (sslength ss1) 1))
		  )
      )
      (setq ssi 0)
      (while (setq obj (ssname ss1 ssi))
	(vlax-safearray-put-element
	  array
	  ssi
	  (vlax-ename->vla-object obj)
	)
	(setq ssi (+ ssi 1))
      )
      (vla-movetotop sent array)
      (print (strcat lay " success!"))
    )
  )
)


;;;
					;
					;
					;
					;


(defun c:stylecheck ()
  (vlax-for for-item *layers*
    (setq name (vla-get-name for-item))
    (print name)
;;;;opravqne na imenata na sloevete
    (if	(wcmatch name "1k_*")
      (vla-put-name for-item (strcat "1K_" (substr name 4)))
    )
;;; smenq bylgrsko k
    (if	(wcmatch name "1Ê_*")
      (vla-put-name for-item (strcat "1K_" (substr name 4)))
    )

    (if	(wcmatch name "1ê_*")
      (vla-put-name for-item (strcat "1K_" (substr name 4)))
    )

    (if	(= name "1K_shajbi")
      (vla-put-name for-item "1K_shajby")
    )
    ;(if	(= name "1K_nasadeni_kolony")
    ;  (vla-put-name for-item "1K_nasadeni koloni")
   ; )
;;; opravqne debelinite na sloevete
    (if	(or
	  (wcmatch name "1K_kontur*")
	  (= name "1K_dupki")
	  (= name "1K_gredi")
	  (= name "1K_shajby_2")
	  (= name "1K_steni")
	)
      (vla-put-lineweight for-item acLnWt050)
    )

    (if	(or (wcmatch name "1K_koti*")
	    (= name "1K_arm_koti")
	)
      (vla-put-lineweight for-item acLnWt013)
    )

    (if	(or
	  (= name "1K_kolony")
	  (= name "1K_nasadeni koloni")
	  (= name "1K_nasadeni_kolony")
	  (= name "1K_shajby")
	)
      (vla-put-lineweight for-item acLnWt090)
    )

    (if	(or
	  (= name "1K_strih")
	  (= name "1K_strih_steni")
	)
      (vla-put-lineweight for-item acLnWt013)
    )

  )
)


;;;;;;;;;;;;;;;;;
(defun c:dorder	()
  (s:front "1K_kolony")
  (s:front "1K_razrez")
  (s:back "1K_nAXIS")
  (s:back "1K_razrez-solid")
  (s:back "1K_shajby")
  (s:back "1K_steni")

    (vlax-for for-item *layers*
      (setq name (vla-get-name for-item))
      (if (wcmatch name "1K_kontur*")
	(s:back name)
      )
    )
  
  (s:back "1K_kontur")
  (s:back "1K_gredi")
  (s:back "1K_bord")
  (s:back "1K_dupki")
  (s:back "1K_stalbi")
  (s:back "1K_shajby-solid")
  (s:back "1K_strih")
  (s:back "1K_strih_steni")
  (s:back "1K_otvori-solid")
  (s:back "1K_strih_steni")
  
  (s:front "1K_koti_otvori")
  (s:front "1K_koti_koloni")
  (s:front "1K_koti_nasadeni")
  (s:front "1K_koti_razrezi")
  (s:front "1K_koti_shajby")
  (s:front "1K_koti_steni")
  (s:front "1K_koti_van6ni")
  (s:front "1K_arm_koti")
  (s:front "1K_koti_van6ni2")
  (s:front "1K_koti_vatre6ni")

  (s:front "1K_arm")
  (s:front "1K_arm_x")
  (s:front "1K_arm_x_us")
  (s:front "1K_arm_y")
  (s:front "1K_arm_y_us")
  (s:front "1K_arm_koti")
  (s:front "1K_arm_koti_x")
  (s:front "1K_arm_koti_x_us")
  (s:front "1K_arm_koti_y")
  (s:front "1K_arm_koti_y_us")
  )

(defun c:stc()
  (c:stylecheck)
  (c:dorder)
  )


















					;(setq obj (car (entsel)))

					;(setq obja (vlax-ename->vla-object obj))

					;(vlax-make-safearray vlax-vbobject '(0 . 0))

					;(vla-movetobottom sent (vlax-safearray-fill
					;	    pointcc;
					;	    (list obja)
					;  ))