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


;;;;;;;;;;;;;;;;;
(defun c:dorder	()

  (s:back "GO_0_SEC-Plateformes")
  (s:back "GO_0-SEC-pentes")
  (s:back "GO_0-SEC-SYN RSV CUI")
  (s:back "GO_0-SEC-SYN RSV CUI Voile")
  (s:back "GO_0-SEC-SYN RSV CVC Dalle")
  (s:back "GO_0-SEC-SYN RSV CVC Voile")
  (s:back "GO_0-SEC-SYN RSV ELEC")
  (s:back "GO_0-SEC-SYN RSV ELEC Dalle")
  (s:back "GO_0-SEC-SYN RSV ELEC Voile")  
  (s:back "GO_0-SEC-SYN RSV PB Dalles")    
  (s:back "GO_0-SEC-SYN RSV PB Voiles")
  (s:back "GO_0-SEC-RSV")
  (s:back "GO_0-SEC-SYN RSV CUI")
  (s:back "GO_0-SEC-SYN RSV ELEC")
  (s:back "GO_0-SEC-SYNT-RSV-PB-Dalles")
  (s:back "GO_0-SEC-SYNT-RSV-PB-Voiles")
  (s:back "GO_0-SEC-portes")
  (s:back "GO_0-SEC-recharges")
  (s:back "GO_0-SEC-hachures") ;doesn't matter at all

  (s:front "GO_0-SEC-axes-OK") ;important
  (s:front "GO_0-SEC-SF")
  (s:front "GO_0-SEC-SI")  
  (s:front "GO_0-SEC-dalles")
  (s:front "GO_0-SEC-linteau")
  (s:front "GO_0-SEC-poutres")
  (s:front "GO_0-SEC-murs")
  (s:front "GO_0-SEC-poteaux")  
  (s:front "GO_0-SEC-niv sup acrotere")
  (s:front "GO_0-SEC-niv sup")
  (s:front "GO_0-SEC-dalles-reperes")
  (s:front "GO_0_SEC-SF_Repere")
  (s:front "GO_0_SEC-SI_Repere")
  (s:front "GO_0-SEC-poteaux-reperes")
  (s:front "GO_0-SEC-murs-reperes")
  (s:front "GO_0-SEC-poutres-reperes")
  (s:front "GO_0-SEC-modifs")
  (s:front "GO_0-SEC-modifs ind 0")
  (s:front "GO_0-SEC-modifs ind A")
  (s:front "GO_0-SEC-modifs ind B")
  (s:front "GO_0-SEC-modifs ind C")
  (s:front "GO_0-SEC-modifs ind D")
  (s:front "GO_0-SEC-modifs ind E")
  (s:front "GO_0-SEC-notas")
  (s:front "GO_0-SEC-RSV-cotes")
  (s:front "GO_0-SEC-cotations-SYN")
  (s:front "GO_0-SEC-cotations") ;very important
  (s:front "GO_0-SEC-cotations-AXE")
)
