;Incializationg
(vl-load-com)
(setq *ModelSpace*
       (vla-get-ModelSpace
	 (vla-get-ActiveDocument (vlax-get-acad-object))
       )
)

;;;111 4ertae kryg za problem
(defun pr:drawc	(center)
  (if (= (getvar 'INSUNITS) 5)
    (vla-addCircle
      *ModelSpace*
      (vlax-3d-point center)
      2
    )
    ((vla-addCircle
       *ModelSpace*
       (vlax-3d-point center)
       2
     )
    )
  )
)
;;;111 end 4ertae kryg za problem
;;;222
(defun pr:getss (/ ss)
  (setq ss (ssget))
)
;;;222 end
;;;333  selectiong output table for bars
(defun pr:gettable ()
  (setq table (car (entsel "Select table: ")))
  (if (or (= table nil) (/= (cdr (assoc 2 (entget table))) "bartable"))
    (progn
      (princ "Right table not selected!")
      (pr:gettable)
      )
      )
  table
  )
;;;333 end selectiong output table for bars

(defun c:sp ()
  (setq
    datatype "Moments"
	dialogLoaded T
	dialogShow T
  ) 

  (if (= -1 (setq dcl_id (load_dialog "spectot.dcl")))
    (progn
      (princ "\nCannot load spectot.dcl")
      (setq dialogLoaded nil)
    ) 
  ) 

  (if (and dialogLoaded
	   (not (new_dialog "loadiDialog" dcl_id))
      ) 
    (progn

      (princ "\nCannot show dialog loadiDialog")
      (setq dialogShow nil)
    ) 
  )


  (if (and dialogLoaded dialogShow)
    (progn
      ;; Set the initial state of the tiles
      (set_tile "loi_h0" (rtos 20 2 2))
      (set_tile "loi_Rb" (rtos 1.45 2 2))
      (set_tile "loi_Rs" (rtos 37.5 2 2))
      (action_tile
	"loi_M"
	"(setq datatype \"Moments\")"
      )
      (action_tile
	"loi_R"
	"(setq datatype \"Reinforcement\")"
      )
              
      (action_tile "cancel" "(done_dialog) (setq UserClick nil)")
      (action_tile
	"accept"
	(strcat	"(progn"
		"(setq h0 (atof (get_tile \"loi_h0\")))"
		"(setq Rb (atof (get_tile \"loi_Rb\")))"
		"(setq Rs (atof (get_tile \"loi_Rs\")))"
		"(setq colv (atof (get_tile \"loi_col\")))"
		"(done_dialog) (setq UserClick T))"
	)
      )
      
      (start_dialog)

      (unload_dialog dcl_id)


      (if UserClick			; User clicked Ok
	;; Build the resulting data
	(progn
	  (setq	Result (list
			 (cons 1 datatype)
			 (cons 2 h0)
			 (cons 3 Rb)
			 (cons 4 Rs)
			 (cons 5 colv)
		       )
	  )
	)
      )

    )
  )
  Result
) ;_ end of defun

