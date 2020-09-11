;Zoom12.LSP:  Enhancement to the zoom function that allows you to zoom in
;             incrementally, zoom to a maximum zoom factor you set, zoom
;             to extents or all without a regen and more.
;____________________________________________________________________________

(defun mid (w z)
(LIST (/ (+ (CAR w) (CAR z))2) (/ (+ (CADR w) (CADR z))2))
)

(defun set_opt (/ Chooze YorN )
 (initget "Max In Out All")
 (setq chooze (getkword "\nMax/In/Out/All: "))
(cond
   (
     (= chooze "Max")
     (setq *max-z (getreal 
                    (strcat "\nEnter Maximum zoom value <" (rtos *max-z 2 2) ">: " )
                  )
     )
   )
   (
     (= chooze "In")
     (setq *in (getreal 
                    (strcat "\nEnter zoom in factor <" (rtos *In 2 2) ">: " )
                  )
     )
   )
   (
     (= chooze "Out")
     (setq *out (getreal 
                    (strcat "\nEnter zoom out factor <" (rtos *out 2 2) ">: " )
                  )
     )
   )
   (
     (= chooze "All")
       (princ "\nSelect Current view or Virtual screen limits for All option...")
       (initget "Current Virtual")
       (setq YorN (getkword "\nVirtual screen limits/<Current view>: "))
       (if (/= YorN "Virtual")
           (progn
           (setq *all t)
           (command "'view" "s" "all")
           )
           (setq *all nil)
       )
   )
);end cond
(initget "Max Set In Out All Window Previous Extents Rt")
(setq sw (getkword "\n All/Extents/In/Out/Max/Previous/Rt pan/Set/Window/<pan>: "))
(if (not sw)(setq sw ""))
(zvcont)
)




(defun ctrclc (ctr)
(setq xctr (car ctr) yctr (cadr ctr) zval3 (/ (- ymax ymin) zval))
(if(< xctr xlow)(setq xctr xlow))
(if(> xctr xhi)(setq xctr xhi))
(if(< yctr ylow)(setq yctr ylow))
(if(> yctr yhi)(setq yctr yhi))
(list xctr yctr)
)

(defun find_Zm ()
(if (> zval *max-z)
    (setq zval *max-z)
)
(if (or(= zval nil)(= zval 0))
    (setq zval zglobe)
)
(if (/= zval 1)
    (setq zglobe zval)
)
(if (<= zval 1)
    (setq zval1 2 zval 1)
    (setq zval1 (* zval 2))
)
(if (and (not ctr)(not prev))
    (setq ctr (getpoint "Pick view center: "))
)
(setq xctr (car ctr) 
      yctr (cadr ctr) 
      zval3 (/ (- ymax ymin) zval)
      xbuf (/ (- xmax xmin) zval1) 
      ybuf (/ (- ymax ymin) zval1)
      xlow (+ xmin xbuf) 
      xhi (- xmax xbuf)
      ylow (+ ymin ybuf) 
      yhi (- ymax ybuf)
      ctrfin (ctrclc ctr)
)
)

(defun draw (gdc / aa ba ca da hdst vdst)
(setq hdst (* (/ 1 zglobe) 0.5 (- xmax xmin)) vdst (* (/ 1 zglobe) 0.5 (- ymax ymin)))
(setq aa (list (- (caadr gdc) hdst) (- (cadadr gdc) vdst)))
(setq ba (list (+ (caadr gdc) hdst) (- (cadadr gdc) vdst)))
(setq ca (list (+ (caadr gdc) hdst) (+ (cadadr gdc) vdst)))
(setq da (list (- (caadr gdc) hdst) (+ (cadadr gdc) vdst)))
(grvecs
  (list 255 aa ba 255 ba ca 255 ca da 255 da aa)
)
)

(defun dr_sqr ()
      (progn
         (while (not pick)
            (setq gd (grread t))
            (draw gd)
            (draw gd)
            (setq gd2 gd)
            (setq pick (= 3 (car gd)))
         ) ;while
         (draw gd)
         (setq zval zglobe ctr (cadr gd))
      )
)


(defun rtz ()
      (setq vs (/ (getvar "viewsize") 4))
      (if (or(= zval nil)(= zval 0))(setq zval zglobe))
      (if (/= zval 1)(setq zglobe zval))
      (if (<= zval 1)(setq zval1 2 zval 1)(setq zval1 (* zval 2)))
      (setq xbuf (/ (- xmax xmin) zval1) ybuf (/ (- ymax ymin) zval1))
      (setq xlow (+ xmin xbuf) xhi (- xmax xbuf))
      (setq ylow (+ ymin ybuf) yhi (- ymax ybuf))
         (while (not pick)
            (setq gd (grread t))
            (setq ctrr (cadr gd))
            (setq ctrr (ctrclc (cadr gd)))
             (if
              (< vs (distance ctrr (setq vctr (getvar "viewctr"))))
               (progn
                 (setq nctr (polar vctr (angle vctr ctrr) vs))
                 (command "'pan" ctrr nctr)
               );end progn
             );end if
            (setq pick (or (= 3 (car gd)) (= 12 (car gd))(= 6 (car gd))))
          );end while
         (setq zval zglobe ctr nctr)
)


(defun zvcont ()
(cond
      (
      (= sw "Window")
         (setq a (getpoint "\nSelect view with window: "))
         (setq b (getcorner a))
         (setq ctr (mid a b))
         (setq zval (/ (- ymax ymin) (* (distance a b) 0.7)))
      (find_zm)
      )

      ( 
      (= sw "Set")
      (set_opt)
      )

      (
      (= sw "")
      (dr_sqr)
      (find_zm)
      )

      (
      (= sw "Rt")
      (rtz)
      )

       (
       (= sw "All")
       (if *all
          (command "'view" "r" "all")
          (progn
          (setq zval 1 
             ctr (mid (getvar "limmin")(getvar "limmax"))
          )
          (find_zm)
          )
        )
       )

      (
       (= sw "Max")
       (setq zval *max-z)
       (find_zm)
      )

      (
       (= sw "Extents")
        (command "ucs" "w")
        (command "'zoom" "w" (getvar "extmin")(getvar "extmax"))
        (command "ucs" "p")
      )

      (
       (= sw "Out")
       (setq zval (/ (- ymax ymin)(* (getvar "viewsize") *out)))
       (setq ctr (getvar "viewctr"))
       (find_zm)
      )

      (
       (= sw "In")
       (setq zval (/ (- ymax ymin)(* (getvar "viewsize") *in)))
       (setq ctr (getvar "viewctr"))
       (find_zm)
      )

      (
       (= sw "Previous")
       (setq prev t)
      )
);end cond
)

(defun c:zv (/ zval1 ctr xctr yctr xbuf ybuf pick
                xlow ylow xhi yhi ctrfin sw zval zval3 gd gd2 prev)
(setvar "cmdecho" 0)
(defun *error* (msg) (princ))
(setq sw t)
(if (not *max-z) (setq *max-z 100.0))
(if (not *in) (setq *in 0.6))
(if (not *out) (setq *out 1.5))
(if (or (= zglobe nil)(= zglobe 0))(setq zglobe 1))
(setq xmax (car (getvar "vsmax")) 
      xmin (car (getvar "vsmin"))
      ymax (cadr (getvar "vsmax")) 
      ymin (cadr (getvar "vsmin"))
)
  (initget "Max Set In Out All Window Previous Extents Rt")
  (setq sw (getkword "\n All/Extents/In/Out/Max/Previous/Rt pan/Set/Window/<pan>: "))
  (if (not sw)(setq sw ""))
  (zvcont)
  (if prev (command "'zoom" "P" )(command "'zoom" "C" ctrfin zval3))

)


