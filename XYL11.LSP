;CYLINDER.LSP v1.1
;
;There are no restrictions on the variables.
;However the "Resolution" variable should be kept to whole
;integers.  Suggested Resolution is 10.
;
;For the latest version send  $5.00(US) to:
;
;                 Kevin Reardon
;                 536 41 St. #27
;                 Oakland, CA   94609
;
;Enclose the version identification number of that one you have
;as I don't expect many more revisions.
;
(DEFUN PHIL ()
      (COMMAND "LINE" P1A P2A)
      (COMMAND)
      (COMMAND "LINE" P1B P2B)
      (COMMAND)
    )
(DEFUN PHILL ()
          (SETVAR "THICKNESS" 0)
          (COMMAND "SOLID" P1A P3A P2A P4A)
          (COMMAND)
          (COMMAND "SOLID" P1B P3B P2B P4B)
          (COMMAND)
    )
(DEFUN C:CYLINDER ()
   (SETQ L (GETVAR "CMDECHO"))
   (SETQ M (GETVAR "BLIPMODE"))
   (SETQ N (GETVAR "FILLMODE"))
   (SETQ TH (GETVAR "THICKNESS"))
   (SETQ EE (GETVAR "ELEVATION"))
   (SETQ E1 0)
   (SETQ P1 (GETPOINT "First point: "))
   (SETQ P2 (GETPOINT "Second point: "))
   (SETQ RAD (GETDIST P2 "Radius: "))
   (SETQ RES (GETREAL "Resolution: "))
   (SETVAR "BLIPMODE" 0)
   (SETVAR "FILLMODE" 0)
   (SETVAR "CMDECHO" 0)
   (SETQ A (- (ANGLE P1 P2) 1.570796))
   (SETQ B (+ (ANGLE P1 P2) 1.570796))
   (SETQ C 0)
   (SETQ P3A (POLAR P1 A RAD))
   (SETQ P4A (POLAR P2 A RAD))
   (SETQ P3B (POLAR P1 B RAD))
   (SETQ P4B (POLAR P2 B RAD))
   (WHILE (< C (1+ RES))
      (SETQ T (* (/ C RES) 1.570796))
      (SETQ X (* RAD (COS T)))
      (SETQ ELE (* RAD (SIN T)))
      (SETQ P1A (POLAR P1 A X))
      (SETQ P2A (POLAR P2 A X))
      (SETQ P1B (POLAR P1 B X))
      (SETQ P2B (POLAR P2 B X))
      (SETVAR "ELEVATION" (+ EE ELE))
      (SETVAR "THICKNESS" (- E1 ELE))
      (PHIL)
      (SETVAR "ELEVATION" (- EE E1))
      (PHILL)
      (SETVAR "ELEVATION" (- EE ELE))
      (SETVAR "THICKNESS" (- ELE E1))
      (PHIL)
      (SETVAR "ELEVATION" (+ EE E1))
      (PHILL)
      (SETQ P3A P1A)
      (SETQ P4A P2A)
      (SETQ P3B P1B)
      (SETQ P4B P2B)
      (SETQ C (1+ C))
      (SETQ E1 ELE)
       )
   (SETVAR "CMDECHO" L)
   (SETVAR "BLIPMODE" M)
   (SETVAR "FILLMODE" N)
   (SETVAR "THICKNESS" TH)
   (SETVAR "ELEVATION" EE)
 )
...-.... 1200  N81N          ........................