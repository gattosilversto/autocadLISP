(defun YorN (Question)
   (initget 1 "Yes No")
   (getkword (strcat Question " [Yes or No] ?")))
