Here's one more, this one also get's 2 points and returns
one point which has the x of the first point and the y
of the second point.
 
usage: At any AutoCAD "point:" prompt, reply (XY) <space>
 
(defun XY()
  (list
    (car  (getpoint "\nX point: ") )
    (cadr (getpoint "  Y point: ") )
  )
)
 
 
-Tony
