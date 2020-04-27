(write-line "Hello World")
(defun insertP (lst num elt)
  (if (zerop num) (return-from insertP (cons elt lst)))
  (rplacd (nthcdr (- num 1) lst) (cons elt (nthcdr num lst)))
  lst)

(defun deleteP (lst &optional (n x))
  (cond ((zerop n) (cdr lst))
        (t (cons (car lst) (deleteP (cdr lst) (- n 1))))))
  
(defun findP (x s)
   (cond ((null s) nil)
         ((equal x (car s)) 0)
         (t (+ 1 (findP x (cdr s))))))


 
(format t "~a~%" (insertP '(AB 234H 89 BBBb C 4) 2 'x))
(format t "~a~%" (insertP '(AB 234H 89 BBBb C 4) 6 'x))

(format t "~a~%" (deleteP '(AB 234H 89 BBBb C 4) 2)) 
(format t "~a~%" (deleteP '(AB 234H 89 BBBb C 4) 5)) 

(format t "~a~%" (findP 'j '(j h p l h o p))) 
(format t "~a~%" (findP 'hh '(j h p l hh o p)))