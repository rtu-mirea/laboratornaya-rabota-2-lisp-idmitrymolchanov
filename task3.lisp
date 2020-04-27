;;Функция сжатия списка
(defun compress (w &optional (n 1) ac)
  (cond ((null w) (reverse ac))
        ((equal (car w) (cadr w)) (compress (cdr w) (1+ n) ac))
        ((> n 1) (compress (cdr w) 1 (cons (list (car w) n) ac)))
        ((compress (cdr w) 1 (cons (car w) ac)))))

(defun list_of (cnt item)
    (if (eq cnt 0)
        `()
        (cons item (list_of (- cnt 1) item))
    )
)

(defun deccomprsf (all i)
    (append 
        (if (consp i)
            (list_of (nth 0 i) (nth 1 i))
            (list i)
        )
        all
    )
)

;;Функция расжатия списка
(defun decompress (lst)
    (reverse(reduce #'deccomprsf lst :initial-value (list)))
)

(format t "~a~%"(compress '(a b b b c c c d d)))
(format t "~a~%"(decompress (compress '(12 lll 1 1 1 g 5 5))))