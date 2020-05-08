(defun readFile(path)
    (setf str (open path :direction :input :if-does-not-exist :error))
    (do ((line (read-line str nil 'eof)
            (read-line str nil 'eof)))
        ((eql line 'eof))
		
        (format t "~a~%" line)
    )
)