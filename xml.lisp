(defun xml (el)
  (let* ((tag (car el))
         (argvs (loop for (a v) on (cdr el)
                      by #'cddr while (keywordp a)
                      nconc (list a v)))
         (contents (remove-if (lambda (i) (find i argvs)) (cdr el))))
    (format t "~&<~(~a~)~{ ~(~a~)=\"~@[~a~]\"~}~:[~;/~]>~%"
            tag argvs (not contents))
    (when contents
      (progn
        (loop for content in contents
              do (if (listp content)
                     (xml content)
                     (format t "~a " content)))
        (format t "~&</~(~a~)>~%" tag)))))
