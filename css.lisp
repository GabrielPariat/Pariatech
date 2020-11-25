(in-package :www.pariatech.com)

(defun print-tabs (num)
  (format t "~v@{~C~:*~}" num #\tab))

(defun print-property-value (property value)
  (format t "~(~a~): ~(~a~);"
          property
          value))

(defun print-css-rule-set (rule-set deep)
  (let ((selector (car rule-set))
        (property-values (cdr rule-set)))
    (print-tabs deep)
    (format t "~a {~%"
            (if (stringp selector)
                selector
                (string-downcase (symbol-name selector))))
    (labels ((f (lst)
               (when lst
                 (let ((property (car lst))
                       (value (cadr lst)))
                   (if (listp property)
                       (progn (print-css-rule-set property (1+ deep))
                              (f (cdr lst)))
                       (progn (print-tabs (1+ deep))
                              (print-property-value property value)
                              (format t "~%")
                              (f (cddr lst))))))))
      (f property-values))
    (print-tabs deep)
    (format t "}~%")))

(defun css (lst)
  (loop for i in lst
        do (print-css-rule-set i 0)))

(defun inline-css (lst)
  (loop for (p v) on lst by #'cddr while v
        do (print-property-value p v)))
