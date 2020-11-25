(in-package #:www.pariatech.com)

(defun replace-all (string part replacement &key (test #'char=))
"Returns a new string in which all the occurences of the part
is replaced with replacement."
    (with-output-to-string (out)
      (loop with part-length = (length part)
            for old-pos = 0 then (+ pos part-length)
            for pos = (search part string
                              :start2 old-pos
                              :test test)
            do (write-string string out
                             :start old-pos
                             :end (or pos (length string)))
            when pos do (write-string replacement out)
            while pos)))

(defun filter (fn lst)
  (labels ((f (fn lst yes no)
             (if lst
                 (if (funcall fn (car lst) (cdr lst))
                     (f fn (cdr lst) (cons (car lst) yes) no)
                     (f fn (cdr lst) yes (cons (car lst) no)))
                 (values (reverse yes) (reverse no)))))
    (f fn lst nil nil)))

(defun flatten (lst &key (depth most-positive-fixnum))
  (labels ((f (lst acc depth)
             (if lst
                 (if (listp lst)
                     (let ((head (car lst))
                           (tail (cdr lst)))
                       (if (and (listp head) (> depth 0))
                           (f tail (f head acc (1- depth)) depth)
                           (f tail (cons head acc) 0)))
                     (cons lst acc))
                 acc)))
    (reverse (f lst nil depth))))

(defun pairs (lst)
  (labels ((f (lst acc)
             (if lst
                 (let ((head (car lst))
                       (tail (cdr lst)))
                   (f (cdr tail) (cons (list head (car tail)) acc)))
                 (reverse acc))))
    (f lst nil)))

(defun keyword-pairs (lst)
  (multiple-value-bind (kws notkws)
      (filter (lambda (x) (keywordp (car x))) (pairs lst))
    (values kws (flatten notkws :depth 1))))

(defun keys-body (lst)
  (loop for rest on lst
        by #'cddr
        when (keywordp (car rest))
          collect (cons (car rest) (cadr rest))
            into keys
        when (not (keywordp (car rest)))
          return (list (cons 'keys keys) (cons 'body rest))
        finally (return (list (cons 'keys keys) (cons 'body nil)))))

(defmacro plet (kvals &body body)
  `(let ,(pairs kvals)
     ,@body))
