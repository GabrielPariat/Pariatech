
(in-package :www.pariatech.com)

(defun http-char (c1 c2 &optional (default #\Space))
  (let ((code (parse-integer
               (coerce (list c1 c2) 'string)
               :radix 16
               :junk-allowed t)))
    (if code
        (code-char code)
        default)))

(defun decode-param (s)
  (labels ((f (lst)
             (when lst
               (case (car lst)
                 (#\% (cons (http-char (cadr lst) (caddr lst))
                            (f (cdddr lst))))
                 (#\+ (cons #\space (f (cdr lst))))
                 (otherwise (cons (car lst) (f (cdr lst))))))))
    (coerce (f (coerce s 'list)) 'string)))

(defun parse-params (s)
  (let ((i1 (position #\= s))
        (i2 (position #\& s)))
    (cond (i1 (cons (cons (intern (string-upcase (subseq s 0 i1)))
                          (decode-param (subseq s (1+ i1) i2)))
                    (and i2 (parse-params (subseq s (1+ i2))))))
          ((equal s "") nil)
          (t s))))

(defun parse-url (s)
  (let* ((url (subseq s
                      (+ 2 (position #\space s))
                      (position #\space s :from-end t)))
         (x (position #\? url)))
    (if x
        (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
        (cons url '()))))

(defun get-header (stream)
  (let* ((s (read-line stream))
         (h (let ((i (position #\: s)))
              (when i
                (cons (intern (string-upcase (subseq s 0 i)))
                      (subseq s (+ i 2)))))))
    (when h
      (cons h (get-header stream)))))

(defun get-content-params (stream header)
  (let ((length (cdr (assoc 'content-length header))))
    (when length
      (let ((content (make-string (parse-integer length))))
        (read-sequence content stream)
        (parse-params content)))))

(require 'sb-bsd-sockets)
(defparameter *localhost-address* '(127 0 0 1))
(defun socket-server (port)
  (let ((socket (make-instance 'sb-bsd-sockets:inet-socket
                               :type :stream :protocol :tcp)))
    (setf (sb-bsd-sockets:sockopt-reuse-address socket) t)
    (sb-bsd-sockets:socket-bind socket *localhost-address* port)
    (sb-bsd-sockets:socket-listen socket 255)
    socket))

(defmacro socket-accept (socket &body body)
  `(let ((c (sb-bsd-sockets:socket-accept ,socket)))
    (unwind-protect
         (let ((stream (sb-bsd-sockets:socket-make-stream c :output t :input t)))
           ,@body)
      (sb-bsd-sockets:socket-close c))))

(defun socket-server-close (socket)
  (sb-bsd-sockets:socket-close socket))

(defvar *server-running* nil)
(defvar *request-handler* nil)
(defvar *server-socket* nil)

(defmacro defhandler (params &body body)
  `(setf *request-handler*
         (lambda ,params
           ,@body)))

(defun stop-server ()
  (setf *server-running* nil))

(defun serve ()
  (unwind-protect
       (loop while *server-running*
             do (socket-accept *server-socket*
                  (let* ((url  (parse-url (read-line stream)))
                         (path (car url))
                         (header (get-header stream))
                         (params (append (cdr url)
                                         (get-content-params stream header)))
                         (*standard-output* stream))
                    (when *request-handler*
                      (funcall *request-handler* path header params)))))
    (progn (socket-server-close *server-socket*)
           (stop-server))))

(defun start-server ()
  (unless *server-running*
    (progn
      (setf *server-running* t)
      (setf *server-socket* (socket-server 8080))
      (sb-thread:make-thread #'serve))))


(defun print-thread-info ()
      (let* ((curr-thread sb-thread:*current-thread*)
             (curr-thread-name (sb-thread:thread-name curr-thread))
             (all-threads (sb-thread:list-all-threads)))
        (format t "Current thread: ~a~%~%" curr-thread)
        (format t "Current thread name: ~a~%~%" curr-thread-name)
        (format t "All threads:~% ~{~a~%~}~%" all-threads))
      nil)
