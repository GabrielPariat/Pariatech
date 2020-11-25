(in-package #:www.pariatech.com)

(defvar *ressources* nil)

(defmacro http (code &body body)
  (let* ((keys-body (keys-body body))
         (keys (cdr (assoc 'keys keys-body)))
         (body (cdr (assoc 'body keys-body)))
         (content-type (cdr (assoc :content-type keys))))
    `(progn (format t "HTTP/1.1 ~a~&" ,code)
            (format t "Content-Type: ~(~a~); charset=utf-8~&" ,content-type)
            (format t "Content-Encoding: UTF-8~&")
            (format t "Accept-Ranges: bytes~&")
            (format t "Connection: keep-alive~&")
            (format t "~%")
            ,@body)))

(defmacro http404 (&body body)
  `(http "404 Not Found" ,@body))

(defmacro http200 (&body body)
  `(http "200 OK" ,@body))


(defhandler (path header params)
  (let ((ressource (assoc path *ressources* :test #'string-equal)))
    (if ressource
        (http200 :content-type (cadr ressource)
                 (funcall (caddr ressource)))
        (http404 :content-type "text/html"
          "Woopsy!"))))

(defmacro defressource (name &body body)
  (let* ((keys-body (keys-body body))
         (keys (cdr (assoc 'keys keys-body)))
         (body (cdr (assoc 'body keys-body)))
         (route (or (cdr (assoc :route keys)) (string name)))
         (content-type (cdr (assoc :content-type keys))))
    `(setq *ressources*
           (cons (list ,route
                       ,content-type
                       (lambda ()
                         ,@body))
                 *ressources*))))

(defmacro defpage (name &body body)
  `(defressource ,name
     :content-type 'text/html
     ,@body))

(defmacro defstylesheet (name &body body)
  `(defressource ,(format nil "css/~(~a~).css" name)
     :content-type 'text/css
     ,@body))

(defun ressource-path (content-type route)
  (concatenate 'string "dist/"
               (case content-type
                 ('text/html (concatenate 'string route
                                          (if (string= "" route) "" "/")
                                          "index.html"))
                 (otherwise route))))

(defun write-ressource-to-file (ressource)
  (let* ((route (car ressource))
         (content-type (cadr ressource))
         (ressourcefn (caddr ressource))
         (path (ressource-path content-type route)))
    (ensure-directories-exist path)
    (with-open-file (stream path
                            :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (let ((*standard-output* stream))
        (funcall ressourcefn)))))

(defun generate-pages ()
  (loop for ressource in (remove-duplicates *ressources*
                                            :key #'car
                                            :test #'string-equal
                                            :from-end t)
        do (write-ressource-to-file ressource)))

(defun clear-ressources ()
  (setq *ressources* nil))

;; (serve 'handler)
