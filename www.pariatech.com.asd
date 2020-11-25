;;;; www.pariatech.com.asd

(asdf:defsystem #:www.pariatech.com
  :description "Describe www.pariatech.com here"
  :author "Your Name <your.name@example.com>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :components ((:file "package")
               (:file "utils")
               (:file "xml")
               (:file "css")
               (:file "org")
               (:file "webserver")
               (:file "website-generator")
               (:file "www.pariatech.com")))
