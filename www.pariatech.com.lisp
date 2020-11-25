;;;; www.pariatech.com.lisp

(in-package #:www.pariatech.com)

(defun xhtml (el)
  (format t "<!DOCTYPE html>")
  (xml el))

(defun logo (s)
  (let ((r (/ s 5)))
    `(svg :width ,s :height ,s
          (rect :fill "red" :x 0 :y 0 :width ,r :height ,(* 5 r))
          (rect :fill "red" :x ,r :y 0 :width ,r :height ,r)
          (rect :fill "red" :x ,r :y ,(* 2 r) :width ,r :height ,r)
          (rect :fill "red" :x ,(* 2 r) :y ,r :width ,r :height ,(* 2 r))
          (rect :fill "black" :x ,(* 2 r) :y 0 :width ,(* 3 r) :height ,r)
          (rect :fill "black" :x ,(* 3 r) :y ,r :width ,r :height ,(* 4 r)))))

(defvar head `(head (meta :charset "UTF-8")
                    (meta :name "viewport"
                          :content "width=device-width,initial-scale=1, shrink-to-fit=no")
                    (link :rel "stylesheet" :href "/css/styles.css")
                    (title "Pariatech")))

(defun link (active-path path label)
  `(a :href ,path
      ,(if (equalp active-path (subseq path 1))
           `(b ,label)
           label)))

(defun header (path)
  `(header (div :class "title"
                (a :class "logo"
                   :href "/"
                   ,(logo 30)
                   (strong www.pariatech.com)))
           (hr :class "hidden")
           (nav ,(link path "/" "Home") "&nbsp;"
                ,(link path "/fitness" "Fitness") "&nbsp;"
                ,(link path "/tech" "Tech"))
           (hr :class "hidden")))

(defun aside (path paths)
  `(aside (ul ,@(loop for i in paths
                      collect `(li ,(link path (car i) (cdr i)))))))

(defun page-template (path aside-path aside-paths &rest main)
  (xhtml
   `(html :lang "en"
           ,head
           (body ,(header path)
                 ,(aside aside-path aside-paths)
                 (main ,@main)
                 (footer)))))

(defpage index :route ""
  (page-template "" ""
                 '(("/" . "About") ("/contact" . "Contact"))
                 `(article (hgroup (h1 "Welcome to Pariatech")
                                   (h2 "The Home of a Tech'n'Fitness Enthusiast"))
                           (p "Welcome my name is Gabriel Pariat and here is my website to share my
love of tech and fitness. Enjoy your stay!"))))

(defpage contact :route "contact"
  (page-template "" "contact"
                 '(("/" . "About") ("/contact" . "Contact"))
                 `(article (h1 "Contact")
                           (p "You can contact me through this "
                              (a :href "mailto:gabrielpariat@gmail.com" "email")
                              " if you have any question or business inquiry."
                              "I'll be happy to answer!"))))

(defpage fitness :route "fitness"
  (page-template "fitness" "fitness"
                 '(("/fitness" . "About")
                   ("/fasting" . "Fasting")
                   ("/calisthenic" . "Calisthenic"))
                 `(article (h1 "Fitness")
                           (p "Fitness is pretty cool!"))))

(defpage fasting :route "fasting"
  (page-template "fitness" "fasting"
                 '(("/fitness" . "About")
                   ("/fasting" . "Fasting")
                   ("/calisthenic" . "Calisthenic"))
                 `(article (h1 "Fasting")
                           (p "Fasting is pretty cool!"))))

(defpage calisthenic :route "calisthenic"
  (page-template "fitness" "calisthenic"
                 '(("/fitness" . "About")
                   ("/fasting" . "Fasting")
                   ("/calisthenic" . "Calisthenic"))
                 `(article (h1 "Calisthenic")
                           (p "Calisthenic is pretty cool!"))))

(defpage tech :route "tech"
  (page-template "tech" "tech"
                 '(("tech" . "About")
                   ("tech/emacs" . "Emacs")
                   ("tech/common-lisp" . "Common Lisp")
                   ("tech/linux" . "Linux"))
                 `(article (h1 "About")
                           (p "Tech is pretty cool!"))))

(defpage emacs :route "tech/emacs"
  (page-template "tech" "emacs"
                 '(("../tech" . "About")
                   ("emacs" . "Emacs")
                   ("common-lisp" . "Common Lisp")
                   ("linux" . "Linux"))
                 `(article (h1 "Emacs")
                           (p "Emacs is pretty cool!"))))

(defpage common-lisp :route "tech/common-lisp"
  (page-template "tech" "common-lisp"
                 '(("../tech" . "About")
                   ("emacs" . "Emacs")
                   ("common-lisp" . "Common Lisp")
                   ("linux" . "Linux"))
                 `(article (h1 "Common Lisp")
                           (p "Common Lisp is pretty cool!"))))

(defpage common-lisp :route "tech/common-lisp/getting-started"
  (page-template "tech" "common-lisp"
                 '(("../" . "About")
                   ("../emacs" . "Emacs")
                   ("../common-lisp" . "Common Lisp")
                   ("../linux" . "Linux"))
                 (assoc 'article
                        (parse-org
                         (read-org-file
                          "pages/tech/common-lisp/getting-started.org")))))

(defpage linux :route "tech/linux"
  (page-template "tech" "linux"
                 '(("../tech" . "About")
                   ("emacs" . "Emacs")
                   ("common-lisp" . "Common Lisp")
                   ("linux" . "Linux"))
                 `(article (h1 "Linux")
                           (p "Linux is pretty cool!"))))

(defstylesheet styles
  (css
   `((body :margin 0
           :font-family "Roboto, monospace"
           :background-color "#282828"
           :color "#fff")
     (a :color "#fff")
     ("header a" :text-decoration none)
     ("header a.logo" :font-size 30px)
     ("header .title" :padding 0.5em)
     ("header nav" :background-color "#000"
                   :padding "0.5em 1em")
     ("header nav a" :color "#fff"
                     :padding "0.5em 1em")
     ("header nav a:hover" :box-shadow "inset 0 0 999px rgba(255, 255, 255, 0.2)")
     (main :padding "0 1em")
     ("@media screen and (min-width: 768px)"
      (aside :float left
             :width 10em
             :border-right "1px dotted #444")
      (main :margin "0 0 0 10em"
            :padding "0 2em"))
     ("aside ul" :padding 0)
     ("aside ul li" :list-style none)
     ("aside ul li a" :text-decoration none
                      :display block
                      :color "#fff"
                      :padding "0.5em 1em")
     ("aside ul li a:hover" :box-shadow "inset 0 0 999px rgba(255, 255, 255, 0.2)")

     (".hidden" :display none))))
