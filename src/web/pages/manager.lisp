
(in-package :s3minder-pages)

(defun get-user-js-var ()
  `(var user-data (create))); ,@(dao-to-list (hunchentoot-sessions-redis:session-data 'user)))))

(define-route manager ("/manager")
  (:decorators #'pw:@requires-auth)

  "Manager"

  (let ((user-js-var (ps:ps* (get-user-js-var))))
  
  (cl-who:with-html-output-to-string (out)
    (:html
     (:head

      (:link :href "http://fonts.googleapis.com/css?family=Open+Sans:700,400" :rel "stylesheet" :type "text/css")
      (:link :href "http://fonts.googleapis.com/css?family=Raleway:400,700" :rel "stylesheet" :type "text/css")
      
      (:link :rel "stylesheet" :href "/public/style/kube.css" :type "text/css")
      (:link :rel "stylesheet" :href "/public/style/main.css" :type "text/css")

      ; (:style "body { background: grey }")
      ; (:script (str (ps:ps* parenscript:*ps-lisp-library*)))
      (:script (str (compile-psruntime "plus")))
      (:script (str (compile-psruntime "contracts")))
      
      (:script :src "/public/lib/util.js")
      (:script :src "/public/lib/HTML.js")
      (:script :src "/public/lib/zepto.js")
      (:script :src "/public/lib/date.js")
      (:script :src "/public/lib/react-with-addons.js")
      (:script :src "/public/lib/blame.js")
      (:script :src "/public/lib/sc.js")      
      
      (loop for url in (reverse (all-files-urls "s3m"))
         do (htm (:script :src (concatenate 'string "/js/" url))))

      (:div :class "main" :style "width: 100%; max-width: 90%;"
            (:div :id "jobs")
            (:div :id "main")
            (:div :id "helper"))

      (:div :class "main" :style "clear: both;" "Flohu, 2015")
      
      (:script (str user-js-var))
      (:script (str "main()")))))))
