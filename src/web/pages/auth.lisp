
(in-package :s3minder-pages)

(define-route signout ("/signout" :method :post)
  (:render-method #'write-json-to-string)
  "Signout"
  (jso "status" "okay"))

(define-route login ("/login" :method :get)
  "Auth page"
  (cl-who:with-html-output-to-string (out)
    (:html
     (:head

      (:link :rel "stylesheet" :href "/public/style/kube.css" :type "text/css")
      (:link :rel "stylesheet" :href "/public/style/main.css" :type "text/css"))

     (:body
      (:div :class "main login"
            (:div :class "units-row"
                  (:div :class "unit-50"
                        (:h4 "Login")
                        (:form :method "post" :action "/login" :class "forms"
                               (:fieldset
                                (:label "Email"
                                        (:input :type "email" :name "user-email" :class "width-100"))
                                (:label "Password"
                                        (:input :type "password" :name "user-password" :class "width-100"))
                               (:p (:button :class "btn btn-blue btn-outline" "Log In")))))

                  (:div :class "unit-50"
                        (:h4 "Register")
                        (:form :method "post" :action "/register" :class "forms"
                               (:fieldset
                                (:label "Email"
                                        (:input :type "email" :name "user-email" :class "width-100"))
                                (:label "Password"
                                        (:input :type "password" :name "user-password" :class "width-100"))
                                (:label "AWS Access Key"
                                        (:input :type "text" :name "aws-access-key" :class "width-100"))
                                (:label "AWS Secret Key"
                                        (:input :type "text" :name "aws-secret-key" :class "width-100"))
                                (:p (:button :class "btn btn-blue btn-outline" "Register")))))))))))

(define-route login-form ("/login" :method :post)
  (alexandria:if-let ((user-dao (pw::find-user (hunchentoot:post-parameter "user-email")
                                               (hunchentoot:post-parameter "user-password"))))
    (progn
      (pw:create-session user-dao)
      (restas:redirect 'manager))
    (restas:redirect 'login)))

(define-route register ("/register" :method :post)
  (:requirement (lambda ()
                  (and (s3minder::check-aws-credentials (hunchentoot:post-parameter "aws-access-key")
                                                       (hunchentoot:post-parameter "aws-secret-key"))
                       (not (pw::find-user-by-email (hunchentoot:post-parameter "user-email"))))))
  (let ((user-dao (make-dao 'pw::puser
                            :email (hunchentoot:post-parameter "user-email")
                            :password (hunchentoot:post-parameter "user-password")
                            :aws-access-key (hunchentoot:post-parameter "aws-access-key")
                            :aws-secret-key (hunchentoot:post-parameter "aws-secret-key"))))
    (pw::create-session user-dao)
    
    (bordeaux-threads:make-thread #'(lambda ()
                                      (pw::build-initial-index user-dao)))

    (pw::set-sync-in-progress user-dao)
    (restas:redirect 'manager)))
