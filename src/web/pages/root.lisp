
(in-package :s3minder-pages)

(define-route root ("/")
  "Root page"
  (cl-who:with-html-output-to-string (out)
    (:html
     (:head
      (:link :href "http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,600,600italic' rel='stylesheet" :type "text/css")
      (:link :href "http://fonts.googleapis.com/css?family=Raleway:200,400" :rel "stylesheet" :type "text/css")
      (:link :rel "stylesheet" :href "/public/style/kube.css" :type "text/css")
      (:link :rel "stylesheet" :href "/public/style/main.css" :type "text/css"))
     (:body
      (:div :class "main"
            (:div :class "units-row"
                  (:div :class "unit-20"
                        (:h2 ""))
                  (:div :class "unit-80"
                        (:header :class "group"   
                                 (:nav :class "navbar navbar-left"
                                       (:ul :style "padding-top: 15px"
                                            (:li ;; (:a :href "/login" "About")
                                                 )))
                                 (:nav :class "navbar navbar-right"
                                       (:ul (:li (:a :class "btn btn-blue btn-outline" :href "/login" "Login")))))))
            (:br) (:br)
            (:div :class "units-row"
                  (:div :class "unit-centered"
                        (:h3 (:span :class "underline" "Flohu") ": cloud file manager for " (:b "developers"))))
            (:br) (:br)
            (:div :class "units-row"
                  (:div :class "unit-centered unit-80" :style "text-align: center"
                        (:p :style "font-size: 1.45em;" "We are in closed beta testing mode right now, if you're interested in our product please leave your email below")
                        (:br) (:br) (:br)
                        (:form :class "forms forms-inline"
                               (:input :type "email" :class "input-big width-50" :style "text-align: center" :placeholder "Your Email") "&nbsp;" "&nbsp;"
                               (:button :class "btn btn-big btn-green" "Interested!")))))))))
