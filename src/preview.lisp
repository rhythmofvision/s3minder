
(in-package :preview)

(defvar *host* "http://test.com/")

(s3-blog::defs3blog
  :domain-name *host*
  :dirs '(#p"/home/cb/dev/2014/s3minder/public")
  :sync-on-compile t
  :s3-config (list :ACCESS-KEY ""
                   :SECRET-KEY ""
                   :BUCKET ""))

(defun static (filename)
  (concatenate 'string "/" filename))

(defpage index
  (:html
   (:head
    (:link :href "http://fonts.googleapis.com/css?family=Source+Sans+Pro:300,400,600,600italic' rel='stylesheet" :type "text/css")
    (:link :href "http://fonts.googleapis.com/css?family=Raleway:200,400" :rel "stylesheet" :type "text/css")
    (:link :rel "stylesheet" :href (static "style/kube.css") :type "text/css")
    (:link :rel "stylesheet" :href (static "style/main.css") :type "text/css")
    (:script :src "//code.jquery.com/jquery-1.11.3.min.js"))
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
                                     (:ul (:li ;; (:a :class "btn btn-blue btn-outline" :href "/login" "Login")
                                               ))))))
          (:br) (:br)
          (:div :class "units-row"
                (:div :class "unit-centered"
                      (:h3 (:span :class "underline" "Flohu") ": cloud file manager for " (:b "developers"))))
          (:br) (:br)
          (:div :class "units-row"
                (:div :class "unit-centered unit-80" :style "text-align: center"
                      (:p :style "font-size: 1.45em;" "We are in closed beta testing mode right now, if you're interested in our product please leave your email below")
                      (:br) (:br) (:br)
                      (:form :class "forms forms-inline" :id "form"
                             (:input :type "email" :name "email" :class "input-big width-50" :style "text-align: center" :placeholder "Your Email") "&nbsp;" "&nbsp;"
                             (:button :class "btn btn-big btn-green" "Interested!"))
                      (:div :id "thanks" :style "display: none"
                            (:p "Thanks for your interest, we will email you as soon as we have something to show!")))))
    (:script "$(function () {
$('#form').on('submit', function (ev) {
    ev.preventDefault();
    var data = $(this).serializeArray();
    
    var form_url = 'https://docs.google.com/forms/d/1O_cDt5iSOqaeFm5vwWSKHBY4xjSGva9wb7w_TyveMiA/formResponse';
    var map  = { 'email': 'entry.805419211' };
    var post = {};

    for (var idx in data) {
        var obj = data[idx];
        post[ map[obj.name] ] = obj.value;
    }

    $.ajax({
      crossDomain: true,
      dataType: 'jsonp',
      url: form_url,
      data: post,
      type: 'POST',
      success: function () {
        $('#form').hide();
        $('#thanks').show();
        
      },
      error: function () {
        $('#form').hide();
        $('#thanks').show();
        
      }
    });

});
}); 
"))))
