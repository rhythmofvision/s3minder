
(defpackage :preview
  (:use :cl :cl-who :s3-blog :plus.paren :serve.paren))

(defpackage :s3minder-util
  (:use :cl :alexandria :st-json :iterate)
  ;; macros
  (:export :concat)
  ;; functions
  (:export :last-char
           :last-char-slash-p
           :remove-trailing-slash
           :mapnlst
           :mapplist
           :to-string
           :meta-json
           :json-meta-to-plist
           :alist-meta-json
           :join-prefix
           :except-last
           :build-lookup-hash
           :get-keys
           :sign
           :get-signature-key
           :string-octets
           :string64
           :hash-string
           :get-policy
           :policy-expire-date
           :policy-date))

(defpackage :s3minder
  (:use :cl :s3minder-util :redis :zs3 :iterate :alexandria :split-sequence
        :st-json :puri :do-urlencode :esrap
        :mgl-pax)

  ;; key
  (:export :key
           :key-path
           :key-file
           
           :key-scheme
           :key-login
           :key-password
           :key-bucket
           :key-path
           :key-name
           :key-prefix
           :key-authority
           :key-query
           :key-fragment
           :key-meta

           :make-key
           :parse-key
           :print-key
           :save-key-redis
           
           :copy-key
           :move-key
           :delete-key

           :create-bkt))

(restas:define-module :s3minder-web
  (:use :cl
        :cl-who :iterate :restas :restas.directory-publisher :hunchentoot-sessions :postmodern
        :parenscript :serve.paren :plus.paren :react.paren :serve.paren :contracts.paren
        :s3minder-util
        :bordeaux-threads
        :mgl-pax)
  (:export :@owns-keys
           :@requires-auth)
  (:export :puser
           :email
           :s3-creds
           :user-keys
           :user-owns-keys
           :sync-key
           :sync-bucket
           :session-user
           :create-session
           :build-initial-index))

(restas:define-module #:s3minder-api
  (:use :cl
        :cl-who :iterate :restas :hunchentoot-sessions :st-json
        :s3minder-util
        :mgl-pax)
  (:local-nicknames (pw s3minder-web)
                    (ht hunchentoot)
                    (hts hunchentoot-sessions)))

(restas:define-module :s3minder-pages
  (:use :cl
        :cl-who :parenscript :restas :hunchentoot-sessions :st-json :postmodern
        :s3minder-util 
        :plus.paren :react.paren :serve.paren :contracts.paren
        :bordeaux-threads
        :mgl-pax)
  (:local-nicknames (pw s3minder-web)
                    (ht hunchentoot)
                    (hts hunchentoot-sessions)))
