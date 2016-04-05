(asdf:defsystem :s3m
  :name "s3minder"
  :description ""
  :author "Rhythm of vision"
  :maintainer "Rhythm of vision"
  :license "The MIT License (MIT)"
  :components ((:static-file "s3m.asd")
               (:module "src"
                        :components ((:file "package")
                                     (:file "utils")
                                     (:file "key")
                                     (:file "s3m")
                                     (:file "actions")
                                     (:module "web"
                                              :components ((:file "permissions")
                                                           (:file "main")
                                                           (:file "model")
                                                           (:file "pages/root")
                                                           (:file "pages/auth")
                                                           (:file "pages/manager")
                                                           (:file "api/private")))))
               
               (:module "bower_components"
                        :components ((:javascript-file "HTML/dist/HTML")
                                     (:javascript-file "zepto/zepto")))
               
               (:module "public"
                        :components ((:javascript-file "lib/backbone-model")
                                     (:javascript-file "lib/blame")
                                     (:javascript-file "lib/date")
                                     (:javascript-file "lib/util")
                                     (:javascript-file "lib/sc")))
               
               (:module "js"
                        :components ((:parenscript-file "macro")
                                     (:parenscript-file "mixin/buffer-scroll")
                                     (:parenscript-file "mixin/draggable")
                                     (:parenscript-file "mixin/highlight")
                                     
                                     (:parenscript-file "keyboard")
                                     (:parenscript-file "status")
                                     (:parenscript-file "manager")
                                     (:parenscript-file "key")
                                     
                                     (:parenscript-file "buffer/buckets")
                                     (:parenscript-file "buffer/dest")
                                     (:parenscript-file "buffer/search")
                                     (:parenscript-file "buffer/keys")
                                     (:parenscript-file "buffer")
                                     (:parenscript-file "data")
                                     (:parenscript-file "actions")
                                     (:parenscript-file "job")
                                     (:parenscript-file "search")
                                     (:parenscript-file "upload")
                                     (:parenscript-file "main"))))
  
  :depends-on (:cl-redis :zs3 :iterate :alexandria :split-sequence
                         :st-json :puri :cl-who :parenscript :restas :restas-directory-publisher
                         :hunchentoot-sessions :st-json :do-urlencode :postmodern :esrap
                         :react.paren :serve.paren :contracts.paren :plus.paren :mgl-pax
                         :bordeaux-threads))
