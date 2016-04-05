
(in-package :s3minder-web)

(hts:create-redis-backend)

(defpssyslib :s3m "s3m" 
    :package :s3minder-web)

(mount-module static (#:restas.directory-publisher)
  (restas.directory-publisher:*directory* #P"~/dev/2014/s3minder/public"))

(mount-module parenscript (#:serve.paren-restas)
  (serve.paren::*parenscript-ending* "paren")
  (serve.paren::*route-prefix* "js/"))

(mount-module api (#:s3minder-api)
  (:decorators #'@requires-auth)
  (:url "/api/"))

(mount-module pages (#:s3minder-pages))

(defmethod hunchentoot:handle-request :before ((acceptor hunchentoot:acceptor)
                                               (request hunchentoot:request))
  (hunchentoot-sessions:start-session))

(restas:start 's3minder-web :port 8080)
