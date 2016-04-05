(in-package :s3minder-web)

(defmacro concat (&rest args)
  `(concatenate 'string ,@args))

(connect-toplevel "s3m" "crackbot" "" "localhost")

(defclass puser ()
  ((id :accessor user-id :col-type serial)
   (email :reader email :col-type string :initarg :email) 
   (password :col-type string :initarg :password)
   (aws-access-key :col-type string :initarg :aws-access-key)
   (aws-secret-key :col-type string :initarg :aws-secret-key)
   (initial-sync :col-type boolean :col-default nil))
  
  (:metaclass dao-class)
  (:keys email))

(defparameter *tables* '(puser))

(defun sync-db ()
  (mapcar (lambda (table)
            (execute (dao-table-definition table)))
          *tables*))

(defun rebuild-db ()
  (mapcar (lambda (table)
            (query (:drop-table table)))
          *tables*)
  (sync-db))

(defun find-user-by-id (id)
  (car (select-dao 'puser (:= 'id id))))

(defun find-user-by-email (email)
  (car (select-dao 'puser (:= 'email email))))

(defun find-user (email password)
  (car (select-dao 'puser (:and (:= 'email email) (:= 'password password)))))

(defmethod s3-creds ((user puser))
  (list (slot-value user 'aws-access-key)
        (slot-value user 'aws-secret-key)))

(defmethod set-sync ((user puser) val)
  (setf (slot-value user 'initial-sync) val)
  (update-dao user))

(defmethod set-sync-not-in-progress ((user puser))
  (set-sync user nil))

(defmethod set-sync-in-progress ((user puser))
  (set-sync user t))

(defmethod user-keys ((user puser))
  "Get user owned hosts"
  (redis:with-connection ()
    (red:smembers (concat "user://" (hash-string (email user)) "/"))))

(defmethod user-owns-keys ((user puser) keys-str)
  (let* ((keys (mapcar #'s3minder::parse-key
                       keys-str))
         (user-keys (user-keys user))
         (table (build-lookup-hash user-keys :test 'equal)))
    (every #'(lambda (key)
               (gethash (s3minder::remove-trailing-slash (s3minder::key-authority key))
                        table))
           keys)))

(defmethod sync-key ((user puser) bucket key)
  (let ((s3m-key (s3minder::make-key (zs3:name bucket) (zs3:name key))))
    (setf (s3minder::key-meta s3m-key)
          (list :name (s3minder::key-name s3m-key)
                                        ; "kind"
                :size (zs3:size key)
                :owner "mikhail" ; (zs3:owner key)
                :lm (zs3:last-modified key)
                                        ; :etag (zs3:etag key)
                :sc (zs3:storage-class key)))
    (s3minder::save-key-redis s3m-key)))

(defmethod sync-bucket ((user puser) bucket)
  "Sync specific bucket"
  (let ((keys (zs3:all-keys bucket :credentials (s3-creds user)))
        (hashed-email (hash-string (email user))))
    
    (redis:with-connection ()
    
      ;; save all buckets
      (red:sadd (concat "user://" hashed-email "/")
                (concat "s3://" (zs3:name bucket)))
      
      ;; save keys
      (iter (for key in-vector keys)      
            (sync-key user bucket key)))))

(defmethod build-initial-index ((user puser))
  "Given s3 access data builds internal representation of s3 buckets
inside redis, those can be accessed later through api"
  (let ((buckets (zs3:all-buckets :credentials (s3-creds user))))
    (iter (for bucket in-vector buckets)
          (sync-bucket user bucket)))
  (sort-all-keys user)
  (set-sync-not-in-progress user))

(defmethod sort-all-keys ((user puser))
  (labels ((sort-list-recur (key)
             "Sort list named by key recursivley"
             (let* ((list-key key)
                    (children-keys (red:lrange list-key 0 (red:llen list-key))))
               (format t "sorting: ~A~%" list-key)
               (red:sort list-key :alpha t :store list-key)
               (mapcar #'(lambda (ckey)
                           (when (last-char-slash-p ckey)
                             (sort-list-recur (concat list-key ckey))))
                       children-keys))))
    (redis:with-connection ()
      (let* ((hashed-email (hash-string (email user)))
             (user-buckets (red:smembers (concat "user://" hashed-email "/"))))
        (mapcar #'(lambda (bucket)
                    (sort-list-recur (concat bucket "/")))
                user-buckets)))
    t))

(defmethod list-all-keys ((user puser))
  (redis:with-connection ()
    (labels ((process-key (bucket key)
               (format t "s3://~A/~A~%"
                      (zs3:name bucket)
                      (zs3:name key)))
               
             (process-bucket (bucket)
               (let ((keys (zs3:all-keys bucket :credentials (s3-creds user))))
                 ;; save all bucket names
                 ;; (red:sadd (concat "s3bucket:" (car s3)) (zs3:name bucket))
                 
                 ;; save keys
                 (iter (for key in-vector keys)
                       (process-key bucket key)))))
      (redis:with-connection ()
        (let ((buckets (zs3:all-buckets :credentials (s3-creds user))))
          (iter (for bucket in-vector buckets)
                (process-bucket bucket)))))))

(defun class-slots-list (cls)
  (iter (for slot in (closer-mop:class-direct-slots (class-of cls)))
        (when (slot-boundp cls (closer-mop:slot-definition-name slot))
          (appending (list (closer-mop:slot-definition-name slot) 
                           (slot-value cls (closer-mop:slot-definition-name slot)))))))

(defmethod dao-to-list (dao)
  (class-slots-list dao))
