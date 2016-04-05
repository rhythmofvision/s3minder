
(in-package :s3minder)

(defmacro concat (&rest args)
  `(concatenate 'string ,@args))

(defsection @key-main (:title "Keys")
  (@key-manual section)
  (@creating-keys section))
  ;(@key-actions section))

(defsection @key-manual (:title "Key format")
  "Key is an extension to URL, usualy URL format is the following
<scheme>://<login>:<password>@<host>:<port>/<url-path>?<params>#<fragment>

In case of s3 key it will look like this
s3://accesskey:@miraishome/path/to/key.txt

In case of dropbox
dropbox://@path/to/file.txt

In case of server file
server://127.0.0.1/path/to/key.txt")

(defsection @creating-keys (:title "Working with keys")
  (key class)
  (parse-key function)
  (make-key function)
  )
  ;(full-key (method () (t)))
  ;(full-key-path (method () (t))))

(defclass key ()
;;   "Key class is used to represent files sotred in different storage
;; systems, like s3, dropbox, google drive.

;; Its semantics is based on URI"
  ((scheme :initarg :scheme
           :initform nil
           :accessor key-scheme
           :documentation "Key scheme, the only supported right now is
           s3, future possible schemes are dropbox, server, ftp")
   (login :initarg :login
          :initform nil
          :accessor key-login
          :documentation "")
   (password :initarg :password
             :initform nil
             :accessor key-password)
   (bucket :initarg :bucket
           :initform nil
           :accessor key-bucket
           :documentation "For s3 scheme bucket represents the host of
           the URI")
   (path :initarg :path
         :initform nil
         :accessor key-path
         :documentation "For s3 scheme path represents the full key
         path, for example for a key like s3://hello/world/key.txt,
         the path will be world/key.txt")
   (name :initarg :name
         :initform nil
         :accessor key-name
         :documentation "key name is the last part of path when
         splitted by / sign, s3://hello/world/key.txt -> key.txt")
   (prefix :initarg :prefix
           :initform nil
           :accessor key-prefix
           :documentation "Prefix is the path without the key name,
           s3://hello/world/key.txt -> world/")
   (authority :initarg :authority
              :initform nil
              :accessor key-authority
              :documentation "Authiroty is anyting before path starts:
              s3://hello/world/key.txt -> s3://hello/")
   ;; (s3key :initarg :s3key :initform nil :accessor key-s3key)
   ;; (s3prefix :initarg :s3prefix :initform nil :accessor key-s3prefix)
   (query :initarg :query
          :initform nil
          :accessor key-query)
   (fragment :initarg :fragment
             :initform nil
             :accessor key-fragment)
   (meta :initarg :meta
         :initform nil
         :accessor key-meta))
  (:documentation "Key object"))

(defclass key-path (key)
  ()
  (:documentation ""))

(defclass key-file (key)
  ()
  (:documentation ""))

(defmethod initialize-instance :after ((key key-path) &key)
  "For path key set name in meta"
  (let* ((path-parts (split-sequence #\/ (key-prefix key)))
         (len (length path-parts)))
    (when (> len 1)
      (setf (slot-value key 'meta)
            (list :name (concat (nth (- len 2) path-parts)
                                "/"))))))

(defrule scheme
    (or "s3" "dropbox" "pc"))

(defrule post-scheme
    (and #\: #\/ #\/)
  (:constant t))

(defrule host
    (+ (not #\/))
  (:text t))

(defrule path
    (+ character)
  (:text t))

(defrule login
    (+ (alphanumericp character))
  (:text t))

(defrule password
    (+ (alphanumericp character))
  (:text t))

(defrule host-only
    host
  (:function (lambda (res)
               (list res nil nil))))

(defrule host-with-path
    (and host #\/ (? path)))

(defrule uri
    (and scheme post-scheme
         (? (or (and login #\: password #\@)
                (and login #\: #\@)
                (and #\: password #\@)))
         (or host-with-path host-only)))
         
         ;; host))
         ;; #\/
         ;; path))

(defun key-meta-json (key)
  "Return json string by encoding key meta plist"
  (meta-json (key-meta key)))

(defun parse-key (thing)
  "Parse string into key instance"
  ;(declare (ignore junk1 junk2))
  (let (login password)
    (destructuring-bind (scheme junk1 auth bucket-and-path)
        (parse 'uri thing)
      (declare (ignore junk1 auth))
      (destructuring-bind (bucket junk2 path)
          bucket-and-path
        (declare (ignore junk2))
        (let* ((splitted-path (split-sequence:split-sequence #\/ path))
               (name (car (last splitted-path)))
               (class (if (eq (length name) 0) 'key-path 'key-file))
               (prefix (join-prefix (except-last splitted-path)))
               (instance (make-instance class
                                        :scheme scheme
                                        :bucket bucket
                                        ;; :version port
                                        :path path
                                        :prefix (or (and (> (length prefix) 0) prefix)
                                                    nil)
                                        ;:file file
                                        :name name
                                        :login login
                                        :password password
                                        ;; :prefix prefix
                                        ;; :query query
                                        ;; :fragment fragment
                                        ;; :escaped escape
                                      )))
          (setf (key-authority instance)
                (with-output-to-string (out)
                  (print-key instance out :print-key nil :print-prefix nil)))

        ;; (setf (key-meta instance)
        ;;       (list :size 0
        ;;             :name (or name 
        ;;             ))
        
          instance)))))

(defun find-key (str)
  (let ((key (parse-key str)))
    (fetch-keys-meta (list key))
    key))

(defmethod print-object ((key key) stream)
  "Print the key object"
  (format stream "[KEY] ")
  (print-key key stream))

(defun print-key (key stream &key (print-key t) (print-prefix t) (print-scheme t))
  "General method for printing the key, look at render-key, full-key,
full-key-path instead"
  (when print-scheme
    (format stream "~A://" (key-scheme key)))
  (when (key-login key)
    (format stream "~A:" (key-login key)))
  (when (key-password key)
    (format stream "~A" (key-password key)))
  (when (or (key-login key) (key-password key))
    (format stream "@"))
  (format stream "~A/" (key-bucket key))
  (when (and print-prefix (key-prefix key))
      (format stream "~A" (key-prefix key)))
  (when (and print-key (key-name key))
    (format stream "~A" (key-name key))))


(defmethod serialize ((key key))
  (setf (getf (key-meta key) :ukey) (full-key key))
  (apply #'jso
         (mapplist #'(lambda (name value)
                       (values (to-string name) value))
                   (key-meta key))))

(defun serialize-keys (keys)
  (mapcar #'serialize keys))

(defmethod key-pathp ((key key))
  "Check if key is really a path, an s3 path should include bucket
name and prefix, but no key name"
  (and (key-prefix key) (eq (length (key-name key))
                            0)))

(defun clone-key (key)
  "Copy key, not very performant but works"
  (parse-key (full-key key)))

(defun make-key (bucket path &key (scheme "s3"))
  "Utility function to construct the key from bucket and path, scheme
key is optional and by default set to the only supported scheme at the
moment - s3"
  (let ((key-str (concat scheme "://" bucket "/")))
    (when (> (length path) 0)
       (setf key-str (concat key-str path)))
    (parse-key key-str)))

(defmethod full-key ((key key))
  "Return string representing the whole key structure, so if you parse
key like s3://bucket/hello/world.txt that't exactly what you will get
back"
  (with-output-to-string (out)
    (print-key key out)))

(defmethod full-key-path ((key key))
  "Same as full-key but it doesnt unclude key name, for example
s3://bucket/hello/world.txt becomes s3://bucket/hello/"
  (with-output-to-string (out)
    (print-key key out :print-key nil)))

(defmethod full-key-as-path-key ((key key))
  "Same as full-key-path but no trailing slash, which makes a meta key for paths"
  (let ((key-str (with-output-to-string (out)
                   (print-key key out :print-key nil))))
    (subseq key-str 0 (- (length key-str) 1))))

;;;; Actions on individual keys
;; SAVE
;; MOVE 
;; COPY 
;; DELETE

(defsection @key-actions (:title "Key actions")
  (copy-key function)
  (move-key function)
  (delete-key function))

(defun save-path (key prefixes)
  (let ((path-name (concat (car (last prefixes)) "/")))
    
    ;; (format t "native meta: ~A~%" (key-meta key))
    (red:set (concat (key-authority key)
                     (join-prefix prefixes :trailing nil))
             (alist-meta-json (key-meta key)))
    
    (red:lpush (concat (key-authority key)
                       (join-prefix (except-last prefixes)))
               path-name)))

(defun split-key (key)
  "Given a key split it into (bucket path-part1 ... path-partN), for
example s3://karl-and-olga/public/images/table.png will become
'(karl-and-olga public images)"
  (with-slots (bucket prefix) key
    (cons bucket (except-last (split-sequence #\/ prefix)))))

;; [TODO] for now copy key meta

(defun parent-keys-from-key (key &key (fetch-meta nil))
  "Given a key build all associated parent keys"
  (mapnlst #'(lambda (props)
               (make-key (car props)
                         (join-prefix (cdr props))))
           (split-key key)))

(defun direct-parent-key (key)
  "Given a key will build only a direct parent key"
  (if (plusp (length (key-name key)))
      (values (make-key (key-bucket key) (key-prefix key))
              (key-name key))
      (let* ((prefix-parts (split-sequence #\/ (key-prefix key)))
             (len (length prefix-parts)))
        (when (> len 1)
          (values (make-key (key-bucket key)
                            (join-prefix (subseq prefix-parts 0 (- len 2))))
                  (concat (nth (- len 2) prefix-parts) "/"))))))

(defun parent-relations-from-key (key)
  "Give a key builds an alist of relations, for example given s3://bucket/path/to/file.png will return you an alist like this (([KEY] s3://bucket/path/to/ . file.png)
                      ([KEY] s3://bucket/path/ to/)
                      ([KEY] s3://bucket/ path/))"
  (let* ((lst (split-key key))
         (res '())
         (len (length lst)))
    (do ((i 2 (1+ i)))
        ((> i len) nil)
      (let ((props (subseq lst 0 i)))
        (push (cons (make-key (car props) (join-prefix (except-last (cdr props))))
                    (concat (car (last props)) "/")) res)))
    
    (when (> (length (key-name key)) 0)
      (push (cons (make-key (key-bucket key) (key-prefix key))
                  (key-name key))
            res))
    (nreverse res)))

(defun update-parent-key-meta (key parent-key)
  "Given a parent-key and it's child key update the parent-key based
on child meta, for example given s3://bucket/some/ and s3://bucket/some/key.png"
  (let* ((parent-meta (key-meta parent-key))
         (child-meta (key-meta key))
         (new-parent (clone-key parent-key))
         (new-meta (key-meta new-parent)))
    (setf (getf new-meta :size)
          (+ (or (getf parent-meta :size) 0) (getf child-meta :size)))
    (setf (key-meta new-parent) new-meta)
    new-parent))

(defun build-parent-keys (key)
  "Give a key like s3://karl-and-olga/public/images/table.png, will
build all parent keys with correct metadata, returning the list of parent keys"
  ;; we iterate through all keys parts building parent keys first
  ;; then we fetch metadata for those and return the result
  (mapcar (curry #'update-parent-key-meta key)
          (parent-keys-from-key key)))

(defun build-relation-keys-fn (key)
  "Given a key this function returns a function which in turn when
  called with initiated redis connection will buildup relation between
  bucket path(s) and file, checking for duplicates in db"
  (lambda (parent-keys)
    (when-let ((relations (parent-relations-from-key key)))

      (let* ((lookup-keys (append (mapcar #'full-key-as-path-key parent-keys)
                                  (list (full-key key))))
             (lookup-res (apply #'red:mget lookup-keys))
             (lookup-alist (pairlis lookup-keys lookup-res)))

        (dolist (rel relations)
          (let* ((rel-parent-key (full-key (car rel)))
                 (tmp-key (concat rel-parent-key (remove-trailing-slash (cdr rel)))))
            
            (when (not (cdr (assoc tmp-key lookup-alist :test #'string=)))
              (red:lpush rel-parent-key
                         (cdr rel)))))))))
  
(defun keys-to-meta-plist (keys)
  "Given list of keys convert it to a plist with structure of key
string -> key meta json"
  (let ((to-set '()))
    (dolist (pkey keys)
      (setf (getf to-set (full-key-as-path-key pkey))
            (key-meta-json pkey)))
    to-set))

(defun tree-iter-bottom-up (fun tree)
  "Given tree iterate it bottom up calling fun on each node")

(defun build-tree (keys)
  "Given keys in some random order construct a tree representing the
relationship between the keys"
  
  )

(defmethod save-key-redis ((key key-path))
  "Save path key into redis database"
  (let* ((parent-keys (parent-keys-from-key key))
         (relations-fn (build-relation-keys-fn key)))

    ;; [TODO] move to some other place?
    (mapcar #'(lambda (pkey)
                (let ((meta (key-meta pkey)))
                  (setf (getf meta :size) 0)
                  (setf (key-meta pkey) meta)))
            parent-keys)
    
    (redis:with-connection ()
      (funcall relations-fn parent-keys)
      (apply #'red:mset (keys-to-meta-plist parent-keys)))))

(defmethod save-key-redis ((key key-file))
  "Saves key record into redis database"
  (let* ((parent-keys (build-parent-keys key))
         (relations-fn (build-relation-keys-fn key))
         (to-set (append (list (full-key key) (key-meta-json key))
                         (keys-to-meta-plist parent-keys))))
    (redis:with-connection ()
      (funcall relations-fn parent-keys)
      (apply #'red:mset to-set))))

(defun fetch-keys-meta (keys)
  (let* ((lookup-keys (mapcar #'remove-trailing-slash
                              (mapcar #'full-key keys)))
         (meta-data (apply #'red:mget lookup-keys)))
    (mapcar #'(lambda (lst)
                (let ((key (car lst)))
                  (setf (slot-value key 'meta) (json-meta-to-plist (cdr lst)))
                  key))
            (pairlis keys meta-data))))

(defun update-parent-sizes (key)
  (let* ((full-key (remove-trailing-slash (full-key key)))
         (key-meta-data (read-json (red:get full-key)))
         (key-file-size (getjso "size" key-meta-data))
         (parent-keys (fetch-keys-meta (parent-keys-from-key (parse-key full-key)))))
    (dolist (pkey parent-keys)
      (setf (getf (key-meta pkey) :size)
            (- (getf (key-meta pkey) :size) key-file-size)))
    (lambda ()
      (apply #'red:mset (keys-to-meta-plist parent-keys)))))

(defun get-all-key-children (key)
  "Given key path it will return two values first one is usuall keys
second one is list type keys"
  ;; this is done because after parse-key you won't be able to tell
  ;; the difference between s3://bucket/path/ and s3://bucket/path but
  ;; on the redis level that's the difference between list type and
  ;; usuall get/set key
  (let ((lst-type '())
        (get-type '())
        (keys (red:keys (concat (remove-trailing-slash (full-key-path key)) "*"))))
    (dolist (k keys)
      (if (last-char-slash-p k)
          (push k lst-type)
          (push k get-type)))
    (values (mapcar #'parse-key get-type)
            (mapcar #'parse-key lst-type))))

(defmethod exists-s3-p ((key key-file))
  (handler-case (zs3:get-object (key-bucket key) (key-path key))
    ((or zs3:no-such-bucket zs3:no-such-key)
        nil)
    (:no-error (data meta)
      (declare (ignore data meta))
      t)))

(defmethod delete-key-s3 ((key key-file))
  (zs3:delete-object (key-bucket key)
                     (key-path key)))

(defmethod delete-key-s3 ((key key-path))
  (multiple-value-bind (keys paths)
      (get-all-key-children key)
    (declare (ignore paths))
    ;; [TODO] this is a hack but may work!
    (dolist (k (sort keys #'(lambda (a b)
                              (> (length (full-key a))
                                 (length (full-key b))))))

      (zs3:delete-object (key-bucket k)
                         (key-path k))
      
      ;; [TODO] remove that one
      (zs3:delete-object (key-bucket k)
                         (concat (key-path k) "/")))))    
    ;; (declare (ignore paths))
    ;; (tree-iter-bottom-up #'(lambda (key)
    ;;                          (zs3:delete-object (key-bucket key)
    ;;                                             (key-path key)))
    ;;                      (build-tree keys))))

(defmethod delete-key-redis ((key key-path))
  "Delete path from redis database"
  ;; can update parents easily
  (redis:with-connection ()
    (let ((update-fn (update-parent-sizes key)))
      (multiple-value-bind (get-type lst-type)
          (get-all-key-children key)
        (redis:with-pipelining
          (funcall update-fn)
          (apply #'red:del (append lst-type get-type))
          
          (multiple-value-bind (parent stripped)
              (direct-parent-key key)
            (red:lrem (full-key parent) 0 stripped))
          
          (red:del (full-key key)))))))

(defmethod delete-key-redis ((key key-file))
  "Delete key from redis database, key-file means a key has a name, as
opposed to key-path"
  ;; need to fetch key meta data to be able to update parent paths
  ;; sizes
  (redis:with-connection ()
    (let ((update-fn (update-parent-sizes key)))
      (redis:with-pipelining
        ;; update redis
        (funcall update-fn)
        (red:lrem (full-key-path key) 0 (key-name key))
        (red:del (full-key key))))))

(defmethod delete-key ((key key) &aux (ok-status 204))
  ;; [TODO] check if success 
  (delete-key-s3 key)
  (delete-key-redis key))

;; [TODO] issue: check when key has empty subdirs!
(defmethod copy-key-s3 ((dest-key key-path) (key key-path))
  "Copy dir on s3"
  (multiple-value-bind (keys paths)
      (get-all-key-children key)
    (declare (ignore paths))

    ;(break)
    (dolist (k (sort keys (lambda (a b) (> (length a) (length b)))))
      (let* ((diff (s3minder-util::string-diff (full-key key)
                                              (full-key k)))
             (path-parts (split-sequence #\/ (remove-trailing-slash (key-prefix key))))
             (copy-path (car (last path-parts)))
             (to-key (concat (key-path dest-key) copy-path))
             (to-key-full (or (and (plusp (length to-key)) (concat to-key "/" diff))
                              diff)))
        (zs3:copy-object :from-bucket (key-bucket key)
                         :to-bucket (key-bucket dest-key)
                         :from-key (key-path k) 
                         :to-key to-key-full)))))

(defmethod copy-key-s3 ((dest-key key-path) (key key-file))
  "Copy key on s3"
  ;; s3://test-bucket-cHaF-1/tt/
  ;; s3://test-bucket-cHaF-3/test3/subpath3/file3.txt
  ;; s3://test-bucket-cHaF-1/tt/file3.txt
  (let ((to-key (concat (key-path dest-key) (key-name key))))
    (zs3:copy-object :from-bucket (key-bucket key)
                     :to-bucket (key-bucket dest-key)
                     :from-key (key-path key) 
                     :to-key to-key)))

;; [TODO]
(defmethod copy-key-redis ((dest-key key-path) (key key-path))
  "Copy entire directory inside redis"
  (multiple-value-bind (keys paths)
      (get-all-key-children key)
    (declare (ignore paths))
    (iter:iter (iter:for k iter:in (fetch-keys-meta keys))
      (let* ((diff (s3minder-util::string-diff (full-key key)
                                              (full-key k)))
             (path-parts (split-sequence #\/ (remove-trailing-slash (key-prefix key))))
             (copy-path (car (last path-parts)))
             (to-key (concat (key-path dest-key) copy-path))
             (to-key-full (or (and (plusp (length to-key)) (concat to-key "/" diff))
                              diff))
             (new-key (make-key (key-bucket dest-key)
                                to-key-full)))
        
        (setf (key-meta new-key) (key-meta k))
        (break)
        (save-key-redis new-key)
        (iter:collect new-key)))))

(defmethod copy-key-redis ((dest-key key-path) (key key-file))
  "Copy key inside redis"
  ;; dest = "s3://bucket/hello/path/"
  ;; key  = "s3://bucket/some/other/image.png"
  (let ((new-key (make-key (key-bucket dest-key)
                           (concat (key-prefix dest-key) (key-name key)))))
    (setf (key-meta new-key) (key-meta key))

   ; (break)
    
    (save-key-redis new-key)
    new-key))

(defmethod copy-key ((dest-key key)
                     (key key) &aux (ok-status 204))
  (copy-key-s3 dest-key key)
  
  ;; (let ((res (zs3::http-code )))
  ;;   (when (or (eq res ok-status)
  ;;             (eq res 200))
  (copy-key-redis dest-key key))

(defmethod move-key ((dest-key key) (key key))
  "Move key to dest key, copying the object this key represents and
metadata"
  (when-let ((res (copy-key dest-key key)))
    (delete-key key)))

(defmethod create-bkt ((key key) &aux (ok-status 204))
  "Create bucket"
  (when (eq (zs3::http-code (zs3:create-bucket (key-bucket key)))
            ok-status)
    (save-key-redis key)))
