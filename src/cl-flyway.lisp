(in-package cl-flyway)

(deftable flyway-history ()
  ((version :col-type :text)
   (description :col-type :text)
   (start-time :col-type :timestamp)
   (end-time :col-type :timestamp))
  (:documentation "Core table that stores the operation history of data
migration."))

(defstruct action version description function)

(defun make-action-map ()
  "Return a hash map of version => ACTION instance."
  (make-hash-table :test #'equal))

(defun filter-actions (action-map max-version)
  "Remove any actions of COLLECTION-NAME in given ACTIONS-MAP whose version is
less than or equal to MAX-VERSION, then sort it by version."
  (check-type action-map hash-table)
  (sort (remove-if (lambda (x) (string<= (action-version x) max-version))
                   (hash-table-values action-map))
        (lambda (x y) (string<= x y)) :key #'action-version))

(defun setup (action-map &optional (connection mito:*connection*))
  "Setup the table and internal states. This function will *not* perform any
migration work.
CONNECTION is a connected database, default to current mito connection."
  (let ((mito:*connection* connection))
    (mito:ensure-table-exists 'flyway-history)
    (let* ((max-version (if-let (v (get-max-version)) v ""))
           (action-list (filter-actions action-map max-version)))
      (do-migration action-list))))

(defmacro defaction (action-map version description &body body)
  "Define a migration action for specific version.
VERSION is a string representing the target version of this action.
DESCRIPTION is the text description.
BODY is the body of lambda function that will be called."
  (with-gensyms (gaction-map gversion gdescription)
    `(let ((,gaction-map ,action-map)
           (,gversion ,version)
           (,gdescription ,description))
       (setf (gethash ,gversion ,gaction-map)
             (make-action :version ,gversion
                          :description ,gdescription
                          :function (lambda () ,@body))))))

(defun get-max-version (&optional (connection mito:*connection*))
  "Return the max version number in the table. If there is no record, return
NIL."
  (second
   (dbi:fetch
    (dbi:execute
     (dbi:prepare connection
                  "select max(version) from flyway_history")))))

(defun do-migration (actions)
  "For each action in ACTIONS list, run the related function."
  (dolist (action actions)
    (let (start-time end-time)
      (setf start-time (local-time:now))
      (funcall (action-function action))
      (setf end-time (local-time:now))
      (insert-dao (make-instance 'flyway-history
                                 :version (action-version action)
                                 :description (action-description action)
                                 :start-time start-time
                                 :end-time end-time)))))
