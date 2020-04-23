;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2020
;;; Last Modified <michael 2020-02-14 18:44:29>

;;; https://tools.ietf.org/html/rfc2109

(in-package :polarcl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct cookie
  name value path domain
  expires
  (max-age (* 24 60 60 30))
  (options '("Secure" "HttpOnly")))

(defmethod print-object ((object cookie) stream)
  (format stream "~a=~a;~@[Expires=~a;~]~@[Max-Age=~a;~]~{~a~^;~}"
          (cookie-name object)
          (cookie-value object)
          (when (cookie-expires object)
            (format-date-imf nil (cookie-expires object)))
          (cookie-max-age object)
          (cookie-options object)))

(defgeneric get-cookie (r name))
(defmethod  get-cookie ((r http-request) name)
  (let ((cookies (find  :|cookie| (headers r) :key #'field-name)))
    (when cookies
      (setf *r* r)
      (find name (field-value cookies) :key #'cookie-name :test #'string=))))

(defgeneric set-cookie (r name value &key expires max-age options))
(defmethod set-cookie ((r http-request) name value &key expires max-age options)
  (error "NYI"))
(defmethod set-cookie ((r http-response) name value  &key expires
                                                       (max-age  (* 24 60 60 30))
                                                       (options '("Secure" "HttpOnly")))
  (let ((new-header (make-instance 'http-header
                                   :name :|Set-Cookie|
                                   :value (make-cookie :name name :value value
                                                       :expires expires
                                                       :max-age max-age
                                                       :options options))))
    (push new-header (headers r))))


(defun parse-cookies (string)
  (let ((parts (cl-utilities:split-sequence #\; string))
        cookies
        name value path domain)
    (dolist (part parts)
      (destructuring-bind (key &optional val)
          (cl-utilities:split-sequence #\= part)
        (setf key (string-trim " " key))
        (cond
          ((string= key "$Path")
           (setf path (string-trim " " val)))
          ((string= key "$Domain")
           (setf domain (string-trim " " val)))
          ((string= key "$Version"))
          (t
           (when name
             (push (make-cookie :name name
                                :value value
                                :path path
                                :domain domain)
                   cookies))
           (setf name key)
           (setf value (string-trim " " val))))))
    (when name
      (push
       (make-cookie :name name
                    :value value
                    :path path
                    :domain domain)
       cookies))
    cookies))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
