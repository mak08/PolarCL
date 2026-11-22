;;; After loading PolarCL, call (polarcl-head-tests:run-tests).
(defpackage :polarcl-head-tests
  (:use :cl)
  (:export :run-tests))
(in-package :polarcl-head-tests)

(defclass captured-output ()
  ((octets :reader octets
           :initform (make-array 0 :element-type '(unsigned-byte 8)
                                  :adjustable t :fill-pointer 0))))

(defmethod mbedtls:write-to-stream ((stream captured-output) (data vector))
  (map nil (lambda (byte) (vector-push-extend byte (octets stream))) data)
  (length data))

(defun serialize (request payload &optional (status "200") identity)
  (let ((response (polarcl::make-ok-response request))
        (stream (make-instance 'captured-output)))
    (when identity
      (setf (polarcl:http-header response :|Content-Encoding|) "identity"))
    (setf (polarcl:status-code response) status
          (polarcl:http-body response) payload)
    (polarcl::write-response stream response)
    (values (octets stream) response)))

(defun run-tests ()
  ;; Check the actual serialized bytes, for string and binary bodies, with and
  ;; without automatic gzip, and for success and error responses.
  (dolist (payload (list "" "hello" "Grüße" "€" "😀" (make-array 4 :element-type '(unsigned-byte 8)
                                           :initial-contents '(0 128 255 13))))
    (dolist (encoding '(nil "gzip" :identity))
      (dolist (status '("200" "403"))
        (let* ((headers (when encoding
                          (list (make-instance 'polarcl:http-header
                                 :name :|accept-encoding|
                                 :value (if (eq encoding :identity) "gzip" encoding)))))
               (get-request (polarcl:make-http-get :headers headers))
               (head-request (polarcl::make-http-head :headers headers)))
          (multiple-value-bind (get-bytes get-response)
              (serialize get-request payload status (eq encoding :identity))
            (multiple-value-bind (head-bytes head-response)
                (serialize head-request payload status (eq encoding :identity))
              (let* ((boundary #(13 10 13 10))
                     (header-end (+ 4 (or (search boundary get-bytes)
                                          (error "Missing header terminator"))))
                     (get-body (polarcl:http-body get-response))
                     (body-bytes (if (stringp get-body)
                                     (babel:string-to-octets get-body :encoding :utf-8)
                                     get-body)))
                (assert (= (length head-bytes) header-end))
                (assert (equalp head-bytes (subseq get-bytes 0 header-end)))
                (assert (= (length get-bytes) (+ header-end (length body-bytes))))
                (assert (equalp (subseq get-bytes header-end) body-bytes))
                (assert (= (polarcl:http-header get-response :|Content-Length|)
                           (length body-bytes)))
                (when (equal encoding "gzip")
                  (assert (equalp (zlib:gunzip body-bytes)
                                  (if (stringp payload)
                                      (babel:string-to-octets payload :encoding :utf-8)
                                      payload))))
                (when (and (stringp payload) (not (equal encoding "gzip")))
                  (assert (stringp get-body))
                  (assert (equal payload get-body)))
                (assert (= (polarcl:http-header head-response :|Content-Length|)
                           (polarcl:http-header get-response :|Content-Length|)))
                ;; Serialization must not erase the generated representation.
                (assert (equalp (polarcl:http-body head-response) get-body)))))))))
  (format t "~&PolarCL HEAD response tests passed.~%")
  t)
