(in-package :polarcl-websocket-tests)

(defun compressed-frame (opcode payload &optional (final t))
  (let ((frame (masked-frame opcode payload final)))
    (setf (aref frame 0) (logior 64 (aref frame 0))) frame))
(defun compressed-decode (data &key (limit 1048576) (wire-limit 1048576) (enabled t))
  (let ((decoder (polarcl::make-ws-decoder :compression enabled :max-size limit :max-compressed-size wire-limit))
        (messages nil))
    (loop for byte across data do
      (polarcl::ws-feed decoder (bytes byte) (lambda (opcode payload) (push (cons opcode payload) messages))))
    (nreverse messages)))
(defun test-compression ()
  (dolist (offer '("permessage-deflate" "permessage-deflate; client_max_window_bits"
                   "permessage-deflate; server_max_window_bits=15; client_max_window_bits=\"15\""
                   "permessage-deflate; server_no_context_takeover; client_no_context_takeover"))
    (let ((answer (polarcl::ws-deflate-offer offer)))
      (assert (search "server_no_context_takeover" answer))
      (assert (search "client_no_context_takeover" answer))))
  (dolist (offer '("other" "permessage-deflate;" "permessage-deflate; mystery"
                   "permessage-deflate; client_no_context_takeover=1"
                   "permessage-deflate; client_max_window_bits=7" "permessage-deflate; server_max_window_bits=14"
                   "permessage-deflate; server_max_window_bits" "permessage-deflate; server_max_window_bits=015"
                   "permessage-deflate; client_max_window_bits; client_max_window_bits"
                   "permessage-deflate; client_max_window_bits=\"15"))
    (assert (null (polarcl::ws-deflate-offer offer))))
  (let ((request (handshake-request)))
    (setf (polarcl:http-header request "Sec-WebSocket-Extensions")
          "unknown; value=\"a,b;c\", permessage-deflate; server_max_window_bits=10, permessage-deflate")
    (assert (polarcl::ws-negotiate-deflate request)))
  ;; Published RFC 7692 examples, including BFINAL=1 followed by an empty block.
  (dolist (wire (list (bytes #xf2 #x48 #xcd #xc9 #xc9 #x07 #x00)
                     (bytes #xf3 #x48 #xcd #xc9 #xc9 #x07 #x00 #x00)))
    (assert (equalp #(72 101 108 108 111) (polarcl::ws-inflate-message wire 5)))
    (assert (protocol-error-p 1009 (lambda () (polarcl::ws-inflate-message wire 4)))))
  ;; Python zlib fixture: final block followed by a block referencing its history.
  (assert (equalp (polarcl::ws-text (apply #'concatenate 'string (make-list 200 :initial-element "Hello")))
                 (polarcl::ws-inflate-message
                  (bytes 243 72 205 201 201 247 24 37 70 18 1 0 26 37 70 30 1 0) 1000)))
  (assert (zerop (length (polarcl::ws-inflate-message (bytes 0) 0))))
  (dolist (wire (list (bytes) (bytes 7) (bytes #xf2 #x48) (bytes #xf2 #x48 #xcd #xc9 #xc9 #x07)))
    (assert (protocol-error-p 1002 (lambda () (polarcl::ws-inflate-message wire 100)))))
  (let* ((text (make-string 2000 :initial-element #\a)) (payload (polarcl::ws-text text)))
    (multiple-value-bind (wire compressed) (polarcl::ws-deflate-message payload)
      (assert compressed) (assert (< (length wire) (length payload)))
      ;; Each message can be independently decoded; no dictionary carry-over.
      (dotimes (i 2) (assert (equalp payload (polarcl::ws-inflate-message wire (length payload)))))
      (let ((messages (compressed-decode
                       (concatenate '(vector (unsigned-byte 8))
                         (compressed-frame 1 (subseq wire 0 2) nil)
                         (masked-frame 9 (bytes 42))
                         (masked-frame 0 (subseq wire 2))
                         (masked-frame 1 (polarcl::ws-text "plain"))))))
        (assert (= 9 (caar messages)))
        (assert (equal text (polarcl::ws-utf8 (cdr (second messages)))))
        (assert (equal "plain" (polarcl::ws-utf8 (cdr (third messages))))))
      (assert (protocol-error-p 1002 (lambda () (compressed-decode (compressed-frame 1 wire) :enabled nil))))
      (assert (protocol-error-p 1009 (lambda () (compressed-decode (compressed-frame 1 wire) :limit 100))))
      (assert (protocol-error-p 1009 (lambda () (compressed-decode (compressed-frame 1 wire) :wire-limit 2))))
      (assert (protocol-error-p 1002
                (lambda () (compressed-decode (concatenate '(vector (unsigned-byte 8))
                                                (compressed-frame 1 (subseq wire 0 2) nil)
                                                (compressed-frame 0 (subseq wire 2)))))))))
  (dolist (opcode '(8 9 10))
    (assert (protocol-error-p 1002 (lambda () (compressed-decode (compressed-frame opcode (bytes)))))))
  (multiple-value-bind (wire compressed) (polarcl::ws-deflate-message (bytes 1 2 3))
    (assert (not compressed)) (assert (equalp #(1 2 3) wire)))
  (format t "~&WebSocket DEFLATE negotiation, framing, boundaries and size limits passed.~%"))
