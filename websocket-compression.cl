(in-package :polarcl)

(defun ws-extension-parts (text separator)
  "Split extension grammar outside quoted strings; NIL for malformed quoting."
  (let ((quoted nil) (escaped nil) (start 0) (parts nil))
    (loop for char across text for i from 0 do
      (cond (escaped (setf escaped nil))
            ((and quoted (char= char #\\)) (setf escaped t))
            ((char= char #\") (setf quoted (not quoted)))
            ((and (not quoted) (char= char separator))
             (push (string-trim '(#\Space #\Tab) (subseq text start i)) parts)
             (setf start (1+ i)))))
    (unless (or quoted escaped)
      (nreverse (cons (string-trim '(#\Space #\Tab) (subseq text start)) parts)))))

(defun ws-extension-value (text)
  (if (and (> (length text) 1) (char= (char text 0) #\")
           (char= (char text (1- (length text))) #\"))
      (let ((value (with-output-to-string (out)
                     (loop for i from 1 below (1- (length text)) do
                       (when (char= (char text i) #\\) (incf i))
                       (when (>= i (1- (length text))) (return-from ws-extension-value nil))
                       (write-char (char text i) out)))))
        (when (ws-token-p value) value))
      (when (ws-token-p text) text)))

(defun ws-deflate-offer (offer)
  (let* ((parts (ws-extension-parts offer #\;)) (parameters nil))
    (unless (equal "permessage-deflate" (first parts)) (return-from ws-deflate-offer nil))
    (dolist (part (rest parts))
      (let* ((equals (position #\= part))
             (name (string-trim '(#\Space #\Tab) (subseq part 0 equals)))
             (value (when equals (ws-extension-value (string-trim '(#\Space #\Tab) (subseq part (1+ equals)))))))
        (unless (and (ws-token-p name) (not (assoc name parameters :test #'string=))
                     (or (null equals) value))
          (return-from ws-deflate-offer nil))
        (cond ((member name '("server_no_context_takeover" "client_no_context_takeover") :test #'string=)
               (when equals (return-from ws-deflate-offer nil)))
              ((string= name "server_max_window_bits")
               (unless (equal value "15") (return-from ws-deflate-offer nil)))
              ((string= name "client_max_window_bits")
               (unless (or (null equals) (equal value "15")) (return-from ws-deflate-offer nil)))
              (t (return-from ws-deflate-offer nil)))
        (push (cons name value) parameters)))
    (concatenate 'string "permessage-deflate; server_no_context_takeover; client_no_context_takeover"
                 (if (assoc "server_max_window_bits" parameters :test #'string=) "; server_max_window_bits=15" "")
                 (if (assoc "client_max_window_bits" parameters :test #'string=) "; client_max_window_bits=15" ""))))

(defun ws-negotiate-deflate (request)
  (loop for header in (ws-header-values request "Sec-WebSocket-Extensions") do
    (loop for offer in (ws-extension-parts header #\,) for answer = (ws-deflate-offer offer)
          when answer do (return-from ws-negotiate-deflate answer))))

(defun ws-deflate-message (bytes)
  "Return wire payload and whether compression saved bytes. No shared context."
  (handler-case
      (zlib:with-codec (encoder (zlib:make-deflater :max-output (+ (length bytes) 3)))
        (let ((compressed (zlib:deflate-bytes encoder bytes)))
          (unless (and (>= (length compressed) 4)
                       (equalp #(0 0 255 255) (subseq compressed (- (length compressed) 4))))
            (error "zlib did not produce a sync-flush trailer."))
          (values (subseq compressed 0 (- (length compressed) 4)) t)))
    (zlib:zlib-output-limit () (values bytes nil))))

(defun ws-inflate-message (bytes limit)
  "Decode one RFC 7692 message with independent history and bounded output."
  (handler-case
      (zlib:with-codec (decoder (zlib:make-inflater :max-output limit))
        (let ((offset 0) (parts nil) (size 0))
          ;; RFC 7692 permits BFINAL blocks inside a message, followed by padded
          ;; blocks. Retain their history and cumulative output budget on restart.
          (loop
            (multiple-value-bind (output status consumed)
                (zlib:inflate-bytes decoder bytes :start offset :allow-trailing t)
              (push output parts) (incf size (length output)) (incf offset consumed)
              (unless (eq status :finished) (return))
              (zlib:restart-inflater decoder)
              (when (= offset (length bytes)) (return))))
          ;; Do not turn a truncated stream into accepted application data by
          ;; blindly appending the omitted four bytes to an arbitrary decoder state.
          (unless (zlib:inflater-sync-point-p decoder) (ws-fail 1002 "Invalid DEFLATE message boundary"))
          (let ((tail (zlib:inflate-bytes decoder (make-array 4 :element-type '(unsigned-byte 8)
                                                             :initial-contents '(0 0 255 255)))))
            (unless (zerop (length tail)) (ws-fail 1002 "Invalid DEFLATE flush block")))
          (let ((result (ws-bytes size)) (position 0))
            (dolist (part (nreverse parts)) (replace result part :start1 position) (incf position (length part)))
            result)))
    (zlib:zlib-output-limit () (ws-fail 1009 "Decompressed message exceeds configured limit"))
    (zlib:zlib-error () (ws-fail 1002 "Invalid DEFLATE message"))))
