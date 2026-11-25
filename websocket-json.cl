(in-package :polarcl)

(defun ws-json-depth-check (text limit)
  ;; Bound recursion before invoking a parser, including application adapters.
  (let ((depth 0) (quoted nil) (escaped nil))
    (loop for char across text do
      (cond (escaped (setf escaped nil))
            ((and quoted (char= char #\\)) (setf escaped t))
            ((char= char #\") (setf quoted (not quoted)))
            ((not quoted)
             (when (and (< (char-code char) 32) (not (find char '(#\Tab #\Newline #\Return))))
               (error "Invalid JSON whitespace."))
             (case char ((#\{ #\[) (incf depth)) ((#\} #\]) (decf depth)))
             (when (> depth limit) (error "JSON nesting limit exceeded.")))))))

(defun ws-json-read (text)
  ;; No START argument: MAKROS rejects trailing values by default.
  (macros:parse-json text))

(defun ws-json-write (value)
  ;; Use the same generic serializer and application methods as Bitsailor.
  (with-output-to-string (stream) (macros:json stream value)))
