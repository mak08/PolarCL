;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    HTTP-date handling
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2020-02-11 22:26:31>

(in-package :polarcl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP-date see https://tools.ietf.org/html/rfc7231#section-7.1.1.1

(defparameter +day-name+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter +month-name+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")) 
  
(defmethod format-date-imf (stream (time integer))
  (multiple-value-bind (sec min hour day month year weekday daylight-p zone)
      (decode-universal-time time 0)
    (declare (ignore daylight-p zone))
    (format stream "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT"
            (aref +day-name+ weekday)
            day
            (aref +month-name+ (1- month))
            year
            hour
            min
            sec)))

(defmethod format-date-imf (stream (timestamp local-time:timestamp))
  (let ((timezone local-time:+utc-zone+))
    (format stream "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT"
            (aref +day-name+ (1- (local-time:timestamp-day-of-week timestamp :timezone timezone)))
            (local-time:timestamp-day timestamp :timezone timezone)
            (aref +month-name+ (1- (local-time:timestamp-month timestamp :timezone timezone)))
            (local-time:timestamp-year timestamp :timezone timezone)
            (local-time:timestamp-hour timestamp :timezone timezone)
            (local-time:timestamp-minute timestamp :timezone timezone)
            (local-time:timestamp-second timestamp :timezone timezone))))

(defun parse-date (datetime)
  (destructuring-bind (garbage date)
      (cl-utilities:split-sequence #\, datetime)
    (destructuring-bind (day month year time zone)
        (cl-utilities:split-sequence #\space date :remove-empty-subseqs t)
      (destructuring-bind (hour min sec)
          (cl-utilities:split-sequence #\: time)
        (encode-universal-time (read-from-string sec) 
                               (read-from-string min)
                               (read-from-string hour)
                               (read-from-string day)
                               (1+ (position month +month-name+ :test #'string=))
                               (read-from-string year)
                               0)))))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
