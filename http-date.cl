;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description    HTTP-date handling
;;; Author         Michael Kappert 2017
;;; Last Modified <michael 2017-12-29 21:44:03>

(in-package :polarcl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HTTP-date see https://tools.ietf.org/html/rfc7231#section-7.1.1.1

(defvar +day-name+
  #("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defvar +month-name+
  #("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov" "Dec")) 
  
(defun format-date (time)
  (multiple-value-bind (sec min hour day month year weekday daylight-p zone)
      (decode-universal-time time 0)
    (declare (ignore daylight-p zone))
    (format nil "~a, ~2,'0d ~a ~a ~2,'0d:~2,'0d:~2,'0d GMT"
            (aref +day-name+ weekday)
            day
            (aref +month-name+ (1- month))
            year
            hour
            min
            sec)))

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
