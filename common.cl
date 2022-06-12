;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description   
;;; Author         Michael Kappert 2019
;;; Last Modified <michael 2022-06-10 22:52:11>

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Common in all modules

(defparameter *source-root*
  (make-pathname :directory (pathname-directory #.*compile-file-truename*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Specific  to this module

(defvar *content-root* *source-root*)


(defvar *loop-delay* 2
  "Handler loops check for termination with this delay")

(defvar +html-prefix+ "
<html>
  <head>
    <style>
      td { text-align: right; }
      tr td: { text-align: right; }
    </style>
  </head>
  <body>")

(defvar +html-suffix+ "
</body>
</html>")

(defvar +filelist-prefix+ "
   <div id=filelist>
     <table>
       <thead>
          <th>Filename</th>
          <th>Type</th>
          <th>Size <small>(bytes)</small></th>
          <th>Date Modified</th>
        </tr>
      </thead>
      <tbody>")

(defvar +filelist-suffix+ "
      </tbody>
    </table>
  </div>")


;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
