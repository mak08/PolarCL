;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Description
;;; Author         Michael Kappert 2015
;;; Last Modified <michael 2020-02-08 01:18:58>

(in-package "POLARCL")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *mime-types*
  '(("gz"                       "application/gzip"            "GNU Zip-Dateien")
    ("xls xla"                  "application/msexcel"         "Microsoft Excel Dateien")
    ("ppt ppz pps pot"          "application/mspowerpoint"    "Microsoft Powerpoint Dateien")
    ("doc dot"                  "application/msword"          "Microsoft Word Dateien")
    ("bin exe com dll class"    "application/octet-stream"    "Nicht näher spezifizierte Daten, z.B. ausführbare Dateien")
    ("pdf"                      "application/pdf"             "PDF-Dateien")
    ("ai eps ps"                "application/postscript"      "PostScript-Dateien")
    ;; ("htm html shtml xhtml"     "application/xhtml+xml"       "XHTML-Dateien")
    ("svg"                      "image/svg+xml"             "???") 
    ("xml"                      "application/xml"             "XML-Dateien")
    ("zip"                      "application/zip"             "ZIP-Archivdateien")
    ("gif"                      "image/gif"                   "GIF-Dateien")
    ("jpeg jpg jpe"             "image/jpeg"                  "JPEG-Dateien")
    ("png"                      "image/png"                   "PNG-Dateien")
    ("ico"                      "image/x-icon"                "Icon-Dateien (z.B. Favoriten-Icons)")
    ("csv"                      "text/comma-separated-values" "kommaseparierte Datendateien")
    ("css"                      "text/css"                    "CSS Stylesheet-Dateien")
    ("htm html shtml"           "text/html"                   "HTML-Dateien")
    ("js"                       "text/javascript"             "JavaScript-Dateien")
    ("js"                       "application/x-javascript"    "serverseitige JavaScript-Dateien")
    ("txt"                      "text/plain"                  "reine Textdateien")
    ("tsv"                      "text/tab-separated-values"   "tabulator-separierte Datendateien")
    ("xml"                      "text/xml"                    "XML-Dateien")))

(defun get-mime-for-extension (ext)
  (or (cadr
       (find ext *mime-types* :key #'car :test #'search))
      "application/octet-stream"))

;;; EOF
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
