;;; org-impaste.el --- Paste image to org mode -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 zombie110year
;;
;; Author: zombie110year <zombie110year@outlook.com>
;; Maintainer: zombie110year <zombie110year@outlook.com>
;; Created: 2022-06-23
;; Modified: 2022-06-23
;; Version: 0.0.1
;; Keywords: extensions files multimedia
;; Homepage: https://github.com/zombie110year/org-impaste
;; Package-Requires: ((emacs "28.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  Paste image to org mode
;;
;;; Code:

(require 'org)
;; Load from org-impaste DLL
(declare-function org-impaste--download-external "org-impaste")
(declare-function org-impaste--clipboard-external "org-impaste")
(declare-function org-impaste--random-key-external "org-impaste")
(declare-function rs-module/load "emacs-rs-module")

(module-load (file-truename "./emacs_rs_module.dll"))
(rs-module/load (file-truename"./target/debug/org_impaste.dll"))

(defgroup org-impaste nil
  "Paste image into orgmode from internet or clipboard or drag/drop."
  :group 'org
  :prefix "org-impaste-")

(defcustom org-impaste-storage-dir (file-truename "~/org/images/")
  "The directory to store all the image files."
  :type 'string)

(setq org-impaste-storage-dir (file-truename "./debug/"))

(defun org-impaste-download (url referer)
  "Download images from internet, need input `URL'.
store image files into `org-impaste-storage-dir'
and request by `REFERER', if it's not empty string.

While downloading, it will insert a placeholder like
=<org-impaste-download KEY>= under point.
When download finished, the placeholder will be replaced
by formatted link."
  (interactive "simage url: \nsreferer: ")
  (let* ((key (org-impaste--random-key-external))
         (placeholder (format "<org-impaste-download %s>" key)))
    (insert placeholder)
    (make-thread
     (lambda ()
       (let* ((impath (org-impaste--download-external
                      url org-impaste-storage-dir referer))
              (impath-r (file-relative-name impath buffer-file-name))
              (impath-s (format "[[file:%s]]" impath-r)))
         (replace-string-in-region
          (format "<org-impaste-download %s>" key) impath-s 1))))))


(provide 'org-impaste)
;;; org-impaste.el ends here
