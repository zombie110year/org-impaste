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
;; Package-Requires: ((emacs "25.3"))
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

(setq org-impaste-storage-dir (file-truename "~/org-life/static/images/"))


(defun org-impaste-download (url)
  "Download images from internet, need input `URL'.
store image files into `org-impaste-storage-dir'"
  (interactive "simage url: ")
  (message "org-impaste downloaded: %s"
    (org-impaste--download-external url org-impaste-storage-dir)))

(provide 'org-impaste)
;;; org-impaste.el ends here
