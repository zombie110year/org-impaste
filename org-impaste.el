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

;; todo build a package which can be installed by use-package.

;; Load from org-impaste DLL
(declare-function org-impaste--download-external "org-impaste")
(declare-function org-impaste--clipboard-external "org-impaste")
(declare-function org-impaste--timer-key-external "org-impaste")

;; todo auto fetch x86_64 windows/linux/osx shared library from github release (CI build)
;;      or auto compile on other platform.
(defun org-impaste--init ()
  "Init.

The shared library should place on the same directory with org-impaste.el"
  (let* ((name "org-impaste")
         (ext (pcase system-type
                ((or 'gnu 'gnu/linux 'gnu/kfreebsd) "so")
                ('darwin "dylib")
                ('windows-nt "dll")
                ((or 'ms-dot 'cygwin) (error "Unsupported system %s" system-type))))
         (filename (format "%s.%s" name ext)))
    (if (file-exists-p filename)
        (module-load (file-truename filename))
      (error "Dynamic module '%s' don't installed, download from github.com/zombie110year/org-impaste or compile from source" filename))))


(defgroup org-impaste nil
  "Paste image into orgmode from internet or clipboard or drag/drop."
  :group 'org
  :prefix "org-impaste-")

;; This is Configure
(defcustom org-impaste-storage-dir (file-truename "~/org/images/")
  "The directory to store all the image files."
  :type 'string)
;; 调试用 (setq org-impaste-storage-dir (file-truename "./debug"))

;; This is Command
(defun org-impaste-download (url referer)
  "Download images from internet, need input `URL'.
store image files into `org-impaste-storage-dir'
and request by `REFERER', if it's not empty string.

While downloading, it will insert a placeholder like
=<org-impaste-download KEY>= under point.
When download finished, the placeholder will be replaced
by formatted link."
  (interactive "simage url: \nsreferer: ")
  (let* ((key (org-impaste--timer-key-external))
         (placeholder (format "<org-impaste-download %s>" key)))
    (insert placeholder)
    (make-thread
     (lambda ()
       (let* ((impath (org-impaste--download-external
                      url org-impaste-storage-dir referer))
              (impath-r (file-relative-name
                         impath
                         (file-name-directory buffer-file-name)))
              (impath-s (format "[[file:%s]]" impath-r)))
         (message "org-impaste-download %s" impath-r)
         (replace-string-in-region placeholder impath-s 1))))))

;; This is Command
(defun org-impaste-clipboard ()
  "Paste image content as PNG files into `org-impaste-storage-dir'.
insert the formatted link into current buffer."
  (interactive)
  (let* ((key (org-impaste--timer-key-external))
         (placeholder (format "<org-impaste-clipboard %s>" key)))
    (insert placeholder)
    (make-thread
     (lambda ()
       (let* ((impath (org-impaste--clipboard-external
                       org-impaste-storage-dir))
              (impath-r (file-relative-name
                         impath
                         (file-name-directory buffer-file-name)))
              (impath-s (format "[[file:%s]]" impath-r)))
         (message "org-impaste-download %s" impath-r)
         (replace-string-in-region placeholder impath-s 1))))))

(org-impaste--init)
(provide 'org-impaste)
;;; org-impaste.el ends here
