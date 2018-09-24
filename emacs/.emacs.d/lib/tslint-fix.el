;;; tslint-fix.el --- Fix JavaScript files using TSLint

;; This is basically a modification of eslint-fix.el, below is the original
;; copyright notice and info:

;; ;; Copyright (C) 2016 Neri Marschik
;; ;; This package uses the MIT License.
;; ;; See the LICENSE file. (https://github.com/codesuki/eslint-fix/blob/f81f3b47a47460611fbdbdae1d23275ec78f2f8d/LICENSE)
;; ;; Author: Neri Marschik <marschik_neri@cyberagent.co.jp>
;; ;; Version: 1.0
;; ;; Package-Requires: ()
;; ;; Keywords: tools, javascript, eslint, lint, formatting, style
;; ;; URL: https://github.com/codesuki/eslint-fix

;;; Commentary:
;;
;; This file provides `tslint-fix', which fixes the current file using TSLint.
;;; Code:

(defgroup tslint-fix nil
  "Fix JavaScript linting issues with ‘tslint-fix’."
  :link '(function-link tslint-fix)
  :tag "TSLint Fix"
  :group 'tools)

(defcustom tslint-fix-executable "tslint"
  "The TSLint executable to use."
  :tag "TSLint Executable"
  :type 'string)

(defcustom tslint-fix-options nil
  "Additional options to pass to TSLint (e.g. “--quiet”)."
  :tag "TSLint Options"
  :type '(repeat string))

;;;###autoload
(defun tslint-fix ()
  "Format the current file with TSLint."
  (interactive)
  (unless buffer-file-name
    (error "TSLint requires a file-visiting buffer"))
  (when (buffer-modified-p)
    (if (y-or-n-p (format "Save file %s? " buffer-file-name))
        (save-buffer)
      (error "TSLint may only be run on an unmodified buffer")))

  (let ((tslint (executable-find tslint-fix-executable))
        (options (append tslint-fix-options
                         (list "--fix" buffer-file-name))))
    (unless tslint
      (error "Executable ‘%s’ not found" tslint-fix-executable))
    (apply #'call-process tslint nil "*TSLint Errors*" nil options)
    (revert-buffer t t t)))

(provide 'tslint-fix)

;;; tslint-fix.el ends here
