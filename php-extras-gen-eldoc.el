;;; php-extras-gen-eldoc.el --- Extra features for `php-mode'

;; Copyright (C) 2012 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>

;; This software is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software.  If not, see
;; <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Download and parse PHP manual from php.net and build a new
;; `php-extras-function-arguments' hash table of PHP functions and
;; their arguments.

;; Please note that build a new `php-extras-function-arguments' is a
;; slow process and might be error prone.

;; The code in this file can must likely be heavily improved.

;;; Code:

(require 'php-extras)



(defvar php-extras-gen-eldoc-temp-methodname nil)

(defvar php-extras-php-manual-url "http://dk.php.net/distributions/manual/php_manual_en.tar.gz"
  "URL of the downloadable .tar.gz version of the PHP manual.")



(defun php-extras-gen-eldoc-elem-string (elem)
  "Helper function for regenerating PHP function argument hash."
  ;; Element is a string it self just return it.
  (if (stringp elem)
      elem
    ;; If element is a list of exactly 3 elements - we continue with
    ;; the 3rd element.
    (if (and (listp elem) 
             (= (safe-length elem) 3))
        (progn
          (ignore-errors
            (when (string= (cdr (car (cadr elem))) "methodname")
              (setq php-extras-gen-eldoc-temp-methodname (nth 2 (nth 2 elem)))))
          (php-extras-gen-eldoc-elem-string (nth 2 elem)))
      ;; If the element is a list with anything but 3 elements iterate
      ;; over them.
      (if (listp elem)
          (let ((result ""))
            (dolist (elem2 (cddr elem) result)
              (setq result (concat result (php-extras-gen-eldoc-elem-string elem2)))))))))

;;;###autoload
(defun php-extras-generate-eldoc ()
  "Regenerate PHP function argument hash table from php.net. This is slow!"
  (interactive)
  (when (yes-or-no-p "Regenerate PHP function argument hash table from php.net. This is slow! ")
    (php-extras-generate-eldoc-1 t)))

(defun php-extras-generate-eldoc-1 (&optional byte-compile)
  "Regenerate PHP function argument hash table from php.net. This is slow!"
  (save-excursion
    (let ((gzip (executable-find "gzip"))
          (php-extras-function-arguments (make-hash-table
                                          :size 4400
                                          :rehash-threshold 1.0
                                          :rehash-size 100
                                          :test 'equal))
          (tar-buffer nil))
      (with-temp-buffer (url-insert-file-contents php-extras-php-manual-url)
                        (call-process-region (point-min) (point-max) gzip t t nil "-c" "-d")
                        (tar-mode)
                        (setq tar-buffer (current-buffer))
                        (goto-char (point-min))
                        (while (re-search-forward "/function\.\\(.+\\)\.html" nil t)
                          (message "Parsing %s..." (match-string-no-properties 1))
                          (let ((buf (tar-extract)))
                            (with-current-buffer buf
                              (goto-char (point-min))
                              (let ((min nil)
                                    (max nil)
                                    (help-string ""))
                                (when (search-forward "<div class=\"methodsynopsis dc-description\">" nil t)
                                  (setq min (point))
                                  (search-forward "</div>" nil t)
                                  (setq max (point))
                                  (dolist (elem (cddr (nth 2 (libxml-parse-html-region min max))))
                                    (setq help-string (concat help-string (php-extras-gen-eldoc-elem-string elem))))
                                  (setq help-string (replace-regexp-in-string "[ \n]+" " " help-string))
                                  (when (and php-extras-gen-eldoc-temp-methodname help-string)
                                    (puthash php-extras-gen-eldoc-temp-methodname help-string php-extras-function-arguments)))))
                            (kill-buffer buf))
                          (set-buffer tar-buffer)))
      (let* ((file (concat php-extras-eldoc-functions-file ".el"))
             (buf (find-file file)))
        (with-current-buffer buf
          (widen)
          (kill-region (point-min) (point-max))
          (insert (format 
                   ";;; %s.el -- file auto generated by `php-extras-generate-eldoc'

(require 'php-extras)

(setq php-extras-function-arguments %s)

(provide 'php-extras-eldoc-functions)

;;; %s.el ends here
"
          (file-name-nondirectory php-extras-eldoc-functions-file)
          (prin1-to-string php-extras-function-arguments)
          (file-name-nondirectory php-extras-eldoc-functions-file)))
          (save-buffer)
          (when byte-compile
            (byte-compile-file file t)))))))



(provide 'php-extras-gen-eldoc)

;;; php-extras-gen-eldoc.el ends here
