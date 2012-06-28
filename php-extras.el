;;; php-extras.el --- Extra features for `php-mode'

;; Copyright (C) 2012 Arne Jørgensen

;; Author: Arne Jørgensen <arne@arnested.dk>
;; URL: https://github.com/arnested/php-extras
;; Created: June 28, 2012
;; Version: 0.1.0-dev
;; Package-Requires: ((php-mode "1.5.0"))
;; Keywords: programming, php

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

;; Extra features for `php-mode':

;;   * `php-extras-insert-previous-variable'

;; Whoop whoop!

;;; Code:

(require 'php-mode)

(defvar php-extras-php-variable-regexp "\\(\\$[a-zA-Z_][a-zA-Z0-9_]*\\(\\[[.*]*\\]\\)*\\)"
  "Regexp for a PHP variable.")



(defun php-extras-insert-previous-variable (arg)
  "Insert previously used variable from buffer.
With prefix argument search that number of times backwards for
variable. If prefix argument is negative search forward."
  (interactive "P")
  (when (null arg)
    (setq arg 1))
  (save-excursion
    (dotimes (var (abs arg))
      (if (> arg 0)
          (re-search-backward php-extras-php-variable-regexp nil t)
        (re-search-forward php-extras-php-variable-regexp nil t))))
  (insert (match-string-no-properties 1)))

(define-key php-mode-map [?\C-c ?\C-$] #'php-extras-insert-previous-variable)



;;;###autoload
(eval-after-load 'php-mode
  (require 'php-extras))



(provide 'php-extras)

;;; php-extras.el ends here