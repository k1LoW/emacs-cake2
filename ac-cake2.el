;;; ac-cake2.el --- CakePHP 2 Minor Mode auto-complete.el source
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2011 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;;
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org

;;; Code:

;;require
(require 'cake2)
(require 'auto-complete)

(defvar ac-cake2-index nil
  "Index of CakePHP 2 candidates.")

(defun ac-cake2-setup ()
  "Setup ac-cake2."
  (add-hook 'after-save-hook 'ac-cake2-build-index))

(defun ac-cake2-build-index ()
  "Build index."
  (unless (not
           (and (cake2-set-app-path) (executable-find "grep")))
    (ignore-errors
      (setq ac-cake2-index nil)
      (with-temp-buffer
        ;;Model Method
        (call-process-shell-command
         (ac-cake2-make-shell-command "Model")
         nil (current-buffer))
        ;;Component Method
        (call-process-shell-command
         (ac-cake2-make-shell-command "Component")
         nil (current-buffer))
        ;;Behavior Method
        (call-process-shell-command
         (ac-cake2-make-shell-command "Behavior")
         nil (current-buffer))
        (goto-char (point-min))
        (flush-lines "^ *$")
        (while (not (eobp))
          (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
              (goto-char (point-max))
            (setq class-name (cake2-camelize (cake2-snake (match-string 1))))
            (setq function-name (match-string 2))
            (delete-region (point) (save-excursion (end-of-line) (point)))
            (push (concat class-name "->" function-name) ac-cake2-index)
            ))
        ac-cake2-index))))

(defun ac-cake2-make-shell-command (dir-name)
  "Make shell command."
  (concat "find "
          cake2-app-path
          " | grep "
          "'/" dir-name "/.*php$'"
          " | xargs grep '[^_]function' "
          "--with-filename"))

(ac-define-source cake2
  '((init . (lambda () (unless ac-cake2-index
                         (ac-cake2-build-index))))
    (candidates . ac-cake2-index)
    (requires . 3)
    (symbol . "f")))

;; Hook
(add-hook 'cake2-hook 'ac-cake2-setup)

(provide 'ac-cake2)

;;; Code ends