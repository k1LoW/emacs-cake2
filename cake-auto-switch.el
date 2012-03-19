;;; cake-auto-switch.el --- Auto switcher for cake.el and cake2.el

;;; Installation:

;; Put `cake-auto-switch.el' in the `load-path' and add
;;
;;   (require 'cake-auto-switch)
;;   (add-hook 'php-mode-hook 'cake-auto-switch)
;;
;; to your init file.

;;; Code:

(require 'cake)
(require 'cake2)

(defun cake-auto-switch ()
  (interactive)
  (let ((cake-major-version (car (cake-version))))
    (cond
     ((eq cake-major-version 1)
      (cake))
     ((eq cake-major-version 2)
      (cake2)))))

(defun cake-version ()
  (let ((version-file (cake-version-file)))
    (if version-file
	(with-temp-buffer
	  (insert-file-contents version-file)
	  (when (re-search-forward
		 "^\\([0-9]\\)\\.\\([0-9]\\)\\.\\([0-9]\\)" (point-max) t)
	    (mapcar
	     (lambda (n) (string-to-number (match-string n)))
	     (list 1 2 3)))))))

(defun cake-version-file ()
  (let ((dir (file-name-directory (buffer-file-name)))
	version-file)
    (catch 'break
      (while dir
	(dolist (txt (list "lib/Cake/VERSION.txt"
			   "cake/VERSION.txt"))
	  (setq version-file (expand-file-name txt dir))
	  (if (file-exists-p version-file)
	      (throw 'break nil)
	    (setq version-file nil)))
	(setq dir (file-name-directory
		   (or (and (string-match "\\(.+\\)/$" dir)
			    (match-string 1 dir))
		       "")))))
    version-file))

(provide 'cake-auto-switch)
;;; cake-auto-switch.el ends here
