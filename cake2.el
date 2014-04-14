;;; cake2.el --- CakePHP2 Minor Mode
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2011-2014 by 101000code/101000LAB

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

;; Version: 2.0.0
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org
;; Package-Requires: ((dash "2.6.0") (s "1.9.0") (f "0.16.2") (ht "2.0") (json "1.2") (cake-inflector "1.1.0") (historyf "0.0.8") (anything "1.3.9"))

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'cake2)
;; (global-cake2 t)
;;
;; If you use default key map, Put the following expression into your ~/.emacs.
;;
;; (cake2::set-default-keymap)

;;; YASnippet
;; If you use yasnippet, Put snippets/ into YASnippet load-directory.
;; And put the following expression before yas/initialize()
;;
;; (add-hook 'cake2::hook
;;              #'(lambda ()
;;                  (setq yas/mode-symbol 'cake2)))
;;

;;; Commentary:

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `cake2'
;;    CakePHP2 minor mode.
;;  `cake2::switch-to-model'
;;    Switch to model.
;;  `cake2::switch-to-view'
;;    Switch to view.
;;  `cake2::switch-to-controller'
;;    Switch to contoroller.
;;  `cake2::switch-to-model-testcase'
;;    Switch to model testcase.
;;  `cake2::switch-to-controller-testcase'
;;    Switch to contoroller testcase.
;;  `cake2::switch-to-fixture'
;;    Switch to fixture.
;;  `cake2::switch-to-function'
;;    Switch to function.
;;  `cake2::switch-to-element'
;;    Switch to element. If region is active, make new element file.
;;  `cake2::switch-to-js'
;;    Switch to js.
;;  `cake2::switch-to-css'
;;    Switch to stylesheet.
;;  `cake2::switch'
;;    Omni switch function.
;;  `cake2::switch-testcase'
;;    Switch testcase <-> C/M. Or, switch form fixture to testcase.
;;  `cake2::switch-to-file-history'
;;    Switch to file history.
;;  `cake2::open-dirs'
;;    Open directory.
;;  `cake2::open-model-dirs'
;;    Open model directories.
;;  `cake2::open-view-dirs'
;;    Open view directories.
;;  `cake2::open-all-views-dir'
;;    Open all view directories.
;;  `cake2::open-controllers-dir'
;;    Open controller directories.
;;  `cake2::open-behaviors-dir'
;;    Open behavior directories.
;;  `cake2::open-helpers-dir'
;;    Open helper directories.
;;  `cake2::open-components-dir'
;;    Open component directories.
;;  `cake2::open-libs-dir'
;;    Open libs dir.
;;  `cake2::open-config-dir'
;;    Open config dir.
;;  `cake2::open-layout-dirs'
;;    Open layout directories.
;;  `cake2::open-element-dirs'
;;    Open element directories.
;;  `cake2::open-js-dir'
;;    Open J directories.
;;  `cake2::open-css-dir'
;;    Open cs directories.
;;  `cake2::open-test-dirs'
;;    Open test directories.
;;  `cake2::set-version'
;;    Set CakePHP2 version.
;;  `cake2::complete'
;;    Insert CakePHP2 code.
;;  `cake2::tail-log'
;;    Show log by "tail".
;;  `anything-c-cake2-anything-only-source-cake2'
;;    anything only anything-c-source-cake2 and anything-c-source-cake2-model-function.
;;  `anything-c-cake2-anything-only-function'
;;    anything only anything-c-source-cake2-function.
;;  `anything-c-cake2-anything-only-model-function'
;;    anything only anything-c-source-cake2-model-function.
;;  `anything-c-cake2-anything-only-po'
;;    anything only anything-c-source-cake2-po.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `cake2::dir-search-limit'
;;    Search limit
;;    default = 5
;;  `cake2::po-file-path'
;;    Po file path.
;;    default = "jpn/LC_MESSAGES/default.po"
;;  `cake2::use-imenu'
;;    Use imenu function
;;    default = nil
;;  `cake2::core-version'
;;    CakePHP2 version
;;    default = "2.0"

;;; TODO
;;

;;; Code:

;;require
(require 'dash)
(require 's)
(require 'f)
(require 'ht)
(require 'json)
(require 'cake-inflector)
(require 'cl)
(require 'anything)
(require 'historyf)
(require 'easy-mmode)

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'cake2::complete
                                '(length cake2::initial-input)))

(defgroup cake2 nil
  "CakePHP2 minor mode"
  :group 'convenience
  :prefix "cake2::")

(defcustom cake2::dir-search-limit 5
  "Search limit"
  :type 'integer
  :group 'cake2)

(defcustom cake2::po-file-path "jpn/LC_MESSAGES/default.po"
  "Po file path."
  :type 'string
  :group 'cake2)

(defcustom cake2::use-imenu nil
  "Use imenu function"
  :type 'boolean
  :group 'cake2)

(defcustom cake2::core-version "2.0"
  "CakePHP2 version"
  :type 'string
  :group 'cake2)

;;;###autoload
(define-minor-mode cake2
  "CakePHP2 minor mode."
  :lighter " Cake2"
  :group 'cake2
  (if cake2
      (progn
        (setq minor-mode-map-alist
              (cons (cons 'cake2 cake2::key-map)
                    minor-mode-map-alist))
        (run-hooks 'cake2::hook))
    nil))

;;;###autoload
(when (fboundp 'define-global-minor-mode)
  (define-global-minor-mode global-cake2
    cake2 cake2::maybe
    :group 'cake2))

(defun cake2::maybe ()
  "What buffer `cake2' prefers."
  (if (and (not (minibufferp (current-buffer)))
           (cake2::app-build))
      (cake2 1)
    nil))

;; key-map
(defvar cake2::key-map
  (make-sparse-keymap)
  "Keymap for Cake2.")

(defvar cake2::app-path nil
  "CakePHP2 app directory path.")

(defvar cake2::action-name "index"
  "CakePHP2 action name.")

(defvar cake2::lower-camelized-action-name "index"
  "CakePHP2 lower camelized action name.")

(defvar cake2::snake-action-name "index"
  "CakePHP2 snake_case action name.")

(defvar cake2::view-extension "ctp"
  "CakePHP2 view extension.")

(defvar cake2::singular-name nil
  "CakePHP2 current singular name.")

(defvar cake2::plural-name nil
  "CakePHP2 current plural name.")

(defvar cake2::camelize-name nil
  "CakePHP2 current camelize name.")

(defvar cake2::themed-name nil
  "CakePHP2 current view themed name.")

(defvar cake2::model-dirs nil
  "Model directories.")

(defvar cake2::view-dirs nil
  "View directories.")

(defvar cake2::themed-dirs nil
  "View directories.")

(defvar cake2::controller-dirs nil
  "Contoroller directories.")

(defvar cake2::behavior-dirs nil
  "Behavior directories.")

(defvar cake2::helper-dirs nil
  "Helper directories.")

(defvar cake2::component-dirs nil
  "Component directories.")

(defvar cake2::model-testcase-dirs nil
  "Model testcase directories.")

(defvar cake2::controller-testcase-dirs nil
  "Contoroller testcase directories.")

(defvar cake2::fixture-dirs nil
  "Fixture directories.")

(defvar cake2::js-dirs nil
  "JavaScript directories.")

(defvar cake2::css-dirs nil
  "Css directories.")

(defvar cake2::plugin-dirs nil
  "Plugin directories.")

(defvar cake2::model-regexp "^.+/app/Model/\\([^/]+\\)\.php$"
  "Model file regExp.")

(defvar cake2::view-regexp "^.+/app/View/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"
  "View file regExp.")

(defvar cake2::themed-regexp "^.+/app/View/Themed/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"
  "View file regExp.")

(defvar cake2::controller-regexp "^.+/app/Controller/\\([^/]+\\)Controller\.php$"
  "Contoroller file regExp.")

(defvar cake2::behavior-regexp "^.+/app/Model/Behavior/\\([^/]+\\)\.php$"
  "Behavior file regExp.")

(defvar cake2::helper-regexp "^.+/app/View/Helper/\\([^/]+\\)\.php$"
  "Helper file regExp.")

(defvar cake2::component-regexp "^.+/app/Controller/Component/\\([^/]+\\)\.php$"
  "Component file regExp.")

(defvar cake2::model-testcase-regexp "^.+/app/Test/Case/Model/\\([^/]+\\)\Test\.php$"
  "Model testcase file regExp.")

(defvar cake2::controller-testcase-regexp "^.+/app/Test/Cases/Controller/\\([^/]+\\)ControllerTest\.php$"
  "Contoroller testcase file regExp.")

(defvar cake2::fixture-regexp "^.+/app/Test/Fixture/\\([^/]+\\)Fixture\.php$"
  "Fixture file regExp.")

(defvar cake2::js-regexp "^.+/app/webroot/js/.+\.js$"
  "Js file regExp.")

(defvar cake2::css-regexp "^.+/app/webroot/css/.+\.css$"
  "Css file regExp.")

(defvar cake2::current-file-type nil
  "Current file type.")

(defvar cake2::file-history nil
  "Switch file history.")

(defvar cake2::hook nil
  "Hook.")

(defun cake2::set-default-keymap ()
  "Set default key-map."
  (setq cake2::key-map
        (let ((map (make-sparse-keymap)))
          (define-key map "\C-cs" 'cake2::switch)
          (define-key map "\C-ct" 'cake2::switch-testcase)
          (define-key map "\C-cm" 'cake2::switch-to-model)
          (define-key map "\C-cv" 'cake2::switch-to-view)
          (define-key map "\C-cc" 'cake2::switch-to-controller)
          (define-key map "\C-cx" 'cake2::switch-to-fixture)
          (define-key map "\C-cf" 'cake2::switch-to-function)
          (define-key map "\C-ce" 'cake2::switch-to-element)
          (define-key map "\C-cj" 'cake2::switch-to-js)
          (define-key map "\C-cb" 'cake2::switch-to-file-history)
          (define-key map "\C-cM" 'cake2::open-model-dirs)
          (define-key map "\C-cV" 'cake2::open-view-dirs)
          (define-key map "\C-u\C-cV" 'cake2::open-all-views-dir)
          (define-key map "\C-c\C-l" 'cake2::open-layout-dirs)
          (define-key map "\C-cC" 'cake2::open-controllers-dir)
          (define-key map "\C-cB" 'cake2::open-behaviors-dir)
          (define-key map "\C-cH" 'cake2::open-helpers-dir)
          (define-key map "\C-cP" 'cake2::open-components-dir)
          (define-key map "\C-cL" 'cake2::open-libs-dir)
          (define-key map "\C-cE" 'cake2::open-element-dirs)
          (define-key map "\C-cJ" 'cake2::open-js-dir)
          (define-key map "\C-cS" 'cake2::open-css-dir)
          (define-key map "\C-cT" 'cake2::open-test-dirs)
          (define-key map "\C-c\C-g" 'cake2::open-config-dir)
          (define-key map "\C-c\C-t" 'cake2::tail-log)
          ;; anything-functions
          (define-key map "\C-cl" 'anything-c-cake2-anything-only-source-cake2)
          (define-key map "\C-co" 'anything-c-cake2-anything-only-function)
          (define-key map "\C-cp" 'anything-c-cake2-anything-only-po)
          map)))

(defun cake2::is-model-file ()
  "Check whether current file is model file."
  (cake2::app-build)
  (if (not (string-match cake2::model-regexp (buffer-file-name)))
      nil
    (setq cake2::singular-name (match-string 1 (buffer-file-name)))
    (setq cake2::plural-name (cake-pluralize cake2::singular-name))
    (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
    (setq cake2::current-file-type 'model)))

(defun cake2::is-view-file ()
  "Check whether current file is view file."
  (cake2::app-build)
  (setq cake2::themed-name nil)
  (if (string-match cake2::themed-regexp (buffer-file-name))
      (progn
        (setq cake2::themed-name (match-string 1 (buffer-file-name)))
        (setq cake2::plural-name (match-string 2 (buffer-file-name)))
        (setq cake2::action-name (match-string 4 (buffer-file-name)))
        (setq cake2::view-extension (match-string 5 (buffer-file-name)))
        (setq cake2::lower-camelized-action-name (cake-lower-camelize cake2::action-name))
        (setq cake2::singular-name (cake-singularize cake2::plural-name))
        (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
        (setq cake2::current-file-type 'view))
    (if (not (string-match cake2::view-regexp (buffer-file-name)))
        nil
      (setq cake2::plural-name (match-string 1 (buffer-file-name)))
      (setq cake2::action-name (match-string 3 (buffer-file-name)))
      (setq cake2::view-extension (match-string 4 (buffer-file-name)))
      (setq cake2::lower-camelized-action-name (cake-lower-camelize cake2::action-name))
      (setq cake2::singular-name (cake-singularize cake2::plural-name))
      (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
      (setq cake2::current-file-type 'view))))

(defun cake2::is-controller-file ()
  "Check whether current file is contoroller file."
  (cake2::app-build)
  (if (not (string-match cake2::controller-regexp (buffer-file-name)))
      nil
    (string-match cake2::controller-regexp (buffer-file-name))
    (setq cake2::plural-name (match-string 1 (buffer-file-name)))
    (save-excursion
      (if (cond
           ((re-search-backward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t))
           ((re-search-forward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t)))
          (progn
            (setq cake2::action-name (match-string 1))
            (setq cake2::lower-camelized-action-name (cake-lower-camelize cake2::action-name))
            (setq cake2::snake-action-name (cake-snake cake2::action-name)))
        (setq cake2::action-name nil)
        (setq cake2::lower-camelized-action-name nil)
        (setq cake2::snake-action-name nil)))
    (setq cake2::singular-name (cake-singularize cake2::plural-name))
    (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
    (setq cake2::current-file-type 'controller)))

(defun cake2::is-behavior-file ()
  "Check whether current file is Behavior file."
  (cake2::app-build)
  (if (not (string-match cake2::behavior-regexp (buffer-file-name)))
      nil
    (setq cake2::current-file-type 'behavior)))

(defun cake2::is-helper-file ()
  "Check whether current file is Helper file."
  (cake2::app-build)
  (if (not (string-match cake2::helper-regexp (buffer-file-name)))
      nil
    (setq cake2::current-file-type 'helper)))

(defun cake2::is-component-file ()
  "Check whether current file is Component file."
  (cake2::app-build)
  (if (not (string-match cake2::component-regexp (buffer-file-name)))
      nil
    (setq cake2::current-file-type 'component)))

(defun cake2::is-model-testcase-file ()
  "Check whether current file is model testcase file."
  (cake2::app-build)
  (if (not (string-match cake2::model-testcase-regexp (buffer-file-name)))
      nil
    (setq cake2::singular-name (match-string 1 (buffer-file-name)))
    (setq cake2::plural-name (cake-pluralize cake2::singular-name))
    (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
    (setq cake2::current-file-type 'model-testcase)))

(defun cake2::is-controller-testcase-file ()
  "Check whether current file is controller testcase file."
  (cake2::app-build)
  (if (not (string-match cake2::controller-testcase-regexp (buffer-file-name)))
      nil
    (setq cake2::plural-name (match-string 1 (buffer-file-name)))
    (setq cake2::singular-name (cake-singularize cake2::plural-name))
    (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
    (setq cake2::current-file-type 'controller-testcase)))

(defun cake2::is-fixture-file ()
  "Check whether current file is fixture file."
  (cake2::app-build)
  (if (not (string-match cake2::fixture-regexp (buffer-file-name)))
      nil
    (setq cake2::singular-name (match-string 1 (buffer-file-name)))
    (setq cake2::plural-name (cake-pluralize cake2::singular-name))
    (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
    (setq cake2::current-file-type 'fixture)))

(defun cake2::is-js-file ()
  "Check whether current file is Js file."
  (cake2::app-build)
  (if (not (string-match cake2::js-regexp (buffer-file-name)))
      nil
    (setq cake2::current-file-type 'js)))

(defun cake2::is-css-file ()
  "Check whether current file is Js file."
  (cake2::app-build)
  (if (not (string-match cake2::css-regexp (buffer-file-name)))
      nil
    (setq cake2::current-file-type 'css)))

(defun cake2::is-file ()
  "Check whether current file is CakePHP2's file."
  (if (or (cake2::is-model-file)
          (cake2::is-controller-file)
          (cake2::is-view-file)
          (cake2::is-behavior-file)
          (cake2::is-helper-file)
          (cake2::is-component-file)
          (cake2::is-js-file)
          (cake2::is-css-file)
          (cake2::is-model-testcase-file)
          (cake2::is-controller-testcase-file)
          (cake2::is-fixture-file))
      t nil))

(defun cake2::get-current-line ()
  "Get current line."
  (thing-at-point 'line))

(defun cake2::app-build ()
  "Set cake2::app-path"
  (setq cake2::app-path (cake2::find-app-path))
  (unless (not cake2::app-path)
    (cake2::set-build-pathes)
    (cake2::set-regexp)))

(defun cake2::find-app-path ()
  "Find app directory."
  (let ((current-dir (f-expand default-directory)))
    (loop with count = 0
          until (f-exists? (f-join current-dir "Config/core.php"))
          ;; Return nil if outside the value of
          if (= count cake2::dir-search-limit)
          do (return nil)
          ;; Or search upper directories.
          else
          do (incf count)
          (unless (f-root? current-dir)
            (setq current-dir (f-dirname current-dir)))
          finally return current-dir)))

(defun cake2::read-dot-cake ()
  "Find .cake"
  (unless cake2::app-path
    (error "%s" "Can't find CakePHP project app path."))
  (unless (not (f-exists? (f-join cake2::app-path ".cake")))
      (json-read-file (f-join cake2::app-path ".cake"))))

(defun cake2::append-to-build-pathes (key pathes)
  "Append path to cake2::build-pathes[key]."
  (unless cake2::build-pathes
    (error "%s" "Can't set cake2::build-pathes"))
  (let ((formatted-key (cake-camelize (cake-singularize (symbol-name key)))))
    (ht-set! cake2::build-pathes formatted-key
      (-union (ht-get cake2::build-pathes formatted-key) (coerce pathes 'list)))))

(defun cake2::set-build-pathes ()
  "Set CakePHP App::build() pathes."
  (unless cake2::app-path
    (error "%s" "Can't find CakePHP project app path."))
  (let ((build-pathes (ht 
                        ((f-filename "Model") (list "Model"))
                        ((f-filename "Model/Behavior") (list "Model/Behavior"))
                        ((f-filename "Model/Datasource") (list "Model/Datasource"))
                        ((f-filename "Model/Datasource/Database") (list "Model/Datasource/Database"))
                        ((f-filename "Model/Datasource/Session") (list "Model/Datasource/Session"))
                        ((f-filename "Controller") (list "Controller"))
                        ((f-filename "Controller/Component") (list "Controller/Component"))
                        ((f-filename "Controller/Component/Auth") (list "Controller/Component/Auth"))
                        ((f-filename "Controller/Component/Acl") (list "Controller/Component/Acl"))
                        ((f-filename "View") (list "View"))
                        ((f-filename "View/Helper") (list "View/Helper"))
                        ((f-filename "Console") (list "Console"))
                        ((f-filename "Console/Command") (list "Console/Command"))
                        ((f-filename "Console/Command/Task") (list "Console/Command/Task"))
                        ((f-filename "Lib") (list "Lib"))
                        ((f-filename "Locale") (list "Locale"))
                        ((f-filename "Vendor") (list "Vendor"))
                        ((f-filename "Plugin") (list "Plugin"))
                       ))
        (dot-cake-hash (ht<-alist (cake2::read-dot-cake))))
    (setq cake2::build-pathes build-pathes)
    ;; build
    (ht-each (lambda ($key $pathes)
               (cake2::append-to-build-pathes $key $pathes)
               ) (ht<-alist (ht-get dot-cake-hash 'build_path)))
    ))

(defun cake2::set-regexp ()
  "Set regExp."
  (setq cake2::model-regexp (concat cake2::app-path "/Model/\\([^/]+\\)\.php"))
  (setq cake2::view-regexp (concat cake2::app-path "/View/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"))
  (setq cake2::themed-regexp (concat cake2::app-path "/View/Themed/\\([^/]+\\)/\\([^/]+\\)/\\([^/]+/\\)?\\([^/.]+\\)\\.\\([a-z]+\\)$"))
  (setq cake2::controller-regexp (concat cake2::app-path "/.+/\\([^/]+\\)Controller\.php$"))
  (setq cake2::behavior-regexp (concat cake2::app-path "/Model/Behavior/\\([^/]+\\)\.php$"))
  (setq cake2::helper-regexp (concat cake2::app-path "/View/Helper/\\([^/]+\\)\.php$"))
  (setq cake2::component-regexp (concat cake2::app-path "/Controller/Component/\\([^/]+\\)\.php$"))
  (setq cake2::model-testcase-regexp (concat cake2::app-path "/Test/Case/Model/\\([^/]+\\)\Test\.php$"))
  (setq cake2::controller-testcase-regexp (concat cake2::app-path "/Test/Case/Controller/\\([^/]+\\)ControllerTest\.php$"))
  (setq cake2::fixture-regexp (concat cake2::app-path "/Test/Fixture/\\([^/]+\\)Fixture\.php$"))
  (setq cake2::js-regexp (concat cake2::app-path "/webroot/js/.+\.js$"))
  (setq cake2::css-regexp (concat cake2::app-path "/webroot/css/.+\.css$"))
  t)

(defun cake2::switch-to-model ()
  "Switch to model."
  (interactive)
  (cake2::is-file)
  (if (cake2::is-file)
      (cake2::switch-to-file (f-join cake2::app-path "Model" (concat cake2::camelize-name ".php")))
    (message "Can't find model name.")))

(defun cake2::switch-to-view ()
  "Switch to view."
  (interactive)
  (let ((view-files nil))
    (if (and (cake2::is-file)
             cake2::action-name)
        (progn
          (if (cake2::is-model-file)
              (setq cake2::plural-name (cake-pluralize cake2::singular-name)))
          (setq view-files (cake2::set-view-list))
          (if view-files
              (cond
               ((= 1 (length view-files))
                (find-file (f-join cake2::app-path "View" cake2::plural-name (car view-files))))
               (t (anything
                   '(((name . "Switch to view")
                      (candidates . view-files)
                      (display-to-real . (lambda (candidate)
                                           (f-join cake2::app-path "View" cake2::plural-name candidate)
                                           ))
                      (type . file)))
                   nil nil nil nil)
                  ))
            (if (y-or-n-p "Make new file?")
                (progn
                  (unless (f-dir? (f-join cake2::app-path "View" cake2::plural-name))
                    (make-directory (f-join cake2::app-path "View" cake2::plural-name)))
                  (find-file (f-join cake2::app-path "View" cake2::plural-name (concat cake2::action-name "." cake2::view-extension))))
              (message (format "Can't find %s" (f-join cake2::app-path "View" cake2::plural-name (concat cake2::action-name "." cake2::view-extension)))))))
      (message "Can't switch to view."))))

(defun cake2::set-view-list ()
  "Set view list."
  (let* ((dir (f-join cake2::app-path "View" cake2::plural-name))
         (view-dir (list dir))
         (view-files nil))
    (unless (not (f-dir? dir))
      (unless (not (f-directories dir))
        (-flatten (add-to-list `view-dir (f-directories dir))))
      (loop for x in view-dir do (if (f-exists? (f-join cake2::app-path "View" cake2::plural-name x (concat cake2::snake-action-name "." cake2::view-extension)))
                                     (unless (some (lambda (y) (equal (f-join x (concat cake2::snake-action-name "." cake2::view-extension)) y)) view-files)
                                       (push (f-join x (concat cake2::snake-action-name "." cake2::view-extension)) view-files))))
      (loop for x in view-dir do (if (f-exists? (f-join cake2::app-path "View" cake2::plural-name x (concat cake2::snake-action-name ".ctp")))
                                     (unless (some (lambda (y) (equal (f-join x (concat cake2::snake-action-name ".ctp")) y)) view-files)
                                       (push (f-join x (concat cake2::snake-action-name ".ctp")) view-files))))
      (loop for x in view-dir do (if (f-exists? (f-join cake2::app-path "View" cake2::plural-name x (concat cake2::action-name "." cake2::view-extension)))
                                     (unless (some (lambda (y) (equal (f-join x (concat cake2::action-name "." cake2::view-extension)) y)) view-files)
                                       (push (f-join x (concat cake2::action-name "." cake2::view-extension)) view-files))))
      (loop for x in view-dir do (if (f-exists? (f-join cake2::app-path "View" cake2::plural-name x (concat cake2::action-name ".ctp")))
                                     (unless (some (lambda (y) (equal (f-join x (concat cake2::action-name ".ctp")) y)) view-files)
                                       (push (f-join x (concat cake2::action-name ".ctp")) view-files)))))
    view-files))

(defun cake2::switch-to-controller ()
  "Switch to contoroller."
  (interactive)
  (if (cake2::is-file)
      (progn
        (if (f-exists? (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php")))
            (progn
              (find-file (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php")))
              (goto-char (point-min))
              (if (not (re-search-forward (concat "function[ \t]*" cake2::lower-camelized-action-name "[ \t]*\(") nil t))
                  (progn
                    (goto-char (point-min))
                    (re-search-forward (concat "function[ \t]*" cake2::action-name "[ \t]*\(") nil t)))
              (recenter))
          (if (y-or-n-p "Make new file?")
              (find-file (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php")))
            (message (format "Can't find %s" (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php")))))))
    (message "Can't switch to contoroller.")))

(defun cake2::switch-to-model-testcase ()
  "Switch to model testcase."
  (interactive)
  (if (cake2::is-file)
      (cake2::switch-to-file (f-join cake2::app-path "Test/Case/Model" (concat cake2::singular-name "Test.php")))
    (message "Can't switch to model testcase.")))

(defun cake2::switch-to-controller-testcase ()
  "Switch to contoroller testcase."
  (interactive)
  (if (cake2::is-file)
      (cake2::switch-to-file (f-join cake2::app-path "Test/Case/Controller" (concat cake2::plural-name "ControllerTest.php")))
    (message "Can't switch to contoroller testcase.")))

(defun cake2::switch-to-fixture ()
  "Switch to fixture."
  (interactive)
  (if (cake2::is-file)
      (cake2::switch-to-file (f-join cake2::app-path "Test/Fixture" (cake2::singular-name "Fixture.php")))
    (message "Can't switch to fixture.")))

(defun cake2::switch-to-file (file-path)
  "Switch to file."
  (if (f-exists? file-path)
      (find-file file-path)
    (if (y-or-n-p "Make new file?")
        (find-file file-path)
      (message (format "Can't find %s" file-path)))))

(defun cake2::find-file-if-exists (file-path)
  "find file if file exsits."
  (if (f-exists? file-path)
      (find-file file-path)
    nil))

(defun cake2::search-functions ()
  "Search function from current buffer."
  (let ((func-list nil))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "function[ \t]*\\([a-zA-Z0-9_]+\\)[ \t]*\(" nil t)
        (push (match-string 1) func-list))
      func-list)))

(defun cake2::switch-to-function ()
  "Switch to function."
  (interactive)
  (let ((current-func nil))
    (if (and cake2::use-imenu
             (require 'imenu nil t))
        (anything 'anything-c-source-imenu)
      (if (or (cake2::is-controller-file)
              (cake2::is-model-file)
              (cake2::is-js-file))
          (progn
            (setq current-func (cake2::search-functions))
            (anything
             '(((name . "Switch to current function")
                (candidates . current-func)
                (display-to-real . (lambda (candidate)
                                     (concat "function[ \t]*" candidate "[ \t]*\(")))
                (action
                 ("Switch to Function" . (lambda (candidate)
                                           (goto-char (point-min))
                                           (re-search-forward candidate nil t)
                                           (recenter)
                                           )))))
             nil nil nil nil))
        (message "Can't switch to function.")))))

(defun cake2::switch-to-element ()
  "Switch to element.  If region is active, make new element file."
  (interactive)
  (let ((element-name nil) (themed-path ""))
    (if (cake2::is-view-file)
        (progn
          (unless (not cake2::themed-name)
            (setq themed-path (f-join "Themed" cake2::themed-name)))
          (if (or (string-match "renderElement( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake2::get-current-line))
                  (string-match "element(['\"]\\( *[-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake2::get-current-line)))
              (progn (setq element-name (match-string 1 (cake2::get-current-line)))
                     (unless (cake2::find-file-if-exists (f-join cake2::app-path "View" themed-path "Elements" (concat element-name "." cake2::view-extension)))
                       (if (y-or-n-p "Make new file?")
                           (find-file (f-join cake2::app-path "View" themed-path "Elements" (concat element-name "." cake2::view-extension)))
                         (message (format "Can't find %s" (f-join cake2::app-path "View" themed-path "Elements" (concat element-name "." cake2::view-extension)))))))
            (if (not (and (region-active-p)
                          (y-or-n-p "Can't find element name. Make new file?")))
                (message "Can't find element name.")
              (setq element-name (read-string "Element name (no extension): " "element_name"))
              (if (not element-name)
                  (message "Can't find element name.")
                (kill-region (point) (mark))
                (insert (concat "<?php echo $this->element('" element-name "'); ?>"))
                (find-file (f-join cake2::app-path "View" themed-path "Elements" (concat element-name "." cake2::view-extension)))
                (yank)))))
      (message "Current buffer is not view."))))

(defun cake2::switch-to-js ()
  "Switch to js."
  (interactive)
  (let ((js-name nil))
    (if (cake2::app-build)
        (if (or (string-match "$js->link( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line))
                (string-match "$this->Html->script( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line)))
            (progn (setq js-name (match-string 1 (cake2::get-current-line)))
                   (cond
                    ((cake2::find-file-if-exists (f-join cake2::app-path "webroot/js" js-name))
                     t)
                    ((cake2::find-file-if-exists (f-join cake2::app-path "webroot/js" (concat js-name ".js")))
                     t)
                    ((y-or-n-p "Make new file?")
                     (find-file (f-join cake2::app-path "webroot/js" (concat js-name ".js"))))
                    (message (format "Can't find %s" (f-join cake2::app-path "webroot/js" (concat js-name ".js"))))))
          (message "Can't find js name."))
      (message "Can't set app path."))))

(defun cake2::switch-to-css ()
  "Switch to stylesheet."
  (interactive)
  (let ((css-name nil))
    (if (cake2::app-build)
        (if (or (string-match "$html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line))
                (string-match "$this->Html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line)))
            (progn (setq css-name (match-string 1 (cake2::get-current-line)))
                   (cond
                    ((cake2::find-file-if-exists (f-join cake2::app-path "webroot/css" css-name))
                     t)
                    ((cake2::find-file-if-exists (f-join cake2::app-path "webroot/css" (concat css-name ".css")))
                     t)
                    ((y-or-n-p "Make new file?")
                     (find-file (f-join cake2::app-path "webroot/css" (concat css-name ".css"))))
                    (message (format "Can't find %s" (f-join cake2::app-path "webroot/css" (concat css-name ".css"))))))
          (message "Can't find stylesheet  name."))
      (message "Can't set app path."))))

(defun cake2::switch ()
  "Omni switch function."
  (interactive)
  (if (cake2::app-build)
      (cond
       ;;cake2::switch-to-js
       ((or (string-match "$js->link( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line))
            (string-match "$this->Html->script( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line))) (cake2::switch-to-js))
       ;;cake2::switch-to-element
       ((or (string-match "renderElement( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake2::get-current-line))
            (string-match "element(['\"]\\( *[-a-zA-Z0-9_/\.]+\\)['\"].*)" (cake2::get-current-line))) (cake2::switch-to-element))
       ;;cake2::switch-to-css
       ((or (string-match "$html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line))
            (string-match "$this->Html->css( *['\"]\\([-a-zA-Z0-9_/\.]+\\)['\"].*" (cake2::get-current-line))) (cake2::switch-to-css))
       ;;cake2::switch-to-controller
       ((cake2::is-view-file) (cake2::switch-to-controller))
       ;;cake2::switch-to-view
       ((cake2::is-controller-file) (cake2::switch-to-view))
       (t (message "Current buffer is neither view nor controller.")))
    (message "Can't set app path.")))

(defun cake2::switch-testcase ()
  "Switch testcase <-> C/M.  Or, switch form fixture to testcase."
  (interactive)
  (cond ((cake2::is-model-file) (cake2::switch-to-model-testcase))
        ((cake2::is-controller-file) (cake2::switch-to-controller-testcase))
        ((cake2::is-model-testcase-file) (cake2::switch-to-model))
        ((cake2::is-controller-testcase-file) (cake2::switch-to-controller))
        ((cake2::is-fixture-file) (cake2::switch-to-model-testcase))
        (t (message "Current buffer is neither model nor controller."))))

(defun cake2::switch-to-file-history ()
  "Switch to file history."
  (interactive)
  (historyf-back '(cake2)))

(defun cake2::open-dirs (dir &optional recursive ignore)
  "Open DIR."
  (interactive)
  (if (cake2::app-build)
      (anything-other-buffer
       (cake2::create-open-dirs-anything-sources dir recursive ignore) nil)
    (message "Can't set app path.")))

(defun cake2::create-open-dirs-anything-sources (dir &optional recursive ignore)
  "Create 'Open DIR' anything-sources.  If RECURSIVE is true recursive."
  (let (sources)
    (unless (listp dir)
      (setq dir (list dir)))
    (if (cake2::app-build)
        (progn
          (loop for d in dir do
                (unless (not (f-dir? (f-join cake2::app-path d)))
                  (push
                   `((name . ,(concat "Open directory: " d))
                     (candidates . ,(remove-if (lambda (x) (and ignore (string-match ignore x))) (cake2::directory-files d recursive)))
                     (display-to-real . (lambda (candidate)
                                          (f-join ,cake2::app-path ,d candidate)))
                     (type . file))
                   sources)))
          (reverse sources))
      nil)))

(defun cake2::directory-files (dir &optional recursive)
  "Get DIR files. If RECURSIVE = true, get DIR recuresively."
  (-map
   (lambda (file) (f-relative file (f-join cake2::app-path dir)))
   (f-files (f-join cake2::app-path dir) (lambda (file) (not (s-matches? "\\.svn" (f-long file)))) recursive)))

(defun cake2::build-model-files ()
    ".cakeから"
    (let ((dirs (cake2::find-plugin-dirs)))
        (setq dirs (-map (lambda (dir) (if dir (f-join dir "Model") nil))) dirs)
        (push "Model" dirs)
        (cake2::open-dirs dirs t "Behavior\\|Datasource")))

(defun cake2::open-model-dirs ()
  "Open model directries."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "Model") nil))) plugin-list))
    (push "Model" plugin-list)
    (cake2::open-dirs plugin-list t "Behavior\\|Datasource")))

(defun cake2::open-view-dirs ()
  "Open view directories."
  (interactive)
  (let ((themed-list (cake2::find-themed-dirs)) (plugin-list (cake2::find-plugin-dirs)))
    (if (not (or (cake2::is-model-file) (cake2::is-controller-file) (cake2::is-view-file)))
        (cake2::open-all-views-dir)
      (setq themed-list (mapcar (function (lambda (c) (if c (f-join c cake2::plural-name) nil))) themed-list))
      (push (f-join "View" cake2::plural-name) themed-list)
      (cake2::open-dirs themed-list))))

(defun cake2::open-all-views-dir ()
  "Open all view directories."
  (interactive)
  (let ((themed-list (cake2::find-themed-dirs)) (plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "View") nil))) plugin-list))
    (push "View" plugin-list)
    (cake2::open-dirs plugin-list t "Helper")))

(defun cake2::open-controllers-dir ()
  "Open controller directories."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs))
        (controller-list (cake2::find-controller-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "Controller") nil))) plugin-list))
    (setq controller-list (append controller-list plugin-list))
    (push "Controller" controller-list)
    (cake2::open-dirs (remove-if (lambda (x) (string-match "Component" x)) controller-list) t "Component")))

(defun cake2::open-behaviors-dir ()
  "Open behavior directories."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "Model/Behavior") nil))) plugin-list))
    (push "Model/Behavior" plugin-list)
    (cake2::open-dirs plugin-list)))

(defun cake2::open-helpers-dir ()
  "Open helper directories."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "View/Helper") nil))) plugin-list))
    (push "View/Helper" plugin-list)
    (cake2::open-dirs plugin-list)))

(defun cake2::open-components-dir ()
  "Open component directories."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "Controller/Component") nil))) plugin-list))
    (push "Controller/Component" plugin-list)
    (cake2::open-dirs plugin-list)))

(defun cake2::open-libs-dir ()
  "Open libs dir."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "Lib") nil))) plugin-list))
    (push "Lib" plugin-list)
    (cake2::open-dirs plugin-list t)))

(defun cake2::open-config-dir ()
  "Open config dir."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "Config") nil))) plugin-list))
    (push "Config" plugin-list)
    (cake2::open-dirs plugin-list t)))

(defun cake2::open-layout-dirs ()
  "Open layout directories."
  (interactive)
  (let ((layouts)
        (plugin-list (cake2::find-plugin-dirs))
        (themed-list (cake2::find-themed-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "View/Layouts") nil))) plugin-list))
    (setq themed-list (mapcar (function (lambda (c) (if c (f-join c "Layouts") nil))) themed-list))
    (setq layouts (append plugin-list themed-list))
    (push "View/Layouts" layouts)
    (cake2::open-dirs layouts t)))

(defun cake2::open-element-dirs ()
  "Open element directories."
  (interactive)
  (let ((elements)
        (plugin-list (cake2::find-plugin-dirs))
        (themed-list (cake2::find-themed-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "View/Elements") nil))) plugin-list))
    (setq themed-list (mapcar (function (lambda (c) (if c (f-join c "Elements") nil))) themed-list))
    (setq elements (append plugin-list themed-list))
    (push "View/Elements" elements)
    (cake2::open-dirs elements t)))

(defun cake2::open-js-dir ()
  "Open Js directories."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "webroot/js") nil))) plugin-list))
    (push "webroot/js" plugin-list)
    (cake2::open-dirs plugin-list t)))

(defun cake2::open-css-dir ()
  "Open css directories."
  (interactive)
  (let ((plugin-list (cake2::find-plugin-dirs)))
    (setq plugin-list (mapcar (function (lambda (c) (if c (f-join c "webroot/css") nil))) plugin-list))
    (push "webroot/css" plugin-list)
    (cake2::open-dirs plugin-list t)))

(defun cake2::open-test-dirs ()
  "Open test directories."
  (interactive)
  (let ((tests nil)
        (plugin-list (cake2::find-plugin-dirs)))
    (setq tests (append (mapcar (function (lambda (c) (if c (f-join c "Tests/Group") nil))) plugin-list) tests))
    (setq tests (append (mapcar (function (lambda (c) (if c (f-join c "Test/Fixture") nil))) plugin-list) tests))
    (setq tests (append (mapcar (function (lambda (c) (if c (f-join c "Test/Case") nil))) plugin-list) tests))
    (push "Tests/Group" tests)
    (push "Test/Fixture" tests)
    (push "Test/Case" tests)
    (cake2::open-dirs tests t)))

(defun cake2::find-themed-dirs ()
  "Find themed directory. like app/View/Themed/m"
  (cake2::find-dirs "View/Themed"))

(defun cake2::find-controller-dirs ()
  "Find plugin directory. like app/Controller"
  (remove-if (lambda (x) (string-match "Component" x)) (cake2::find-dirs "Contoller")))

(defun cake2::find-plugin-dirs ()
  "Find plugin directory. like app/plugins"
  (cake2::find-dirs "Plugin"))

(defun cake2::find-dirs (dir)
  "Find directory list."
  (if (f-dir? (f-join cake2::app-path dir))
      (f-directories (f-join cake2::app-path dir))
    nil))

(defvar cake2::source-version
  '((name . "CakePHP2 core version")
    (candidates . (lambda () (list "1.2" "1.3")))
    (action
     ("Set Version" . (lambda (candidate)
                        (setq cake2::core-version candidate))))))

(defun cake2::set-version ()
  "Set CakePHP2 version."
  (interactive)
  (if (cake2::app-build)
      (anything '(cake2::source-version)
                nil "Version: " nil)))

(defvar cake2::source-models
  '((name . "Cake2 Model")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (setq c (capitalize c))
                                        (while (string-match "_\\|\.Php" c)
                                          (setq c (replace-match "" nil nil c)))
                                        (concat "$this->loadModel('" c "');")
                                        ))                            
                            (cake2::directory-files "Model")
                            )))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake2::initial-input))
                        (insert candidate))))))

(defvar cake2::source-js
  '((name . "Cake2 Js")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (if (string-match (concat "\\.js$") c)
                                            (setq c (replace-match "" nil nil c)))
                                        (concat "$this->Html->script('" c "');")))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (f-dir? (f-join cake2::app-path "webroot/js" x))))
                                       (cake2::directory-files "webroot/js" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake2::initial-input))
                        (insert candidate))))))

(defvar cake2::source-css
  '((name . "Cake2 Css")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (if (string-match (concat "\\.css$") c)
                                            (setq c (replace-match "" nil nil c)))
                                        (concat "$this->Html->css('" c "');")))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (f-dir? (f-join cake2::app-path "webroot/css" x))))
                                       (cake2::directory-files "webroot/css" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake2::initial-input))
                        (insert candidate))))))

(defvar cake2::source-layouts
  '((name . "Cake2 Layout")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c) (concat "$this->layout = '" c "';")))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (f-dir? (f-join cake2::app-path "View/Layouts" x))))
                                       (cake2::directory-files "View/Layouts" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake2::initial-input))
                        (insert candidate))))))

(defvar cake2::source-elements
  '((name . "Cake2 Element")
    (candidates . (lambda ()
                    (mapcar (function (lambda (c)
                                        (if (string-match (concat "\\." cake2::view-extension "$") c)
                                            (setq c (replace-match "" nil nil c)))
                                        (concat "$this->element('" c "');")))
                            (remove-if (lambda (x) (or (string-match "~$\\|\\.$" x)
                                                       (f-dir? (f-join cake2::app-path "View/Elements" x))))
                                       (cake2::directory-files "View/Elements" t)))))
    (action
     ("Insert Code" . (lambda (candidate)
                        (delete-backward-char (length cake2::initial-input))
                        (insert candidate))))))

;;php-completion.el code
(defvar cake2::initial-input nil)
(defun cake2::get-initial-input ()
  (setq cake2::initial-input
        (buffer-substring-no-properties (point)
                                        (progn (save-excursion
                                                 (skip-syntax-backward "w_")
                                                 (point))))))

(defun cake2::complete ()
  "Insert CakePHP2 code."
  (interactive)
  (if (cake2::app-build)
      (cond
       ((cake2::is-controller-file)
        (anything
         '(cake2::source-layouts
           cake2::source-models)
         (cake2::get-initial-input) "Find Code: " nil))
       ((cake2::is-view-file)
        (anything
         '(cake2::source-js
           cake2::source-css
           cake2::source-elements)
         (cake2::get-initial-input) "Find Code: " nil))
       (t
        (anything
         '(cake2::source-js
           cake2::source-css
           cake2::source-elements
           cake2::source-layouts
           cake2::source-models)
         (cake2::get-initial-input) "Find Code: " nil))
       )))

(defun cake2::logs ()
  "Set logs list."
  (if (cake2::app-build)
      (mapcar
       (function (lambda (el)
                   (if (listp el) el(cons el el))))
       (f-glob "*.log" (f-join cake2::app-path "tmp/logs")))
    nil))

(defun cake2::tail-log (log)
  "Show log by \"tail\"."
  (interactive
   (list (completing-read "tail log: " (cake2::logs) nil t "debug.log")))
  (if (require 'tail nil t)             ;xcezx patch.
      (tail-file (f-join cake2::app-path "tmp/logs" log))
    (let ((logbuffer (concat "*" log "*")))
      (if (and (cake2::logs) (executable-find "tail"))
          (progn
            (unless (get-buffer logbuffer)
              (get-buffer-create logbuffer)
              (set-buffer logbuffer)
              (insert-before-markers (concat "tail -f" cake2::app-path "/tmp/logs/" log "\n"))
              (setq buffer-read-only t)
              (start-process "tail" logbuffer "tail" "-f" (concat cake2::app-path "/tmp/logs/" log)))
            (switch-to-buffer logbuffer))
        (message "Can't set log.")))))

;;; anything sources and functions

(when (require 'anything-show-completion nil t)
  (use-anything-show-completion 'anything-c-cake2-anything-only-function
                                '(length cake2::initial-input)))

(defvar cake2::candidate-function-name nil)

(defvar anything-c-cake2::po-file-buffer-name "*Cake2 Po*")

(defun cake2::build-source-cake2()
  "Build for anything-c-source-cake2"
  (unless
      (not (and (cake2::app-build) (executable-find "find") (executable-find "grep")))
    (call-process-shell-command
     (concat "find " cake2::app-path "/Controller -type f -name '*Controller.php' | xargs grep '[^_]function' --with-filename")
     nil (current-buffer))
    (goto-char (point-min))
    (while (re-search-forward (concat cake2::app-path "/Controller\\/\\(.+\\)Controller\.php:.*function *\\([^ ]+\\) *(.*).*$") nil t)
      (replace-match (concat (match-string 1) " / " (match-string 2))))
    (goto-char (point-max))
    (call-process-shell-command
     (concat "find " cake2::app-path "/Lib -type f -name '*Controller.php' | xargs grep '[^_]function' --with-filename")
     nil (current-buffer))
    (goto-char (point-min))
    (while (re-search-forward (concat cake2::app-path "/Lib\\/\\(.+\\)Controller\.php:.*function *\\([^ ]+\\) *(.*).*$") nil t)
      (replace-match (concat "Lib/" (match-string 1) " / " (match-string 2))))))

(defvar anything-c-source-cake2
  '((name . "Cake2 Switch")
    (init
     . (lambda ()
         (with-current-buffer (anything-candidate-buffer 'local)
           (cake2::build-source-cake2))))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake2::set-names)
    (action
     ("Switch to Contoroller" . (lambda (candidate)
                                  (anything-c-cake2::switch-to-controller)))
     ("Switch to View" . (lambda (candidate)
                           (anything-c-cake2::switch-to-view)))
     ("Switch to Model" . (lambda (candidate)
                            (anything-c-cake2::switch-to-model))))))

(defun anything-c-cake2::set-names (candidate)
  "Set names by display-to-real"
  (string-match "\\(.+\\) / \\(.+\\)" candidate)
  (setq cake2::plural-name (match-string 1 candidate))
  (setq cake2::action-name (match-string 2 candidate))
  (setq cake2::singular-name (cake-singularize cake2::plural-name))
  (setq cake2::camelize-name (cake-camelize (cake-snake cake2::singular-name)))
  (setq cake2::lower-camelized-action-name cake2::action-name)
  (setq cake2::snake-action-name (cake-snake cake2::action-name)))

(defun anything-c-cake2::switch-to-view ()
  "Switch to view."
  (let ((themed-dir "")
        (plural-name cake2::plural-name)
        (action-name cake2::action-name)
        (snake-action-name cake2::snake-action-name))
    (progn
      (cake2::app-build)
      (if (and (cake2::is-view-file) cake2::themed-name)
          (setq themed-dir (f-join "Themed" cake2::themed-name)))
      (cond
       ((cake2::find-file-if-exists (f-join cake2::app-path "View" themed-dir plural-name (concat snake-action-name "." cake2::view-extension)))
        t)
       ((cake2::find-file-if-exists (f-join cake2::app-path "View" themed-dir plural-name (concat snake-action-name ".ctp")))
        t)

       ((cake2::find-file-if-exists (f-join cake2::app-path "View" themed-dir plural-name (concat action-name "." cake2::view-extension)))
        t)
       ((cake2::find-file-if-exists (f-join cake2::app-path "View" themed-dir plural-name (concat action-name ".ctp")))
        t)

       ((cake2::find-file-if-exists (f-join cake2::app-path "View" plural-name (concat snake-action-name "." cake2::view-extension)))
        t)
       ((cake2::find-file-if-exists (f-join cake2::app-path "View" plural-name (concat snake-action-name ".ctp")))
        t)

       ((cake2::find-file-if-exists (f-join cake2::app-path "View" plural-name (concat action-name "." cake2::view-extension)))
        t)
       ((cake2::find-file-if-exists (f-join cake2::app-path "View" plural-name (concat action-name ".ctp")))
        t)

       ((y-or-n-p "Make new file?")
        (unless (f-dir? (f-join cake2::app-path "View" plural-name))
          (make-directory (f-join cake2::app-path "View" plural-name)))
        (find-file (f-join cake2::app-path "View" plural-name (concat action-name "." cake2::view-extension))))
       (t (message (format "Can't find %s" (f-join cake2::app-path "View" plural-name (concat action-name "." cake2::view-extension)))))))))

(defun anything-c-cake2::switch-to-controller ()
  "Switch to contoroller."
  (cake2::app-build)
  (if (f-exists? (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php")))
      (progn
        (find-file (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php")))
        (goto-char (point-min))
        (if (not (re-search-forward (concat "function[ \t]*" cake2::lower-camelized-action-name "[ \t]*\(") nil t))
            (progn
              (goto-char (point-min))
              (re-search-forward (concat "function[ \t]*" cake2::action-name "[ \t]*\(") nil t))))
    (if (f-exists? (f-join cake2::app-path (concat cake2::plural-name "Controller.php")))
        (progn
          (find-file (f-join cake2::app-path (concat cake2::plural-name "Controller.php")))
          (goto-char (point-min))
          (if (not (re-search-forward (concat "function[ \t]*" cake2::lower-camelized-action-name "[ \t]*\(") nil t))
              (progn
                (goto-char (point-min))
                (re-search-forward (concat "function[ \t]*" cake2::action-name "[ \t]*\(") nil t))))
      (if (y-or-n-p "Make new file?")
          (find-file (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php")))
        (message (format "Can't find %s" (f-join cake2::app-path "Controller" (concat cake2::plural-name "Controller.php"))))))))

(defun anything-c-cake2::switch-to-model ()
  "Switch to model."
  (cake2::app-build)
  (unless (cake2::find-file-if-exists (f-join cake2::app-path "Model" (concat cake2::camelize-name ".php")))
    (unless (cake2::find-file-if-exists (f-join cake2::app-path "Model" (concat cake2::singular-name ".php")))
      (if (y-or-n-p "Make new file?")
          (find-file (f-join cake2::app-path "Model" (concat cake2::camelize-name ".php")))
        (message (format "Can't find %s" (f-join cake2::app-path "Model" (concat cake2::camelize-name ".php"))))))))

(defun anything-c-cake2::switch-to-file-function (dir)
  "Switch to file and search function."
  (cake2::app-build)
  (if (not (f-exists? (f-join cake2::app-path dir (concat cake2::camelize-name ".php"))))
      (if (y-or-n-p "Make new file?")
          (find-file (f-join cake2::app-path dir (concat cake2::camelize-name ".php")))
        (message (format "Can't find %s" (f-join cake2::app-path dir (concat cake2::camelize-name ".php")))))
    (find-file (f-join cake2::app-path dir (concat cake2::camelize-name ".php")))
    (goto-char (point-min))
    (re-search-forward (concat "function[ \t]*" cake2::candidate-function-name "[ \t]*\(") nil t)))

(defvar anything-c-source-cake2-model-function
  '((name . "Cake2 Model Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake2::app-build) (executable-find "grep"))
             (with-current-buffer (anything-candidate-buffer 'local)
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake2::app-path
                        "Model/*.php --with-filename")
                nil (current-buffer))
               (goto-char (point-min))
               (while (not (eobp))
                 (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                     (goto-char (point-max))
                   (setq class-name (cake-camelize (cake-snake (match-string 1))))
                   (setq function-name (match-string 2))
                   (delete-region (point) (save-excursion (beginning-of-line) (point)))
                   (insert (concat class-name "->" function-name))
                   )))
           (with-current-buffer (anything-candidate-buffer 'local)
             (call-process-shell-command nil nil (current-buffer)))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake2::set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake2::switch-to-file-function "Model/")))
     ("Insert" . (lambda (candidate)
                   (insert candidate))))))

(defvar anything-c-source-cake2-component-function
  '((name . "Cake2 Component Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake2::app-build) (executable-find "grep"))
             (with-current-buffer (anything-candidate-buffer 'local)
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake2::app-path
                        "Controller/Component/*.php --with-filename")
                nil (current-buffer))
               (goto-char (point-min))
               (while (not (eobp))
                 (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                     (goto-char (point-max))
                   (setq class-name (cake-camelize (cake-snake (match-string 1))))
                   (setq function-name (match-string 2))
                   (delete-region (point) (save-excursion (beginning-of-line) (point)))
                   (insert (concat class-name "->" function-name))
                   )))
           (with-current-buffer (anything-candidate-buffer 'local)
             (call-process-shell-command nil nil (current-buffer)))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake2::set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake2::switch-to-file-function "Controller/Component/")))
     ("Insert" . (lambda (candidate)
                   (insert candidate))))))

(defvar anything-c-source-cake2-behavior-function
  '((name . "Cake2 Behavior Function Switch")
    (init
     . (lambda ()
         (if
             (and (cake2::app-build) (executable-find "grep"))
             (with-current-buffer (anything-candidate-buffer 'local)
               (call-process-shell-command
                (concat "grep '[^_]function' "
                        cake2::app-path
                        "Model/Behavior/*.php --with-filename")
                nil (current-buffer))
               (goto-char (point-min))
               (while (not (eobp))
                 (if (not (re-search-forward ".+\\/\\(.+\\)\.php:.*function *\\([^ ]+\\) *(.*).*" nil t))
                     (goto-char (point-max))
                   (setq class-name (cake-camelize (cake-snake (match-string 1))))
                   (setq function-name (match-string 2))
                   (delete-region (point) (save-excursion (beginning-of-line) (point)))
                   (insert (concat class-name "->" function-name))
                   )))
           (with-current-buffer (anything-candidate-buffer 'local)
             (call-process-shell-command nil nil (current-buffer)))
           )))
    (candidates-in-buffer)
    (display-to-real . anything-c-cake2::set-names2)
    (action
     ("Switch to Function" . (lambda (candidate)
                               (anything-c-cake2::switch-to-file-function "Model/Behavior/")))
     ("Insert" . (lambda (candidate)
                   (insert candidate))))))

(defun anything-c-cake2::set-names2 (candidate)
  "Set names by display-to-real"
  (string-match "\\(.+\\)->\\(.+\\)" candidate)
  (setq cake2::camelized-singular-name (match-string 1 candidate))
  (setq cake2::camelize-name cake2::camelized-singular-name)
  (setq cake2::candidate-function-name (match-string 2 candidate))
  (setq cake2::singular-name (cake-snake cake2::camelized-singular-name))
  candidate)

(defun anything-c-cake2::create-po-file-buffer ()
  "Create buffer from po file."
  (let ((anything-buffer (anything-candidate-buffer 'global)))
    (catch 'invalid-po-file
      (unless (anything-c-cake2::generate-po-file-buffer (f-join cake2::app-path "Locale" cake2::po-file-path))
        (message "Can't find po file: %s" (f-join cake2::app-path "Locale" cake2::po-file-path))
        (throw 'invalid-po-file nil))
      (with-current-buffer anything-buffer
        (set-syntax-table (with-current-buffer anything-current-buffer
                            (syntax-table)))
        (insert-buffer-substring anything-c-cake2::po-file-buffer-name)))))

(defun anything-c-cake2::generate-po-file-buffer (po-file)
  "Generate po file buffer"
  (when (and po-file
             (file-exists-p po-file)
             (file-regular-p po-file))
    (with-current-buffer (get-buffer-create anything-c-cake2::po-file-buffer-name)

      (erase-buffer)
      (insert-file-contents po-file)

      (goto-char (point-min))
      (while (re-search-forward "^[^m].*\n" nil t)
        (replace-match ""))

      (goto-char (point-min))
      (while (re-search-forward "^msgid \"\\(.*\\)\"\nmsgstr \"\\(.*\\)\"$" nil t)
        (replace-match "\\1 / \\2"))
      )
    t))

(defvar anything-c-source-cake2-po
  '((name . "Cake2 po file's msgid and msgstr")
    (init . (lambda ()
              (cake2::app-build)
              (setq path cake2::app-path)
              (anything-c-cake2::create-po-file-buffer)))
    (candidates-in-buffer)
    (action
     ("Insert __('msgid')." . (lambda (candidate)
                                (insert (concat "__('" (anything-c-cake2::get-msgid candidate) "')"))))
     ("Insert __('msgid',true)." . (lambda (candidate)
                                     (insert (concat "__('" (anything-c-cake2::get-msgid candidate) "',true)"))))
     ("Insert msgid." . (lambda (candidate)
                          (insert (anything-c-cake2::get-msgid candidate))))
     ("Insert msgstr." . (lambda (candidate)
                           (insert (anything-c-cake2::get-msgstr candidate))))
     ("Goto po file" . (lambda (candidate)
                         (find-file (concat path "/Locale/" cake2::po-file-path))
                         (goto-char (point-min))
                         (re-search-forward (concat "\"" (anything-c-cake2::get-msgid candidate) "\"") nil t))))))

(defvar anything-c-source-cake2-po-not-found
  '((name . "Create __()")
    (init . (lambda ()
              (cake2::app-build)
              (setq path cake2::app-path)))
    (dummy)
    (action
     ("Insert __('msgid')." . (lambda (candidate)
                                (insert (concat "__('" candidate "')"))))
     ("Insert __('msgid',true)." . (lambda (candidate)
                                     (insert (concat "__('" candidate "',true)"))))
     ("Insert msgid." . (lambda (candidate)
                          (insert candidate)))
     ("Goto po file" . (lambda (candidate)
                         (find-file (f-join path "Locale" cake2::po-file-path))
                         (goto-char (point-max)))))))

(defun anything-c-cake2::get-msgid (candidate)
  "Set msgid"
  (progn
    (string-match "\\(.+\\) /" candidate)
    (match-string 1 candidate)))

(defun anything-c-cake2::get-msgstr (candidate)
  "Set msgstr"
  (progn
    (string-match "/ \\(.+\\)$" candidate)
    (match-string 1 candidate)))

(defun anything-c-cake2-anything-only-source-cake2 ()
  "anything only anything-c-source-cake2 and anything-c-source-cake2-model-function."
  (interactive)
  (anything (list anything-c-source-cake2
                  anything-c-source-cake2-model-function
                  anything-c-source-cake2-component-function
                  anything-c-source-cake2-behavior-function)
            nil "Find CakePHP2 Sources: " nil nil))

(defun anything-c-cake2-anything-only-function ()
  "anything only anything-c-source-cake2-function."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything (list anything-c-source-cake2-model-function
                    anything-c-source-cake2-component-function
                    anything-c-source-cake2-behavior-function) initial-pattern "Find Cake2 Functions: " nil)))

(defun anything-c-cake2-anything-only-model-function ()
  "anything only anything-c-source-cake2-model-function."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything '(anything-c-source-cake2-model-function) initial-pattern "Find Model Functions: " nil)))

(defun anything-c-cake2-anything-only-po ()
  "anything only anything-c-source-cake2-po."
  (interactive)
  (let* ((initial-pattern (regexp-quote (or (thing-at-point 'symbol) ""))))
    (anything (list anything-c-source-cake2-po
                    anything-c-source-cake2-po-not-found)
              initial-pattern "Find Msgid And Msgstr: " nil)))

(defconst cake2::snippets-dir (file-name-directory (or (buffer-file-name)
                                                       load-file-name)))

;;;###autoload
(defun cake2::snippets-initialize ()
  (let ((snip-dir (expand-file-name "snippets" cake2::snippets-dir)))
    (add-to-list 'yas-snippet-dirs snip-dir t)
    (yas-load-directory snip-dir)))

;;;###autoload
(eval-after-load 'yasnippet
  '(cake2::snippets-initialize))

;; Tests
(dont-compile
  (when (fboundp 'expectations)
    (expectations
      (desc "init test1")
      (expect t
        (setq cake2::test-default-dir default-directory)
        t)
      (expect t
        (setq cake2::test-dir (f-expand "t/test1" cake2::test-default-dir))
        t)
      (expect t
        (global-cake2 t)
        t)
      (desc "Inflector test")
      (expect "Lib/Admin/App"
        (cake-singularize "Lib/Admin/App"))
      (expect "Lib/Admin/Post"
        (cake-singularize "Lib/Admin/Posts"))
      (desc "MVC switch test")
      (expect t
        (find-file (f-expand "app/Lib/Controller/Admin/AdminAppController.php" cake2::test-dir))
        (cake2::maybe))
      (expect t
        (find-file (f-expand "app/Controller/PostsController.php" cake2::test-dir))
        (cake2::maybe))
      (expect (f-expand "app/Model/Post.php" cake2::test-dir)
        (cake2::switch-to-model)
        (f-expand (buffer-file-name)))
      (expect (f-expand "app/Controller/PostsController.php" cake2::test-dir)
        (cake2::switch-to-controller)
        (f-expand (buffer-file-name)))
      (expect (f-expand "app/View/Posts/add.ctp" cake2::test-dir)
        (goto-char (point-min))
        (re-search-forward "function add()" nil t)
        (cake2::switch-to-view)
        (f-expand (buffer-file-name)))
      (expect (f-expand "app/Controller/PostsController.php" cake2::test-dir)
        (cake2::switch-to-controller)
        (f-expand (buffer-file-name)))
      (expect t
        (stringp (executable-find "grep")))
      (expect t
        (with-temp-buffer
          (cake2::build-source-cake2)
          (integerp (string-match "Comments / index" (buffer-string)))))
      (expect t
        (with-temp-buffer
          (cake2::build-source-cake2)
          (integerp (string-match "Posts / index" (buffer-string)))))
      (expect t
        (with-temp-buffer
          (cake2::build-source-cake2)
          (integerp (string-match "App / beforeFilter" (buffer-string)))))
      (expect t
        (with-temp-buffer
          (cake2::build-source-cake2)
          (integerp (string-match "Admin/AdminPosts / index" (buffer-string)))))
      (expect t
        (with-temp-buffer
          (cake2::build-source-cake2)
          (integerp (string-match "Lib/Controller/Admin/AdminApp / beforeFilter" (buffer-string)))))
      (expect "Posts"
        (anything-c-cake2::set-names "Posts / add")
        cake2::plural-name)
      (expect "Admin/AdminPosts"
        (anything-c-cake2::set-names "Admin/AdminPosts / index")
        cake2::plural-name)
      (expect "index"
        (anything-c-cake2::set-names "Admin/AdminPosts / index")
        cake2::action-name)
      (expect "Admin/AdminPost"
        (anything-c-cake2::set-names "Admin/AdminPosts / index")
        cake2::singular-name)
      (expect t
        (find-file (f-expand "app/Controller/NoActionController.php" cake2::test-dir))
        (cake2::maybe))
      (expect (f-expand "app/Controller/NoActionController.php" cake2::test-dir)
        (goto-char (point-max))
        (cake2::switch-to-view)
        (f-expand (buffer-file-name)))
      (desc "init test2")
      (expect t
        (setq cake2::test-dir (f-expand "t/test2" cake2::test-default-dir))
        t)
      (expect t
        (global-cake2 t)
        t)
      (expect t
        (find-file (f-expand "myapp/Controller/PostsController.php" cake2::test-dir))
        (cake2::maybe))
      (desc ".cake load test")
      (expect (f-expand "myapp" cake2::test-dir)
        cake2::app-path)
      (expect "{\"cake\":\"..\\/Vendor\\/cakephp\\/cakephp\\/\", \"build_path\":{\"plugins\":[\"..\\/Plugin\\/\"]}}"
        (json-encode (cake2::read-dot-cake)))
      (expect "../Vendor/cakephp/cakephp/"
        (let ((dot-cake-hash (ht<-alist (cake2::read-dot-cake))))
          (ht-get dot-cake-hash `cake)))
      (expect "../Vendor/cakephp/cakephp/"
        (let ((dot-cake-hash (ht<-alist (cake2::read-dot-cake))))
          (ht-get dot-cake-hash (intern-soft "cake"))))
      (expect `((plugins . ["../Plugin/"]))
        (let ((dot-cake-hash (ht<-alist (cake2::read-dot-cake))))
          (ht-get dot-cake-hash `build_path)))
      (expect nil
        (let ((dot-cake-hash (ht<-alist (cake2::read-dot-cake))))
          (ht-get dot-cake-hash `no_path)))
      (expect nil
        (let ((dot-cake-hash (ht<-alist (cake2::read-dot-cake))))
          (ht-get dot-cake-hash nil)))
      (desc "build pathes test")
      (expect `("Plugin" "../Plugin/")
        (cake2::set-build-pathes)
        (ht-get cake2::build-pathes "Plugin"))
      )))

(provide 'cake2)

;;; end
;;; cake2.el ends here
