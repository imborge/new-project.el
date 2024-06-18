;;; prosca.el --- Project Scaffold -*- lexical-binding: t -*-

;; Copyright (C) 2024  Børge André Jensen (imborge@proton.me)

;; Author: Børge André Jensen (imborge@proton.me)
;; Version: 20240617.4
;; Package-Requires: ((emacs "26.1") (templatel "20210902.228"))
;; URL: https://github.com/imborge/prosca/
;; Keywords: tools

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; todo

;;; Code:

(require 'templatel)
(require 'bookmark)
(require 'dired)

;;;; Customization

(defgroup prosca nil
  "Settings for `prosca'."
  :link '(url-link "https://github.com/imborge/prosca")
  :group 'tools)

(defcustom prosca-projects-dir nil
  "The default directory where new projects will be created."
  :group 'prosca
  :type '(string))

(defcustom prosca-templates-dir (expand-file-name "prosca-templates" user-emacs-directory)
  "Directory containing project templates."
  :group 'prosca
  :type '(string))

(defcustom prosca-bookmark-name "prosca-last-created-project"
  "The name of the bookmark record containing the last created project."
  :group 'prosca
  :type '(string))

(defface prosca-logdate-face
  '((t :inherit font-lock-type-face))
  "Face for showing the date in prosca log buffer."
  :group 'prosca)

(defface prosca-logerror-level-face
  '((t :inherit error))
  "Face for showing the `error' level in prosca log buffer."
  :group 'prosca)

(defface prosca-logwarn-level-face
  '((t :inherit warning))
  "Face for showing the `warn' level in prosca log buffer."
  :group 'prosca)

(defface prosca-loginfo-level-face
  '((t :inherit info-node))
  "Face for showing the `info' level in prosca log buffer."
  :group 'prosca)

(defface prosca-logdebug-level-face
  '((t :inherit shadow))
  "Face for showing the `debug' level in prosca log buffer."
  :group 'prosca)

(defvar prosca-logbuffer-name "*prosca-log*"
  "Name of buffer used for logging prosca events.")

(defvar prosca-log-level 'info
  "Lowest typ of message to be logged.")

(defun prosca--logbuffer ()
  "Return the buffer for `prosca--log', creating if not exist."
  (if-let ((buffer (get-buffer prosca-logbuffer-name)))
      buffer
    (with-current-buffer (generate-new-buffer prosca-logbuffer-name)
      (special-mode)
      (current-buffer))))

(defun prosca--log-level (level)
  "Numeric values for log LEVEL."
  (cl-case level
    (debug -10)
    (info 0)
    (warn 10)
    (error 20)
    (otherwise -10)))

(defun prosca--log (level fmt &rest objects)
  "Display a log message if LEVEL is >= `prosca-log-level'.
FMT is a format string that may print OBJECTS."
  (let ((log-buffer (prosca--logbuffer))
        (log-level-face (cl-case level
                          (debug 'prosca-logdebug-level-face)
                          (info 'prosca-loginfo-level-face)
                          (warn 'prosca-logwarn-level-face)
                          (error 'prosca-logerror-level-face)))
        (inhibit-read-only t))
    (when (>= (prosca--log-level level)
              (prosca--log-level prosca-log-level))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert
         (format (concat "[" (propertize "%s" 'face 'prosca-logdate-face) "] "
                         "[" (propertize "%s" 'face 'log-level-face) "]: %s\n")
                 (format-time-string "%Y-%m-%d %H:%M:%S")
                 level
                 (apply #'format fmt objects)))))))

;;; Commands

(defun prosca-goto-last-created-project ()
  "Jump to the bookmark for the last created project."
  (interactive)
  (if-let ((bm (prosca-last-created-project)))
      (bookmark-jump bm)
    (prosca-log info "No bookmark saved for last created project.")))

(defun prosca-create-project ()
  "Interactively create a new project."
  (interactive)
  (let* ((parent-dir (read-directory-name "Parent dir: "
                                          prosca-projects-dir))
         (project-name (read-string "Project name: "))
         (project-template (completing-read "Choose template: "
                                            (lambda ()
                                              (prosca--find-project-templates prosca-templates-dir)))))
    (prosca--create parent-dir
                    project-name
                    (expand-file-name project-template prosca-templates-dir)) ))

(defun prosca--sanitize-project-name (project-name &optional keep-case)
  "Sanitize PROJECT-NAME and converting to lowercase.

Replacing spaces with underscores and removing all characters
that are not a-z, A-Z, 0-9, -, or _, and converting to lowercase
unless KEEP-CASE is non-nil."
  (let ((sanitized (replace-regexp-in-string " " "_" project-name)))
    (setq sanitized (replace-regexp-in-string "[^a-zA-Z0-9_-]" "" sanitized))
    (if keep-case
        sanitized
      (downcase sanitized))))

(defun prosca--find-project-templates (templates-dir)
  "Get a list of project templates in TEMPLATES-DIR."
  (let ((dir (file-name-as-directory templates-dir)))
    (cl-remove-if-not (lambda (f)
                        (and (file-directory-p (concat dir f))
                             (not (member f '("." "..")))))
                      (directory-files dir))))

(defun prosca--list-template-files (dir)
  "List all files in DIR recursively, returning relative paths."
  (let ((result '()))
    (cl-labels ((recursively-list-files (directory)
                  (dolist (file (directory-files directory t))
                    (unless (member (file-name-nondirectory file) '("." ".." "template.el"))
                      (if (file-directory-p file)
                          (recursively-list-files file)
                        (push (file-relative-name file dir) result))))))
      (recursively-list-files dir))
    result))

(defun prosca--make-parent-dirs (file)
  "Create parent dirs for FILE if not existeng."
  (let ( (dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun prosca--write-file (text file-path base-path)
  "Write TEXT to FILE-PATH inside BASE-PATH, creating necessary directories."
  (let* ((absolute-path (expand-file-name file-path base-path))
         (dir (file-name-directory absolute-path)))
    ;; Ensure directory exists
    (prosca--make-parent-dirs dir)
    ;; Write text to file
    (with-temp-buffer
      (insert text)
      (write-file absolute-path))))

(defun prosca--load-template (dir)
  "Load DIR as a template.  Return alist representing the template.

If DIR contains a file named template.el, load this into the
returning alist.  Supported keys are:

- VARS
- AFTER

The returned alist has the form

    ((VARS . ALIST)
     (AFTER . LIST)
     (FILES . LIST))

VARS is an alist of (SYM . EXPR) pairs where EXPR will be evaluated and injected
in the template FILES as the value of SYM.

AFTER is a list of expressions to run after a new project has been created.

FILES is a list of filenames, relative to DIR.

Here's an example:

    ((vars . ((author . (read-string \"Author: \")))
              (copyright . \"2024\"))
     (after . ((async-shell-command \"git init\")))
     (files . (\"README.org\"
               \".gitignore\"
               \"src/main.rs\")))"
  (let* ((template-data-file-name (expand-file-name "template.el" dir))
         (template-data (when (file-exists-p template-data-file-name)
                          (with-temp-buffer
                            (insert-file-contents template-data-file-name)
                            (read (buffer-string))))))
    (push `(files . ,(prosca--list-template-files dir))
          template-data)
    template-data))

(defun prosca--template-val (sym template-data)
  "Return the value of SYM in the VARS alist of TEMPLATE-DATA."
  (alist-get sym (alist-get 'vars template-data)))

(defun prosca--eval-template-vars (vars)
  "Return alist of (SYM . VAL) pairs by calling `eval' on every EXPR in VARS.

VARS is alist of (SYM . EXPR) pairs."
  (mapcar (lambda (element)
            (cons (car element)
                  (eval
                   (cdr element))))
          vars))

(defun prosca--eval-after (template-data)
  "Set `default-directory' to PROJECT-DIR and run `eval' on every EXPR in AFTER.

TEMPLATE-DATA is alist in the form of:

    ((VARS . ALIST)
     (FILES . LIST)
     (AFTER . LIST))

VARS is alist of (SYM . VAL) pairs injected and to the template FILES.

PROJECT-DIR is a key in VARS containg the directory of the newly
created project.

AFTER is list of EXPR to be evaluated for side effects after a
new project is created.

If the result of evaluating EXPR is a function, the function is
called with TEMPLATE-DATA as an argument."
  (let ((default-directory (prosca--template-val 'project-dir template-data)))
    (dolist (expr (alist-get 'after template-data))
      (if-let ((result (eval expr)))
          (when (functionp result)
            (funcall result template-data))))))

(defun prosca--empty-file-p (file)
  "Return t if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defun prosca--create (parent-dir project-name project-template)
  "Create a new project in PARENT-DIR named PROJECT-NAME using PROJECT-TEMPLATE."
  (let ((project-dir (expand-file-name (prosca--sanitize-project-name project-name) parent-dir)))
    (if (file-exists-p project-dir)
        (prosca--log 'error "Project directory already exists: %s" project-dir )
      (prosca--log 'info "Creating new project using '%s' as template." project-template)
      (let ((vars `((project-name . ,project-name)
                    (sanitized-project-name . ,(prosca--sanitize-project-name project-name))
                    (parent-dir . ,parent-dir)
                    (project-dir . ,project-dir)))
            (template-data (prosca--load-template project-template)))

        (setf (alist-get 'vars template-data)
              (append vars
                      (prosca--eval-template-vars (alist-get 'vars template-data))))

        (dolist (file (alist-get 'files template-data))
          (let ((source-file (expand-file-name file project-template))
                (dest-file (expand-file-name file project-dir)))
            (prosca--make-parent-dirs dest-file)
            (if (prosca--empty-file-p source-file)
                (progn
                  (prosca--log 'warning "Source file '%s' is empty." source-file)
                  (copy-file source-file dest-file))
              (let ((contents
                     (templatel-render-file (expand-file-name file project-template) vars)))
                (prosca--write-file contents file project-dir)))
            (prosca--log 'info "File '%s' created." dest-file)))

        (setf  (alist-get 'vars template-data) vars)
        (prosca--eval-after template-data)
        ;; create bookmark
        (bookmark-store prosca-bookmark-name
                        `((filename . ,project-dir)
                          (position . 1)
                          (name . ,project-name)
                          (template . ,project-template))
                        nil)))))

(defun prosca--last-created-project ()
  "Return the bookmark for the last created project."
  (bookmark-get-bookmark prosca-bookmark-name))

(provide 'prosca)
;;; prosca.el ends here
