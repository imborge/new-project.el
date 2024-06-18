;;; new-project.el --- Simple and powerful project templates -*- lexical-binding: t -*-

;; Copyright (C) 2024  Børge André Jensen (imborge@proton.me)

;; Author: Børge André Jensen (imborge@proton.me)
;; Version: 20240617.4
;; Package-Requires: ((emacs "26.1") (templatel "20210902.228"))
;; URL: https://github.com/imborge/new-project.el
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

(defgroup new-project nil
  "Settings for `new-project'."
  :link '(url-link "https://github.com/imborge/new-project.el")
  :group 'tools)

(defcustom new-project-projects-dir nil
  "The default directory where new projects will be created."
  :group 'new-project
  :type '(string))

(defcustom new-project-templates-dir (expand-file-name "new-project-templates" user-emacs-directory)
  "Directory containing project templates."
  :group 'new-project
  :type '(string))

(defcustom new-project-bookmark-name "new-project-last-created-project"
  "The name of the bookmark record containing the last created project."
  :group 'new-project
  :type '(string))

(defface new-project-logdate-face
  '((t :inherit font-lock-type-face))
  "Face for showing the date in new-project log buffer."
  :group 'new-project)

(defface new-project-logerror-level-face
  '((t :inherit error))
  "Face for showing the `error' level in new-project log buffer."
  :group 'new-project)

(defface new-project-logwarn-level-face
  '((t :inherit warning))
  "Face for showing the `warn' level in new-project log buffer."
  :group 'new-project)

(defface new-project-loginfo-level-face
  '((t :inherit info-node))
  "Face for showing the `info' level in new-project log buffer."
  :group 'new-project)

(defface new-project-logdebug-level-face
  '((t :inherit shadow))
  "Face for showing the `debug' level in new-project log buffer."
  :group 'new-project)

(defvar new-project-logbuffer-name "*new-project-log*"
  "Name of buffer used for logging new-project events.")

(defvar new-project-log-level 'info
  "Lowest typ of message to be logged.")

(defun new-project-logbuffer ()
  "Return the buffer for `new-project-log', creating if not exist."
  (if-let ((buffer (get-buffer new-project-logbuffer-name)))
      buffer
    (with-current-buffer (generate-new-buffer new-project-logbuffer-name)
      (special-mode)
      (current-buffer))))

(defun new-project--log-level (level)
  "Numeric values for log LEVEL."
  (cl-case level
    (debug -10)
    (info 0)
    (warn 10)
    (error 20)
    (otherwise -10)))

(defun new-project-log (level fmt &rest objects)
  "Display a log message if LEVEL is >= `new-project-log-level'.
FMT is a format string that may print OBJECTS."
  (let ((log-buffer (new-project-logbuffer))
        (log-level-face (cl-case level
                          (debug 'new-project-logdebug-level-face)
                          (info 'new-project-loginfo-level-face)
                          (warn 'new-project-logwarn-level-face)
                          (error 'new-project-logerror-level-face)))
        (inhibit-read-only t))
    (when (>= (new-project--log-level level)
              (new-project--log-level new-project-log-level))
      (with-current-buffer log-buffer
        (goto-char (point-max))
        (insert
         (format (concat "[" (propertize "%s" 'face 'new-project-logdate-face) "] "
                         "[" (propertize "%s" 'face 'log-level-face) "]: %s\n")
                 (format-time-string "%Y-%m-%d %H:%M:%S")
                 level
                 (apply #'format fmt objects)))))))

;;; Commands

(defun new-project-goto-last-created-project ()
  "Jump to the bookmark for the last created project."
  (interactive)
  (if-let ((bm (new-project-last-created-project)))
      (bookmark-jump (new-project-last-created-project))
    (new-project-log info "No bookmark saved for last created project.")))

(defun new-project-command ()
  "Interactively create a new project."
  (interactive)
  (let* ((parent-dir (read-directory-name "Parent dir: "
                                          new-project-projects-dir))
         (project-name (read-string "Project name: "))
         (project-template (completing-read "Choose template: "
                                            (lambda ()
                                              (new-project-find-project-templates new-project-templates-dir)))))
    (new-project-create parent-dir
                        project-name
                        (expand-file-name project-template new-project-templates-dir)) ))

;;;; Functions

(defun new-project-sanitize-project-name (project-name &optional keep-case)
  "Sanitize PROJECT-NAME and converting to lowercase.

Replacing spaces with underscores and removing all characters
that are not a-z, A-Z, 0-9, -, or _, and converting to lowercase
unless KEEP-CASE is non-nil."
  (let ((sanitized (replace-regexp-in-string " " "_" project-name)))
    (setq sanitized (replace-regexp-in-string "[^a-zA-Z0-9_-]" "" sanitized))
    (if keep-case
        sanitized
      (downcase sanitized))))

(defun new-project-find-project-templates (templates-dir)
  "Get a list of project templates in TEMPLATES-DIR."
  (let ((dir (file-name-as-directory templates-dir)))
    (cl-remove-if-not (lambda (f)
                        (and (file-directory-p (concat dir f))
                             (not (member f '("." "..")))))
                      (directory-files dir))))

(defun new-project-list-template-files (dir)
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

(defun new-project--make-parent-dirs (file)
  "Create parent dirs for FILE if not existeng."
  (let ( (dir (file-name-directory file)))
    (unless (file-exists-p dir)
      (make-directory dir t))))

(defun new-project-write-file (text file-path base-path)
  "Write TEXT to FILE-PATH inside BASE-PATH, creating necessary directories."
    (let* ((absolute-path (expand-file-name file-path base-path))
         (dir (file-name-directory absolute-path)))
    ;; Ensure directory exists
    (new-project--make-parent-dirs dir)
    ;; Write text to file
    (with-temp-buffer
      (insert text)
      (write-file absolute-path))))

(defun new-project-load-template (dir)
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
    (push `(files . ,(new-project-list-template-files dir))
          template-data)
    template-data))

(defun new-project-template-val (sym template-data)
  "Return the value of SYM in the VARS alist of TEMPLATE-DATA."
  (alist-get sym (alist-get 'vars template-data)))

(defun new-project-eval-template-vars (vars)
  "Return alist of (SYM . VAL) pairs by calling `eval' on every EXPR in VARS.

VARS is alist of (SYM . EXPR) pairs."
  (mapcar (lambda (element)
            (cons (car element)
                  (eval
                   (cdr element))))
          vars))

(defun new-project-eval-after (template-data)
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
  (let ((default-directory (new-project-template-val 'project-dir template-data)))
    (dolist (expr (alist-get 'after template-data))
      (if-let ((result (eval expr)))
          (when (functionp result)
            (funcall result template-data))))))

(defun new-project--empty-file-p (file)
  "Return t if FILE is empty."
  (zerop (or (file-attribute-size (file-attributes file)) 0)))

(defun new-project-create (parent-dir project-name project-template)
  "Create a new project in PARENT-DIR named PROJECT-NAME using PROJECT-TEMPLATE."
  (let ((project-dir (expand-file-name (new-project-sanitize-project-name project-name) parent-dir)))
    (if (file-exists-p project-dir)
        (new-project-log 'error "Project directory already exists: %s" project-dir )
      (new-project-log 'info "Creating new project using '%s' as template." project-template)
      (let ((vars `((project-name . ,project-name)
                    (sanitized-project-name . ,(new-project-sanitize-project-name project-name))
                    (parent-dir . ,parent-dir)
                    (project-dir . ,project-dir)))
            (template-data (new-project-load-template project-template)))

        (setf (alist-get 'vars template-data)
              (append vars
                      (new-project-eval-template-vars (alist-get 'vars template-data))))

        (dolist (file (alist-get 'files template-data))
          (let ((source-file (expand-file-name file project-template))
                (dest-file (expand-file-name file project-dir)))
            (new-project--make-parent-dirs dest-file)
            (if (new-project--empty-file-p source-file)
                (progn
                  (new-project-log 'warning "Source file '%s' is empty." source-file)
                  (copy-file source-file dest-file))
              (let ((contents
                     (templatel-render-file (expand-file-name file project-template) vars)))
                (new-project-write-file contents file project-dir)))
            (new-project-log 'info "File '%s' created." dest-file)))

        (setf  (alist-get 'vars template-data) vars)
        (new-project-eval-after template-data)
        ;; create bookmark
        (bookmark-store new-project-bookmark-name
                        `((filename . ,project-dir)
                          (position . 1)
                          (name . ,project-name)
                          (template . ,project-template))
                        nil)))))

(defun new-project-last-created-project ()
  "Return the bookmark for the last created project."
  (bookmark-get-bookmark new-project-bookmark-name))

(provide 'new-project)
;;; new-project.el ends here
