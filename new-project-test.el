;;; new-project-test.el -- $description -*- lexical-binding: t -*-

;; Copyright (C) 2024  Børge André Jensen (imborge@proton.me)

;; Author: Børge André Jensen (imborge@proton.me)
;; Maintainer: Børge André Jensen (imborge@proton.me)
;; Version: 20240616.1
;; Package-Requires: ((templatel "20210902.228"))
;; URL: https://github.com/imborge/new-project.el

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
;;; Code:
(require 'ert)
(require 'new-project)

(defmacro with-temp-dir (temp-dir &rest body)
  "Create temporary directory, binding it to TEMP-DIR, run BODY and delete TEMP-DIR when completed."
  `(let ((,temp-dir (make-temp-file "" t)))
     (unwind-protect
         (progn
           ,@body))
     (delete-directory ,temp-dir t)))

(ert-deftest new-project-load-template ()
  "Tests that `new-project-load-template' returns alist describing template."
  (should (equal '((files . ("README.org")))
                 (new-project-load-template "./test/templates/test1/"))))


(ert-deftest new-project-sanitize-project-name ()
  "Tests that `new-project-sanitize-project-name' correctly sanitize project names."
  (should (equal "my_very_c00l_prject"
                 (new-project-sanitize-project-name "My! Very C00l prøject."))))

(ert-deftest new-project-test-- ()
  "Template test"
  (should t))

(ert-deftest new-project-find-project-templates ()
  "Tests that it lists all subdirectories of templates-dir as templates."
  (let ((expected (sort '("test1" "test2" "rust") #'string<)))
    (should (equal expected (sort (new-project-find-project-templates "./test/templates") #'string<)))))

(ert-deftest new-project-list-template-files ()
  (let ((files '("README.org" "src/main.rs")))
    (should (cl-every (lambda (item) (seq-contains-p files item))
                      (new-project-list-template-files "./test/templates/rust")))))

(ert-deftest new-project-eval-template-vars ()
  (should (equal '((one . 1)
                   (five .  5))
                 (new-project-eval-template-vars '((one . 1)
                                                  (five . (+ 2 3)))))))

(ert-deftest new-project--create ()
  "Tests that it creates the files in project dir."
  (with-temp-dir
   parent-dir
   (new-project-new-project--create parent-dir "test project" "./test/templates/rust")
   (should (file-exists-p (expand-file-name "test_project/src/main.rs" parent-dir)))
   (should (file-exists-p (expand-file-name "test_project/README.org" parent-dir)))))

(ert-deftest new-project-eval-after ()
  "Tests that `new-project-eval-after' can eval and run lambdas."
  (with-temp-dir
   parent-dir
   (let* (;; (data (ne))
          (project-name-sanitized "a_project")
          (data (new-project-eval-after `((vars . ((project-dir . ,(expand-file-name project-name-sanitized parent-dir))))
                                          (after . ((lambda (data)
                                                      (let ((file (expand-file-name "after" (new-project-template-val 'project-dir data))))
                                                        (new-project-make-parent-dirs file)
                                                        (write-file file)))))))))
     (should (file-exists-p (expand-file-name "a_project/after" parent-dir))))))

(provide 'new-project-test)
;;; new-project-test.el ends here
