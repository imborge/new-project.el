;;; prosca-test.el --- Simple and powerful project templates -*- lexical-binding: t -*-

;; Copyright (C) 2024  Børge André Jensen (imborge@proton.me)

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
(require 'prosca)

(defmacro with-temp-dir (temp-dir &rest body)
  "Create temporary directory, binding it to TEMP-DIR, run BODY and delete TEMP-DIR when completed."
  `(let ((,temp-dir (make-temp-file "" t)))
     (unwind-protect
         (progn
           ,@body))
     (delete-directory ,temp-dir t)))

(ert-deftest prosca--load-template ()
  "Tests that `prosca--load-template' returns alist describing template."
  (should (equal '((files . ("README.org")))
                 (prosca--load-template "./test/templates/test1/"))))


(ert-deftest prosca--sanitize-project-name ()
  "Tests that `prosca--sanitize-project-name' correctly sanitize project names."
  (should (equal "my_very_c00l_prject"
                 (prosca--sanitize-project-name "My! Very C00l prøject."))))

(ert-deftest prosca--find-project-templates ()
  "Tests that it lists all subdirectories of templates-dir as templates."
  (let ((expected (sort '("test1" "test2" "rust") #'string<)))
    (should (equal expected (sort (prosca--find-project-templates "./test/templates") #'string<)))))

(ert-deftest prosca--list-template-files ()
  (let ((files '("README.org" "src/main.rs")))
    (should (cl-every (lambda (item) (seq-contains-p files item))
                      (prosca--list-template-files "./test/templates/rust")))))

(ert-deftest prosca--eval-template-vars ()
  (should (equal '((one . 1)
                   (five .  5))
                 (prosca--eval-template-vars '((one . 1)
                                               (five . (+ 2 3)))))))

(ert-deftest prosca--create ()
  "Tests that it creates the files in project dir."
  (with-temp-dir
   parent-dir
   (prosca--create parent-dir "test project" "./test/templates/rust")
   (should (file-exists-p (expand-file-name "test_project/src/main.rs" parent-dir)))
   (should (file-exists-p (expand-file-name "test_project/README.org" parent-dir)))))

(ert-deftest prosca--eval-after ()
  "Tests that `prosca--eval-after' can eval and run lambdas."
  (with-temp-dir
   parent-dir
   (let* (;; (data (ne))
          (project-name-sanitized "a_project")
          (data (prosca--eval-after `((vars . ((project-dir . ,(expand-file-name project-name-sanitized parent-dir))))
                                      (after . ((lambda (data)
                                                  (let ((file (expand-file-name "after" (prosca--template-val 'project-dir data))))
                                                    (prosca--make-parent-dirs file)
                                                    (write-file file)))))))))
     (should (file-exists-p (expand-file-name "a_project/after" parent-dir))))))

(provide 'prosca-test)
;;; prosca-test.el ends here
