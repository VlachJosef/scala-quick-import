(ert-deftest sqi:is-import-present ()
  "Import of Application - simple"
  (sqi:assert-import "import com.Application\n" 24))

(ert-deftest sqi:is-import-present-2 ()
  "Import of Application - in curly braces without spaces"
  (sqi:assert-import "import com.{Application}\n" 25))

(ert-deftest sqi:is-import-present-3 ()
  "Import of Application - in curly braces with spaces"
  (sqi:assert-import "import com.{ Application }\n" 26))

(ert-deftest sqi:is-import-present-4 ()
  "Import of Application - in curly braces as first member"
  (sqi:assert-import "import com.{Application, Config}\n" 25))

(ert-deftest sqi:is-import-present-5 ()
  "Import of Application - in curly braces as second member"
  (sqi:assert-import "import com.{Config,Application}\n" 32))

(ert-deftest sqi:is-import-present-6 ()
  "Import of Application - in curly braces as second member with space"
  (sqi:assert-import "import com.{Config, Application}\n" 33))

(defun sqi:assert-import (content len)
  (with-temp-buffer
    (insert content)
    (should (equal (scala-quick-import:is-import-present "Application") len))))

(defun sqi:assert-normalize (input output)
  (should (equal (scala-quick-import:normalize-import input) output)))

(ert-deftest sqi-normalize-import-1 ()
  (sqi:assert-normalize "com.Application" '("com.Application")))

(ert-deftest sqi-normalize-import-2 ()
  (sqi:assert-normalize "com.{Application}" '("com.Application")))

(ert-deftest sqi-normalize-import-3 ()
  (sqi:assert-normalize "com.{Application, Config}" '("com.Config" "com.Application")))

(ert-deftest sqi-normalize-import-4 ()
  (sqi:assert-normalize "com.foo.bar._" '("com.foo.bar._")))

(ert-deftest sqi-normalize-import-5 ()
  (sqi:assert-normalize "com.{{Bad}}" '()))

(ert-deftest sqi-normalize-and-sort-1 ()
  (should (equal (scala-quick-import:normalize-and-sort
                  '("com.Application" "foo.Application" "bar.Application" "foo.bar.Application") "Application")
                 '("bar.Application" "com.Application" "foo.Application" "foo.bar.Application"))))

(ert-deftest sqi-normalize-and-sort-2 ()
  (should (equal (scala-quick-import:normalize-and-sort '("com.{Application, Config}") "Application") '("com.Application"))))

(ert-deftest sqi-scala-quick-import:search-import-body-1 ()
  (with-current-buffer (find-file-noselect "test.scala")
    (insert "package foo.bar

import com.model.applicant.Application
")
    (scala-quick-import:search-import-body "AnswerMap" (lambda () "import com.model.applicant.AnswerMap"))
    (should (equal (buffer-string) "package foo.bar

import com.model.applicant.{ AnswerMap, Application }
"))))

(ert-deftest sqi-scala-quick-import:search-import-body-2 ()
  (with-current-buffer (find-file-noselect "test2.scala")
    (insert "package foo.bar

import com.model.applicant.Application
")
    (scala-quick-import:search-import-body "AnswerMap" (lambda () "import com.model.AnswerMap"))
    (should (equal (buffer-string) "package foo.bar

import com.model.AnswerMap
import com.model.applicant.Application
"))))

(ert-deftest sqi-scala-quick-import:search-import-body-3 ()
  (with-current-buffer (find-file-noselect "test3.scala")
    (insert "package foo.bar

import com.model.applicant.Application
")
    (scala-quick-import:search-import-body "AnswerMap" (lambda () "import com.model.applicant.{AnswerMap, Application}"))
    (should (equal (buffer-string) "package foo.bar

import com.model.applicant.{ AnswerMap, Application }
"))))

(ert-deftest sqi-scala-quick-import:search-import-body-4 ()
  (with-current-buffer (find-file-noselect "test4.scala")
    (insert "package foo.bar

import com.model.applicant.Application
")
    (scala-quick-import:search-import-body "AnswerMap" (lambda () "import com.model.{Allow, AnswerMap}"))
    (should (equal (buffer-string) "package foo.bar

import com.model.AnswerMap
import com.model.applicant.Application
"))))

;; shameless copy from https://github.com/abo-abo/swiper/blob/365008f8468b885cc03c0db347fa0eb996b6cd8d/ivy-test.el#L30-L49
(defvar ido-result nil
  "Holds the eval result of `ido-expr' by `ido-eval'.")

(defun ido-eval ()
  "Evaluate `ido-expr'."
  (interactive)
  (setq ido-result (eval ido-expr)))

(global-set-key (kbd "C-c e") 'ido-eval)

(defun ido-with (expr keys)
  "Evaluate EXPR followed by KEYS."
  (let ((ido-expr expr))
    (execute-kbd-macro
     (vconcat (kbd "C-c e")
              (kbd keys)))
    ido-result))

(ert-deftest sqi-scala-quick-import:search-import-body-5 ()
  (should
   (equal
    (ido-with '(with-current-buffer (find-file-noselect "test5.scala")
                 (progn
                   (insert "package foo.bar

import com.model.applicant.Application
")
                   (scala-quick-import:search-import-body "AnswerMap" (lambda () "import com.model.AnswerMap
import com.model.application.AnswerMap"))
                   (buffer-string))) "C-m")
    "package foo.bar

import com.model.AnswerMap
import com.model.applicant.Application
")))

(ert-deftest sqi-scala-quick-import:search-import-body-6 ()
  (should
   (equal
    (ido-with '(with-current-buffer (find-file-noselect "test6.scala")
                 (progn
                   (insert "package foo.bar

import com.model.applicant.Application
")
                   (scala-quick-import:search-import-body "AnswerMap" (lambda () "import com.model.AnswerMap
import com.model.applicant.AnswerMap"))
                   (buffer-string))) "applicant.AnswerMap C-m")
    "package foo.bar

import com.model.applicant.{ AnswerMap, Application }
")))


(ert-deftest sqi-scala-quick-import:search-import-scalaz-1 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-1.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
")
         (scala-quick-import:search-import-body "\\/" (lambda () "scalaz.\\/"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.\\/
")))

(ert-deftest sqi-scala-quick-import:search-import-scalaz-2 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-2.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
")
         (scala-quick-import:search-import-body "-\\/" (lambda () "scalaz.-\\/"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.-\\/
")))

(ert-deftest sqi-scala-quick-import:search-import-scalaz-3 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-3.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
")
         (scala-quick-import:search-import-body "\\/-" (lambda () "scalaz.\\/-"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.\\/-
")))


(ert-deftest sqi-scala-quick-import:search-import-scalaz-4 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-4.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
import scalaz.\\/
")
         (scala-quick-import:search-import-body "\\/-" (lambda () "scalaz.\\/-"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.{ \\/, \\/- }
")))

(ert-deftest sqi-scala-quick-import:search-import-scalaz-5 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-5.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
import scalaz.\\/
")
         (scala-quick-import:search-import-body "-\\/" (lambda () "scalaz.-\\/"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.{ -\\/, \\/ }
")))

(ert-deftest sqi-scala-quick-import:search-import-scalaz-6 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-6.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
import scalaz.EitherT
")
         (scala-quick-import:search-import-body "-\\/" (lambda () "scalaz.-\\/"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.{ -\\/, EitherT }
")))

(ert-deftest sqi-scala-quick-import:search-import-scalaz-7 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-7.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
import scalaz.EitherT
")
         (scala-quick-import:search-import-body "\\/" (lambda () "scalaz.\\/"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.{ EitherT, \\/ }
")))

(ert-deftest sqi-scala-quick-import:search-import-scalaz-8 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-8.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
import scalaz.EitherT
")
         (scala-quick-import:search-import-body "\\/-" (lambda () "scalaz.\\/-"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.{ EitherT, \\/- }
")))

(ert-deftest sqi-scala-quick-import:search-import-scalaz-9 ()
  (should
   (equal
    (with-current-buffer (find-file-noselect "test-scalaz-9.scala")
       (progn
         (insert "package foo.bar

import common.model.Application
import scalaz.{ -\\/, EitherT }
")
         (scala-quick-import:search-import-body "\\/-" (lambda () "scalaz.\\/-"))
         (buffer-string)))
    "package foo.bar

import common.model.Application
import scalaz.{ -\\/, EitherT, \\/- }
")))
