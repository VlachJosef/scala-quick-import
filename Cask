;;-*- Mode: Emacs-Lisp -*-
;;; Cask --- project definition

;; Copyright (C) 2017 Vlach Josef

;; Author: Josef Vlach <Vlach.Josef@gmail.com>

;;; Commentary:
;;
;;  Cask is a package manager for emacs lisp projects, this generates
;;  the *-pkg.el.
;;
;;  See http://cask.readthedocs.org/en/latest/guide/dsl.html for more
;;  information about Cask.
;;
;;    cask pkg-file
;;
;;    cask update
;;    cask install
;;
;;  are particularly useful commands.
;;
;; To run the tests:
;;    cask exec ert-runner
;;
;;; Code:

(source melpa)

(package-file "scala-quick-import.el")

(depends-on "s")
(depends-on "ag")
(depends-on "ido")
(depends-on "ensime")
(depends-on "projectile")

(development
 (depends-on "ert-runner")
 ;;(depends-on "ecukes")
 ;;(depends-on "espuds")
 (depends-on "undercover")
 )

;;; Cask ends here
