;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:canebreak-asd
  (:use :cl :asdf))

(in-package :canebreak-asd)

(defsystem canebreak
    :name "canebreak"
    :version "0.0.0"
    :maintainer "fouric"
    :author "fouric"
    :license "All rights reserved"
    :description "an sexp assembler language (translation layer)"

    :serial t
    :pathname "src"
    :components ((:file "canebreak"))
    :depends-on (:fouriclib))
