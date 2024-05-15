;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.flash/tests/state
  (:use #:cl #:rove)
  (:documentation "Test package for `FOO.LISP.FLASH/STATE'."))

(defpackage #:foo.lisp.flash/tests
  (:use #:cl #:rove)
  (:documentation "Test package for `FOO.LISP.FLASH'."))
