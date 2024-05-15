;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:cl-user)

(defpackage #:foo.lisp.flash/state
  (:use #:cl)
  (:export #:flash-state->session-data
           #:session-data->flash-state
           #:flash-state
           #:clear-flash
           #:get-flash
           #:flash
           #:flash-now
           #:flash-keep
           #:delete-flash
           #:sweep-flash)
  (:documentation "This package offers a set of utilities to manipulate flash states.
Exported functions include operations for converting between flash states and session data,
manipulation of flash states, and utilities for managing the lifecycle of flash messages across
requests."))

(defpackage #:foo.lisp.flash
  (:use #:cl)
  (:export #:*flash*
           #:clear-flash
           #:get-flash
           #:flash
           #:flash-now
           #:flash-keep
           #:delete-flash
           #:sweep-flash)
  (:documentation "This package provides a set of utilities to manipulating flash states,
wrapping functions defined from package `FOO.LISP.FLASH/STATE' in macros that expect
`*FLASH*' to be dynamically bound."))
