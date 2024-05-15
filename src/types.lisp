;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.flash/state)

(deftype flash-item-status ()
  `(and (satisfies keywordp)
        (satisfies flash-item-status-p)))

(deftype keyword-list ()
  `(and (satisfies listp)
        (satisfies keyword-list-p)))

(declaim (ftype (function (keyword) boolean) flash-item-status-p))
(defun flash-item-status-p (status)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type keyword status))
  (when (member status '(:loaded :new :temporary) :test #'eql)
    t))

(declaim (ftype (function (list) boolean) keyword-list-p))
(defun keyword-list-p (lst)
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list lst))
  (every #'keywordp lst))
