;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.flash)

(defvar *flash*)

(defmacro clear-flash ()
  "Clears the flash state."
  `(foo.lisp.flash/state:clear-flash *flash*))

(defmacro get-flash (flash-type)
  "Retrieves the flash data from the session for the given key"
  `(foo.lisp.flash/state:get-flash *flash* ,flash-type))

(defmacro flash (&rest plist)
  "Stores flash data in the session. Returns NIL."
  `(foo.lisp.flash/state:flash *flash* (list ,@plist)))

(defmacro flash-now (&rest plist)
  "Adds flash data for the current request only. Returns NIL."
  `(foo.lisp.flash/state:flash-now *flash* (list ,@plist)))

(defmacro flash-keep (&optional flash-type)
  "Preserves flash key(s) marked for deletion; when optional parameter FLASH-TYPE is NIL,
  prevents all keys from being deleted. Should be called when the flash data is
  accessed during the current action but also needed for a future request. Returns NIL."
  `(foo.lisp.flash/state:flash-keep *flash* ,flash-type))

(defmacro delete-flash (flash-type)
  "Deletes the given flash-data associated with the given flash-type. Returns NIL."
  `(foo.lisp.flash/state:delete-flash *flash* ,flash-type))

(defmacro sweep-flash ()
  "Meant to be called after the response has been generated; removes keys that were loaded
from the session and accessed during the current request. Returns NIL."
  `(foo.lisp.flash/state:sweep-flash *flash*))
