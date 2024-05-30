;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/flash)

(defvar *flash-session-key*)

(defparameter *lack-middleware-flash*
  (lambda (app &key (session-key "_flash")
                 (session-env :lack.session))
    (declare (optimize (speed 0) (safety 3) (debug 3))
             (type function app))
    (assert (not (null session-key))
            nil
            "session-key must be set")
    (check-type session-env keyword)
    (lambda (env)
      (declare (optimize (speed 3) (safety 0) (debug 0))
               (type list env)
               (type keyword session-env))
      (let ((*flash-session-key* session-key)
            (session (or (getf env session-env)
                         (error "session middleware is missing from environment: ~A" session-env))))
        (declare (type hash-table session))
        (let* ((flash-data (gethash *flash-session-key* session))
               (foo.lisp.flash:*flash* (foo.lisp.flash/state:session-data->flash-state
                                             flash-data)))
          (declare (type list flash-data)
                   (type foo.lisp.flash/state:flash-state foo.lisp.flash:*flash*))
          (let ((response (funcall app env)))
            (declare (type list response)
                     (type foo.lisp.flash/state:flash-state foo.lisp.flash:*flash*))
            (foo.lisp.flash/state:sweep-flash foo.lisp.flash:*flash*)
            (setf (gethash *flash-session-key* session)
                  (foo.lisp.flash/state:flash-state->session-data
                   foo.lisp.flash:*flash*))
            response))))))
