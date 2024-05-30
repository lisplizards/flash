;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:lack/middleware/flash/tests)

(deftest flash-middleware-test ()
  (testing
   "adds FLASH to the env"
   (let* ((app (lambda (env)
                 `(200
                   (:content-type "text/plain")
                   (,(or (let ((flash-item-box (getf (foo.lisp.flash/state::flash-state-data
                                                      (getf env :flash))
                                                     :message)))
                           (when flash-item-box
                             (foo.lisp.flash/state::flash-item-box-value
                              flash-item-box)) )
                         "")))))
          (wrapped-app (funcall lack/middleware/flash:*lack-middleware-flash*
                                app))
          (env (list :lack.session (make-hash-table :test #'equal))))
     (setf (gethash "_flash" (getf env :lack.session))
           ())
     (let ((response (funcall wrapped-app env)))
       (ok (equal (third response) '(""))))
     (setf (gethash "_flash" (getf env :lack.session))
           (list :message "Hello, World."))
     (let ((response (funcall wrapped-app env)))
       (ok (equal (third response) '("Hello, World."))))))

  (testing
   "signals an error condition when LACK.SESSION is missing from the env"
   (let* ((app (lambda (env)
                 (declare (ignore env))
                 `(200
                   (:content-type "text/plain")
                   ("Hello, World."))))
          (wrapped-app (funcall lack/middleware/flash:*lack-middleware-flash*
                                app))
          (env ()))
     (ok (signals (funcall wrapped-app env) 'simple-error)))))
