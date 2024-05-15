;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.flash"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/flash"
  :bug-tracker "https://github.com/lisplizards/flash/issues"
  :source-control (:git "https://github.com/lisplizards/flash.git")
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("package" "state"))
                 (:file "state" :depends-on ("package" "types"))
                 (:file "types" :depends-on ("package"))
                 (:file "package"))))
  :description "Flash messages for Lisp apps"
  :in-order-to ((test-op (test-op "foo.lisp.flash/tests"))))

(defsystem "foo.lisp.flash/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.flash"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main" :depends-on ("package" "state"))
                 (:file "state" :depends-on ("package"))
                 (:file "package"))))
  :description "Test system for foo.lisp.flash"
  :perform (test-op (op c) (symbol-call :rove :run c)))
