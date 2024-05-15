;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(defsystem "foo.lisp.lack-middleware-flash"
  :version "1.0.0"
  :author "John Newton"
  :license "Apache-2.0"
  :homepage "https://github.com/lisplizards/flash"
  :bug-tracker "https://github.com/lisplizards/flash/issues"
  :source-control (:git "https://github.com/lisplizards/flash.git")
  :depends-on ("foo.lisp.flash"
               "lack-request")
  :components ((:module "src"
                :components
                ((:module "middleware"
                  :components
                  ((:file "main" :depends-on ("package"))
                   (:file "package"))))))
  :in-order-to ((test-op (test-op "foo.lisp.lack-middleware-flash/tests"))))

(defsystem "foo.lisp.lack-middleware-flash/tests"
  :author "John Newton"
  :license "Apache-2.0"
  :depends-on ("foo.lisp.lack-middleware-flash"
               "rove")
  :components ((:module "tests"
                :components
                ((:module "middleware"
                  :components
                  ((:file "main" :depends-on ("package"))
                   (:file "package"))))))
  :description "Test system for foo.lisp.flash"
  :perform (test-op (op c) (symbol-call :rove :run c)))

