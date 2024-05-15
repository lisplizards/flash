;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.flash/tests/state)

(deftest flash-state
  (testing
      "#'MAKE-FLASH-STATE constructor returns a flash-state instance"
    (let ((flash-state (foo.lisp.flash/state::make-flash-state)))
      (ok flash-state)
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::sweep))))))

(deftest clear-flash
  (testing
      "#'CLEAR-FLASH sets the DATA and SWEEP slots to NIL")
  (let ((flash-state (foo.lisp.flash/state::make-flash-state
                      :data '(:a (foo.lisp.flash/state::make-flash-item-box
                                  :status :temporary
                                  :value 0)
                              :b (foo.lisp.flash/state::make-flash-item-box
                                  :status :temporary
                                  :value 1))
                      :sweep '(:a :b))))
    (assert (equal '(:a (foo.lisp.flash/state::make-flash-item-box
                         :status :temporary
                         :value 0)
                     :b (foo.lisp.flash/state::make-flash-item-box
                         :status :temporary
                         :value 1))
                   (slot-value flash-state 'foo.lisp.flash/state::data)))
    (assert (equal '(:a :b) (slot-value flash-state 'foo.lisp.flash/state::sweep)))
    (assert (not (null (slot-value flash-state 'foo.lisp.flash/state::data))))
    (assert (not (null (slot-value flash-state 'foo.lisp.flash/state::sweep))))
    (ok (null (foo.lisp.flash/state:clear-flash flash-state)))
    (ok (null (slot-value flash-state 'foo.lisp.flash/state::data)))
    (ok (null (slot-value flash-state 'foo.lisp.flash/state::sweep)))))

(deftest flash
  (testing
      "#'FLASH copies to the flash-state DATA slot"
    (let ((flash-state (foo.lisp.flash/state::make-flash-state)))
      (ok (null (foo.lisp.flash/state:flash flash-state '(:error "Woah"))))
      (ok (not (null (slot-value flash-state 'foo.lisp.flash/state::data))))
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (ok (equalp (list :error (foo.lisp.flash/state::make-flash-item-box
                                :status :new
                                :value "Woah"))
                  (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (null (foo.lisp.flash/state:flash
                           flash-state
                           '(:notice "OK!"
                             :hello "world"))))
      (ok (equalp (list :hello (foo.lisp.flash/state::make-flash-item-box
                                :status :new
                                :value "world")
                        :notice (foo.lisp.flash/state::make-flash-item-box
                                 :status :new
                                 :value "OK!")
                        :error (foo.lisp.flash/state::make-flash-item-box
                                :status :new
                                :value "Woah"))
                  (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (null (foo.lisp.flash/state:flash flash-state '(:notice "Changed"))))
      (ok (equalp (list :hello (foo.lisp.flash/state::make-flash-item-box
                                :status :new
                                :value "world")
                        :notice (foo.lisp.flash/state::make-flash-item-box
                                 :status :new
                                 :value "Changed")
                        :error (foo.lisp.flash/state::make-flash-item-box
                                :status :new
                                :value "Woah"))
                  (slot-value flash-state 'foo.lisp.flash/state::data))))))

(deftest get-flash
  (testing
     "#'GET-FLASH returns NIL when the flash-state DATA slot is NIL"
     (let ((flash-state (foo.lisp.flash/state::make-flash-state)))
       (ok (null (slot-value flash-state 'foo.lisp.flash/state::data)))
       (ok (null (foo.lisp.flash/state:get-flash flash-state :notice)))))

  (testing
     "#'GET-FLASH returns the value associated with the flash-type and does not mark to sweep if added"
     (let ((flash-state (foo.lisp.flash/state::make-flash-state
                         :data (list :notice (foo.lisp.flash/state::make-flash-item-box
                                              :status :new
                                              :value "Hey, listen")))))
       (assert (equalp (list :notice (foo.lisp.flash/state::make-flash-item-box
                                      :status :new
                                      :value "Hey, listen"))
                       (foo.lisp.flash/state::flash-state-data flash-state)))
       (assert (equal () (foo.lisp.flash/state::flash-state-sweep flash-state)))
       (ok (equal "Hey, listen" (foo.lisp.flash/state:get-flash flash-state :notice)))
       (ok (equal () (foo.lisp.flash/state::flash-state-sweep flash-state)))
       (ok (equal "Hey, listen" (foo.lisp.flash/state:get-flash flash-state :notice)))
       (ok (equal () (foo.lisp.flash/state::flash-state-sweep flash-state)))))

  (testing
     "#'GET-FLASH marks for sweep when the flash item has status LOADED"
     (let ((flash-state (foo.lisp.flash/state::make-flash-state
                         :data (list :notice (foo.lisp.flash/state::make-flash-item-box
                                              :status :loaded
                                              :value "Hey, listen"))
                         :sweep ())))
       (assert (equal () (foo.lisp.flash/state::flash-state-sweep flash-state)))
       (ok (foo.lisp.flash/state:get-flash flash-state :notice))
       (ok (equalp (list :notice (foo.lisp.flash/state::make-flash-item-box
                                  :status :loaded
                                  :value "Hey, listen"))
                   (foo.lisp.flash/state::flash-state-data flash-state)))
       (ok (equal '(:notice) (foo.lisp.flash/state::flash-state-sweep flash-state))))))

(deftest flash-now
  (testing
      "#'FLASH-NOW copies to the flash-state DATA slot and SWEEP slot"
    (let ((flash-state (foo.lisp.flash/state::make-flash-state)))
      (ok (null (foo.lisp.flash/state:flash-now flash-state '(:error "Woah"
                                                              :foo "bar"))))
      (ok (equalp (list :foo (foo.lisp.flash/state::make-flash-item-box
                              :status :temporary
                              :value "bar")
                        :error (foo.lisp.flash/state::make-flash-item-box
                                :status :temporary
                                :value "Woah"))
                 (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (equal '(:foo :error) (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (ok (null (foo.lisp.flash/state:flash-now flash-state '(:notice "OK!"))))
      (ok (equalp (list :notice (foo.lisp.flash/state::make-flash-item-box
                                 :status :temporary
                                 :value "OK!")
                        :foo (foo.lisp.flash/state::make-flash-item-box
                              :status :temporary
                              :value "bar")
                        :error (foo.lisp.flash/state::make-flash-item-box
                                :status :temporary
                                :value "Woah"))
                 (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (null (foo.lisp.flash/state:flash-now flash-state '(:notice "Changed"))))
      (ok (equalp (list :notice (foo.lisp.flash/state::make-flash-item-box
                                 :status :temporary
                                 :value "Changed")
                        :foo (foo.lisp.flash/state::make-flash-item-box
                              :status :temporary
                              :value "bar")
                        :error (foo.lisp.flash/state::make-flash-item-box
                                :status :temporary
                                :value "Woah"))
                  (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (equal '(:notice :foo :error) (slot-value flash-state 'foo.lisp.flash/state::sweep))))))

(deftest flash-keep
  (testing
      "#'FLASH-KEEP without a flash-type argument sets slot SWEEP to NIL"
    (let ((flash-state (foo.lisp.flash/state::make-flash-state
                        :data (list
                               :a (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 0)
                               :b (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 1))
                        :sweep '(:a :b))))
      (ok (null (foo.lisp.flash/state:flash-keep flash-state)))
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (ok (equalp (list :a (foo.lisp.flash/state::make-flash-item-box
                            :status :loaded
                            :value 0)
                        :b (foo.lisp.flash/state::make-flash-item-box
                            :status :loaded
                            :value 1))
                  (slot-value flash-state 'foo.lisp.flash/state::data)))))

  (testing "#'FLASH-KEEP with a flash-type argument removes an entry from the SWEEP slot"
    (let ((flash-state (foo.lisp.flash/state::make-flash-state
                        :data (list
                               :a (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 0)
                               :b (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 1))
                        :sweep '(:a :b))))
      (ok (null (foo.lisp.flash/state:flash-keep flash-state :a)))
      (ok (equal '(:b) (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (ok (equalp (list :a (foo.lisp.flash/state::make-flash-item-box
                            :status :loaded
                            :value 0)
                        :b (foo.lisp.flash/state::make-flash-item-box
                            :status :loaded
                            :value 1))
                  (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (null (foo.lisp.flash/state:flash-keep flash-state :d)))
      (ok (equal '(:b) (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (ok (null (foo.lisp.flash/state:flash-keep flash-state :b)))
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (ok (null (foo.lisp.flash/state:flash-keep flash-state :b)))
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::sweep))))))

(deftest delete-flash
  (testing
      "#'DELETE-FLASH removes entries from the DATA slot and the SWEEP slot"
    (let ((flash-state (foo.lisp.flash/state::make-flash-state
                        :data (list
                               :a (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 0)
                               :b (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 1))
                        :sweep '(:a :b))))
      (ok (null (foo.lisp.flash/state:delete-flash flash-state :a)))
      (ok (equalp (list
                   :b (foo.lisp.flash/state::make-flash-item-box
                      :status :loaded
                      :value 1))
                  (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (equal '(:b) (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (ok (null (foo.lisp.flash/state:delete-flash flash-state :a)))
      (ok (equalp (list
                   :b (foo.lisp.flash/state::make-flash-item-box
                       :status :loaded
                       :value 1))
                 (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (equal '(:b) (slot-value flash-state 'foo.lisp.flash/state::sweep)))
      (foo.lisp.flash/state:delete-flash flash-state :b)
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (null (slot-value flash-state 'foo.lisp.flash/state::sweep))))))

(deftest sweep-flash
  (testing
      "#'SWEEP-FLASH prunes all flash-types represented in the SWEEP slot from the DATA slot"
    (let ((flash-state (foo.lisp.flash/state::make-flash-state
                        :data (list
                               :a (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 0)
                               :b (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 1)
                               :c (foo.lisp.flash/state::make-flash-item-box
                                   :status :loaded
                                   :value 2))
                        :sweep '(:a :x))))
      (ok (null (foo.lisp.flash/state:sweep-flash flash-state)))
      (ok (equalp (list
                   :b (foo.lisp.flash/state::make-flash-item-box
                       :status :loaded
                       :value 1)
                   :c (foo.lisp.flash/state::make-flash-item-box
                       :status :loaded
                       :value 2))
                 (slot-value flash-state 'foo.lisp.flash/state::data)))
      (ok (equal '(:a :x) (slot-value flash-state 'foo.lisp.flash/state::sweep))))))
