;; Copyright (c) 2024 John Newton
;; SPDX-License-Identifier: Apache-2.0

(in-package #:foo.lisp.flash/state)

(defstruct flash-state
  (data () :type list)
  (sweep () :type keyword-list))

(defstruct flash-item-box
  (status :new :type flash-item-status)
  value)

(declaim (ftype (function (flash-state) list) flash-state->session-data))
(defun flash-state->session-data (flash-state)
  "Returns a property-list containing the original user-provided values; used for
storing flash state in the session"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state))
  (loop for (flash-type flash-item-box) of-type (keyword flash-item-box)
        on (flash-state-data flash-state) by #'cddr
        append (list flash-type (flash-item-box-value flash-item-box))))

(declaim (ftype (function (list) flash-state) session-data->flash-state))
(defun session-data->flash-state (flash-data)
  "Returns a property-list containing FLASH-ITEM-BOX values wrapped around FLASH-DATA;
adds STATUS: LOADED in order to distinguish which items were pulled from the session"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type list flash-data))
  (make-flash-state
   :data (loop for (flash-type value) of-type (keyword t) on flash-data by #'cddr
               append (list flash-type (make-flash-item-box
                                        :status :loaded
                                        :value value)))))

(declaim (ftype (function (flash-state) null) clear-flash))
(defun clear-flash (flash-state)
  "Resets FLASH-STATE, setting the DATA and SWEEP slots to NIL"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state))
  (setf (flash-state-data flash-state) ()
        (flash-state-sweep flash-state) ())
  (values))

(declaim (ftype (function (flash-state keyword) t) get-flash))
(defun get-flash (flash-state flash-type)
  "Retrieves the data associated with FLASH-TYPE"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state)
           (type keyword flash-type))
  (let ((flash-item-box (getf (flash-state-data flash-state) flash-type)))
    (declare (type (or null flash-item-box) flash-item-box))
    (unless flash-item-box
      (return-from get-flash nil))
    (when (eql :loaded (flash-item-box-status flash-item-box))
      (pushnew flash-type (flash-state-sweep flash-state)))
    (flash-item-box-value flash-item-box)))

(declaim (ftype (function (flash-state list) null) flash))
(defun flash (flash-state lst)
  "Given a property list with keyword keys, adds flashes to the FLASH-STATE DATA slot"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state)
           (type list lst))
  (loop for (flash-type value) on lst by #'cddr
        do (setf (getf (flash-state-data flash-state) flash-type)
                 (make-flash-item-box
                  :status :new
                  :value value)))
  (values))

(declaim (ftype (function (flash-state list) null) flash-now))
(defun flash-now (flash-state lst)
  "Given a property list with keyword keys, adds flashes to the FLASH-STATE DATA slot"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state)
           (type list lst))
  (loop for (flash-type value) of-type (keyword t) on lst by #'cddr
        do (progn
             (setf (getf (flash-state-data flash-state) flash-type)
                   (make-flash-item-box
                    :status :temporary
                    :value value))
             (pushnew flash-type (flash-state-sweep flash-state))))
  (values))

(declaim (ftype (function (flash-state &optional keyword) null) flash-keep))
(defun flash-keep (flash-state &optional flash-type)
  "Remove items of the specified FLASH-TYPE from the SWEEP slot, ensuring they will
not be deleted at the end of the request"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state)
           (type (or null keyword) flash-type))
  (if flash-type
      (setf (flash-state-sweep flash-state)
            (delete flash-type (flash-state-sweep flash-state)))
      (setf (flash-state-sweep flash-state)
            ()))
  (values))

(declaim (ftype (function (flash-state keyword) null) delete-flash))
(defun delete-flash (flash-state flash-type)
  "Deletes the entry of FLASH-TYPE from FLASH-STATE"
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state)
           (type keyword flash-type))
  (remf (flash-state-data flash-state) flash-type)
  (setf (flash-state-sweep flash-state)
        (delete flash-type (flash-state-sweep flash-state)))
  (values))

(declaim (ftype (function (flash-state) null) sweep-flash))
(defun sweep-flash (flash-state)
  "Deletes all loaded entries that were marked as accessed. Called before
storing the flash data in the session."
  (declare (optimize (speed 3) (safety 0) (debug 0))
           (type flash-state flash-state))
  (dolist (flash-type (flash-state-sweep flash-state))
    (declare (type keyword flash-type))
    (remf (flash-state-data flash-state) flash-type))
  (values))
