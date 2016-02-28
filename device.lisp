(in-package #:cl-vulkan-samples)

;; c++ examples have a predefined struct for this, but just using a
;; plist for now.
(defparameter *info* '())
(defmacro info (property &rest properties-or-indices)
  (let ((f `(getf *info* ,property)))
    (loop for p in properties-or-indices
          do (if (numberp p)
                 (setf f `(elt ,f ,p))
                 (setf f `(getf ,f ,p))))
    f))

(defun init-enumerate-device ()
  (setf (info :gpus) (vk:enumerate-physical-devices (info :instance)))
  (assert (plusp (length (info :gpus))))
  (setf (info :gpu) (info :gpus 0))
  (setf (info :queue-props)
        (vk:get-physical-device-queue-family-properties (info :gpu)))
  (assert (plusp (length (info :queue-props))))
  (setf (info :memory-properties)
        (vk:get-physical-device-memory-properties (info :gpu)))
  (setf (info :gpu-props)
        (vk:get-physical-device-properties (info :gpu))))

(defun device ()
  (vk:with-instance (instancae :app "cl-vulkan-samples device")
    (let ((*info* (list :instance instancae))
          (queue-family-index nil)
          (queue-priorities '(0.0)))
      (init-enumerate-device)

      (loop for queue in (info :queue-props)
            for i from 0
            when (member :graphics (getf queue :queue-flags))
              do (setf queue-family-index i)
              and return t
            finally (error "couldn't find a graphics queue family"))

      (vk:with-device (device (info :gpu)
                       ;; WITH-DEVICE handles all the vk*Info structs
                       ;; we just pass in the details
                       :queue-family-index queue-family-index
                       :priorities queue-priorities
                       :features nil)
        ;; if we get here without an ERROR, DEVICE should be valid
        ;; and WITH-DEVICE wil destroy it on exit
        (format t "got device: ~s~%" device)))))
