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
  ;; save some info about the device for later use
  (setf (info :memory-properties)
        (vk:get-physical-device-memory-properties (info :gpu)))
  (setf (info :gpu-props)
        (vk:get-physical-device-properties (info :gpu)))
  (setf (info :gpu-props)
        (vk:get-physical-device-features (info :gpu))))

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

      ;; WITH-DEVICE handles all the vk*Info structs
      ;; we just pass in the details
      (vk:with-device (device (info :gpu)
                       ;; Specify which queue families we want to use.
                       ;; If using multiple families, specify a list of
                       ;; indices.
                       :queue-family-index queue-family-index
                       ;; if we have multiple queues for a given
                       ;; family, we can specify a list of relative
                       ;; priorities (0.0-1.0) for each. If using
                       ;; multiple queue families, :priorities should
                       ;; be a list containing a list of priorities
                       ;; for each corresponding queue family

                       ;; (API for :queue-family-index and :priorities
                       ;;  might change in the future, since handling
                       ;;  a single family specially is a bit ugly)
                       :priorities queue-priorities
                       ;; Vulkan has various features which are not
                       ;; required to be supported (like different
                       ;; compression formats), or which might slow
                       ;; things down (like checking array bounds
                       ;; before access), so you need to specify which
                       ;; ones you want to use here.  for example
                       ;; :features '(:robust-buffer-access t
                       ;;             :texture-compression-bc t)

                       ;; You should also check the results of
                       ;; vk:get-physical-device-features first to
                       ;; make sure the particular features are
                       ;; available on the selected device.

                       ;; Pass NIL if you don't need any optional features.
                       :features nil)
        ;; if we get here without an ERROR, DEVICE should be valid
        ;; and WITH-DEVICE wil destroy it on exit
        (format t "got device: ~s~%" device)))))
