(in-package #:cl-vulkan-samples)

(defun init-global-layer-properties ()
  (setf (info :instance-layer-properties)
        (vk:enumerate-instance-layer-properties)))

(defun instance-layer-properties ()
  (let ((*info* ()))
    (init-global-layer-properties)
    (format t "Instance Layers:~%")
    (loop for prop in (info :instance-layer-properties)
          do (flet ((prop (x)
                      (getf prop x)))
               (format t "~a:~%" (prop :layer-name))
               (format t "  Version: ~s~%" (prop :implementation-version))
               (format t "  API Version: ~{~d.~d.~d~}~%"
                       (vk:decode-version (prop :spec-version)))
               (format t "  Description: ~a~%" (prop :description))))))
