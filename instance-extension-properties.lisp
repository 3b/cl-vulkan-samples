(in-package #:cl-vulkan-samples)

(defun init-global-extension-properties ()
  (setf (info :instance-extension-properties)
        (vk:enumerate-instance-extension-properties nil)))

(defun instance-extension-properties ()
  (with-sample-instance (instance )
    (init-global-extension-properties)
    (format t "Instance Extensions:~%")
    (loop for prop in (info :instance-extension-properties)
          do (flet ((prop (x)
                      (getf prop x)))
               (format t "~a:~%" (prop :extension-name))
               (format t "  Version: ~s~%" (prop :implementation-version))
               (format t "  API Version: ~{~d.~d.~d~}~%"
                       (vk:decode-version (prop :spec-version)))
               (format t "  Description: ~a~%" (prop :description))))))
