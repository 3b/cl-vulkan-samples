(in-package #:cl-vulkan-samples)

(defun enumerate()
  (vk:with-instance (instance :app "cl-vulkan-samples enumerate")
    (let ((physical-devices (vk::enumerate-physical-devices instance)))
      (format t "found physical devices: ~s~%" physical-devices))))

(defun enumerate-adv ()
  (vk:with-instance (instance :app "cl-vulkan-samples enumerate-adv")
    (let ((physical-devices (vk::enumerate-physical-devices instance)))
      (loop for dev in physical-devices
            for properties = (vk::get-physical-device-properties dev)
            do (flet ((prop (x)
                        (getf properties x)))
                 (format t "apiVersion: ~d.~d.~d~%"
                         (ldb (byte 12 22) (prop :api-version))
                         (ldb (byte 11 11) (prop :api-version))
                         (ldb (byte 12 0) (prop :api-version)))
                 (format t "driverVersion: ~s~%" (prop :driver-version))
                 (format t "vendorId: #x~6,'0x~%" (prop :vendor-id))
                 (format t "deviceId: #x~6,'0x~%" (prop :device-id))
                 (format t "deviceType: ~s~%" (prop :device-type))
                 (format t "deviceName: ~s~%" (prop :device-name))
                 (format t "pipelineCcheUUID: ~{~4@{~2,'0x~^~}-~2@{~2,'0x~^~}-~2@{~2,'0x~^~}-~2@{~2,'0x~^~}-~@{~2,'0x~^~}~}~%"
                         (coerce (prop :pipeline-cache-uuid) 'list)))))))
