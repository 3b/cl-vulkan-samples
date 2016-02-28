(in-package #:cl-vulkan-samples)

(defun instance ()
  ;; WITH-INSTANCE also binds the variables used for managing
  ;; extension function pointers, so it should generally be used in
  ;; place of manually creating the instance object
  (vk:with-instance (instance
                     ;; WITH-INSTANCE handles allocating the vkApplicationInfo
                     ;; we can pass pApplicationName and pEngineName as
                     ;; as keyword arguments:
                     :app "cl-vulkan-samples instance"
                     :engine "cl-vulkan-samples instance"
                     ;; similarly for versions
                     :app-version 1
                     :engine-version 1
                     ;; apiVersion is currently set automatically

                     ;; It also handles creation of the
                     ;; vkInstanceCreateInfo. We can specify layers
                     ;; and extensions as lists of strings, or
                     ;; keywords for known extensions.
                     ;; :exts '(:khr-surface)
                     ;; :layers '("VK_LAYER_LUNARG_api_dump")
                     )
    ;; if instance creation failed, we would have gotten an ERROR
    (format t "got instance ~s~%" instance)
    ;; WITH-INSTANCE destroys the instance for us on exit
    ))
