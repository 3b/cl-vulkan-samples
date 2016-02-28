(defsystem cl-vulkan-samples
  :description "Common Lisp/CL-Vulkan ports of (some of) LunarG VulkanSamples"
  :depends-on (cl-vulkan alexandria)
  :license "MIT"
  :author "Bart Botta <00003b at gmail.com>"
  :serial t
  :components ((:file "package")
               (:file "instance")
               (:file "enumerate")
               (:file "device")))
