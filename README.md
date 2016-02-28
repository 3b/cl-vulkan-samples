CL ports of some of the sample applications from the LunarG [Vulkan Samples Kit](https://github.com/LunarG/VulkanSamples), using [CL-Vulkan](https://github.com/3b/cl-vulkan/).

Initial focus is on the progression of samples leading to drawing a
cube (instance, device, initcommandbuffer, initswapchain,
initdepthbuffer, inituniformbuffer, descriptor_pipeline_layouts,
initrenderpass, initshaders, initframebuffers, vertexbuffer,
allocdescriptorsets, initpipeline, drawcube, drawtexturedcube).

Currently depends on the [Vulkan SDK](http://lunarg.com/vulkan-sdk/)
for GLSL and SPIRV tools, though they may be replaces once CL
equivalents are available.
