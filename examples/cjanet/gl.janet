###
### Use cjanet JIT and libpng to generate a large fractal.
###
### Requires a c compiler and glfw
### Direct port of tutorial from https://learnopengl.com/Getting-started/Hello-Window
###

(use spork/cjanet)

(begin-jit
  :module-name "glexample"
  :pkg-config ["glfw3" "gl"])

(include <GLFW/glfw3.h>)

(function process-input
  "handle key presses"
  [window:*GLFWwindow] -> void
  (when (== GLFW_PRESS (glfwGetKey window GLFW_KEY_ESCAPE))
    (glfwSetWindowShouldClose window 1)))

(function render
  "drawing"
  [window:*GLFWwindow] -> void
  (glClearColor 0.2 0.3 0.3 1.0)
  (glClear GL_COLOR_BUFFER_BIT))

(function framebuffer-size-callback
  "resize fb when user resizes window"
  [window:*GLFWwindow width:int height:int] -> void
  (glViewport 0 0 width height))

(cfunction main-window
  "Create main window"
  [w:int h:int] -> int
  (glfwInit)
  (glfwWindowHint GLFW_CONTEXT_VERSION_MAJOR 3)
  (glfwWindowHint GLFW_CONTEXT_VERSION_MINOR 3)
  (glfwWindowHint GLFW_OPENGL_PROFILE GLFW_OPENGL_CORE_PROFILE)
  (def (window (* GLFWwindow)) (glfwCreateWindow w h "LearnOpenGL" NULL NULL))
  (unless window
    (printf "failed to create glfw window\n")
    (glfwTerminate)
    (return -1))
  (glfwSetFramebufferSizeCallback window framebuffer-size-callback)
  (glfwMakeContextCurrent window)
  (glViewport 0 0 w h)
  (while (not (glfwWindowShouldClose window))
    (process-input window)
    (render window)
    (glfwSwapBuffers window)
    (glfwPollEvents))
  (glfwDestroyWindow window)
  (glfwTerminate)
  (return 0))

(end-jit)

(main-window 800 600)
