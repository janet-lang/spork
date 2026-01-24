###
### Wrapper around gfx2d-codegen that will also compile-in "shaders". These shaders
### can be used to fill and stroke paths with custom per-pixel coloring logic.
### 

(use ./cjanet)

(import ./path)

(def- kernel-env (curenv))
(def- cf (dyn :current-file))

(defn shader-begin
  "Generate but do not load a custom shader module. Returns the path to the generated DLL/shared object."
  [&named module-name shader-args prefix]
  (begin-jit :module-name module-name :prefix prefix)
  (def env (make-env kernel-env))
  (put env :shader-compile true)
  (put env :shader-args shader-args)
  (put env *jit-context* (dyn *jit-context*))
  (put env *cfun-list* (dyn *cfun-list*))
  (put env *cdef-list* (dyn *cdef-list*))
  (put env *abstract-type-list* (dyn *abstract-type-list*))
  (put env *out* (dyn *out*))
  # Add gfx2d-codegen as prelude to our JIT context
  (dofile (path/join cf "../gfx2d-codegen.janet") :env env)
  nil)

(defn shader-end
  "Finish generating shader"
  [&named cache no-load]
  (end-jit :cache cache :no-load no-load))

(defmacro shader
  "Macro shorthand for creating a shader. The body of the shader should contain a CJanet function with the prototype
  (function shader [x:int y:int <args>] -> uint32_t ...) which will be invoked per pixel. The use can define as many helper
  functions as they like, include headers, or even use `print` to emit raw C source code which will be compiled into the shader."
  [shader-name args & body]
  (def prefix (string shader-name "/"))
  (def mname (string "shader-" shader-name))
  ~(upscope
      (,shader-begin :module-name ,mname :shader-args ',args :prefix ,prefix)
      ,;body
      (,shader-end :cache true)))

