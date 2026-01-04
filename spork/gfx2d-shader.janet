###
### Wrapper around gfx2d-codegen that will also compile-in "shaders". These shaders
### can be used to fill and stroke paths with custom per-pixel coloring logic.
### 

(use ./cjanet)

(import ./path)

(def- kernel-env (curenv))
(def- cf (dyn :current-file))

(defn shader-compile
  "Generate but do not load a custom shader module. Returns the path to the generated DLL/shared object."
  [&named module-name pixel-shader]
  (assert module-name)
  (assert pixel-shader)
  (def env (make-env kernel-env))
  (put env :pixel-shader pixel-shader)
  (put env :shader-compile true)
  (var s nil)
  (with-env env
    (begin-jit :module-name module-name)
    (dofile (path/join cf "../gfx2d-codegen.janet") :env env)
    (set s (end-jit :no-load true :cache false)))
  s)

(defn shader-jit
  "Compile and load a shader module."
  [&named prefix pixel-shader]
  (assert prefix)
  (def module-name (string "shader" (gensym) "_" (os/getpid)))
  (def outer-env (curenv))
  (def native-path (shader-compile :pixel-shader pixel-shader :module-name module-name))
  (def e (native native-path))
  (merge-module outer-env e prefix)
  outer-env)
