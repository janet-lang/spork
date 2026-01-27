###
### gfx2d-codegen.janet
###
### Various simple CPU-based 2d graphics tools suitable for demos, visulizations, and charting.
### The module generates C code which then is further compiled. It can also be used to generate "shaders"
### by evaluating this file with `(dyn :shader-compile)` set, which disables a number of functions and allows
### passing in a pixel shader stub.
###
### Leans on the underlying C compiler for optimization - recommended to be used with JANET_BUILD_TYPE=native
### to take advantage of the best available vectorization.
###
### Includes:
### * Saving and loading to and from several common image file formats.
### * Image blitting
### * Blend modes
### * Cropping imges
### * Read/Set individual pixels
### * Built in simple text rendering
### * Image resizing
### * Stroke and Fill paths
### * Bezier curves
### * Pixel shader abstraction with spork/cjanet
###

### TODO
### [x] - save and load to file
### [x] - rect and circle primitive
### [x] - image blitting
### [x] - image cropping
### [x] - testing harness
### [x] - get/set individual pixels (mostly for testing)
### [x] - text w/ simple font
### [x] - stroke paths (w/ thickness)
### [x] - plotting (1 pixel wide lines, no aa)
### [x] - blending
### [ ] - sRGB correct blending
### [x] - image resizing
### [x] - bezier
### [ ] - splines
###       - b-spline
###       - catmull-rom (useful for charts)
### [x] - fill path (raycast rasterizer)
### [ ] - LUTs (possibly w/ shaders)
### [ ] - Gradients (possibly w/ shaders)
### [x] - Better 2D point abstraction
### [ ] - Affine transforms
### [x] - float coordinate primitives (paths, rect, line, etc)
### [ ] - stippled lines
### [ ] - right-angle image rotation / flips
### [ ] - sRGB gamma correction/conversion
### [ ] - rotated text

### Stretch TODO
### [ ] - vector font rendering
### [ ] - anti-aliasing w/ mutli-sampling and/or analysis
### [x] - shaders using cjanet-jit - "fill" and "stroke" shaders
### [ ] - multithreading
### [ ] - Image analysis and statistics (RMSE, histogram, k-means, etc.)

(use ../spork/cjanet)

(defmacro comp-unless
  [condition & body]
  (if-not (eval condition)
    ~(upscope ,;body)))

(include <janet.h>)
(include <string.h>)
(include <math.h>)
(include <assert.h>)

(@ define STBIW_WINDOWS_UTF8)

(comp-unless (dyn :shader-compile)
  (include "stb_image.h")
  (include "stb_image_write.h")
  (include "stb_image_resize2.h"))

###
### Macros
###

(defn- unused
  :cjanet-expression-macro
  "Mark a variable as unused"
  [x]
  ~(cast void ,x))

(defn- swap
  :cjanet-block-macro
  "Swap two variables"
  [x y typ]
  ~(do
     (def (tmp ,typ) ,x)
     (set ,x ,y)
     (set ,y tmp)))

(defn- sort2
  :cjanet-block-macro
  "sort 2 variables such that x is less the y. If not, swap them."
  [x y typ]
  ~(when (< ,y ,x)
     (def (tmp ,typ) ,x)
     (set ,x ,y)
     (set ,y tmp)))

(defn- each-i
  :cjanet-block-macro
  "For loop shortand for `(for [(var i:int start) (< i end) (++ i)] ...)`"
  [start end & body]
  ~(for [(var i:int ,start) (< i ,end) (++ i)]
     ,;body))

(defn- each-y
  :cjanet-block-macro
  "For loop shortand for `(for [(var i:int start) (< i end) (++ i)] ...)`"
  [start end & body]
  ~(for [(var y:int ,start) (< y ,end) (++ y)]
     ,;body))

(defn- each-x
  :cjanet-block-macro
  "For loop shortand for `(for [(var i:int start) (< i end) (++ i)] ...)`"
  [start end & body]
  ~(for [(var x:int ,start) (< x ,end) (++ x)]
     ,;body))

(defn- cond-expression
  :cjanet-expression-macro
  "Cond but as an expression"
  [& cases]
  (assert (odd? (length cases)))
  (def case-value (take 2 cases))
  (def [case value] case-value)
  (if (> 2 (length case-value))
    case
    ~(? ,case ,value ,(cond-expression ;(drop 2 cases)))))

(defn- polymorph
  :cjanet-block-macro
  "Make specializations of a function implementation explicitly to help an optimizing compiler"
  [sym bindings & body]
  ~(switch
     ,sym
     ,;(array/join
         @[]
         ;(seq [b :in bindings]
            [b ~(do ,;body (break))]))
     (assert (and 0 "polymorph case fallthrough"))))

(defn- polymorph-cond
  :cjanet-block-macro
  "Make specializations of a function implementation explicitly to help an optimizing compiler"
  [sym bindings & body]
  ~(if
     ,;(array/join
         @[]
         ;(seq [b :in bindings]
            [~(= ,b ,sym) ~(do ,;body)]))
     (assert (and 0 "polymorph-cond case fallthrough"))))

###
### Math helpers
###

(function lerp :static :inline
  [a:double b:double t:double] -> double
  (return (+ (* (- 1 t) a) (* t b))))

(function clamp :static :inline
  [x:float min_x:float max_x:float] -> float
  (if (< x min_x) (return min_x))
  (if (> x max_x) (return max_x))
  (return x))

(function clip :static :inline
  [xmin:int xmax:int ymin:int ymax:int
   (x 'int) (y 'int)] -> void
  (set 'x (clamp 'x xmin xmax))
  (set 'y (clamp 'y ymin ymax)))

(function max3z :static :inline [a:int b:int c:int] -> int
  (return (? (> a b)
             (? (> a c) a c)
             (? (> b c) b c))))

(function min3z :static :inline [a:int b:int c:int] -> int
  (return (? (< a b)
             (? (< a c) a c)
             (? (< b c) b c))))

(function max2z :static :inline [a:int b:int] -> int
  (return (? (< a b) b a)))

(function min2z :static :inline [a:int b:int] -> int
  (return (? (< a b) a b)))

###
### 2D floating point vector abstraction
###

(typedef V2 (named-struct V2 x double y double))

(each op ['+ '- '* '/]
  (function ,(symbol "v/" op) :static :inline
    ,(string "Vectorized " op)
    [a:V2 b:V2] -> V2
    (return (named-struct V2
              x (,op a.x b.x)
              y (,op a.y b.y))))
  (function ,(symbol "v/s" op) :static :inline
    ,(string "Element-wise vectorized " op)
    [scalar:double a:V2] -> V2
    (return (named-struct V2
              x (,op a.x scalar)
              y (,op a.y scalar)))))

(function v/make :static :inline
  [x:double y:double] -> V2
  (return (named-struct V2 x x y y)))

(function v/dot :static :inline
  [a:V2 b:V2] -> double
  (return (+ (* a.x b.x) (* a.y b.y))))

(function v/cross :static :inline
  [a:V2 b:V2] -> double
  (return (- (* a.x b.y) (* a.y b.x))))

(function v/len :static :inline
  [a:V2] -> double
  (return (sqrt (v/dot a a))))

(function v/norm :static :inline
  [a:V2] -> V2
  (return (v/s* (/ 1 (v/len a)) a)))

(function v/lerp :static :inline
  [a:V2 b:V2 t:double] -> V2
  (return (v/+ (v/s* t a) (v/s* (- 1 t) b))))

(function v/rotate-cw-90 :static :inline
  [a:V2] -> V2
  (return (v/make (- a.y) a.x)))

(function v/rotate-ccw-90 :static :inline
  [a:V2] -> V2
  (return (v/make a.y (- a.x))))

(function indexed-to-vs
  "Get vectors from an indexed collection. Must be freed with janet_sfree."
  [coords:JanetView n:*int] -> 'V2
  (if (band 1 coords.len) (janet-panic "expected an even number of point coordinates"))
  (def ret:*V2 (janet-smalloc (* (sizeof V2) (/ coords.len 2))))
  (for [(var i:int 0) (< i coords.len) (+= i 2)]
    (def x:double (janet-getnumber coords.items (+ i 0)))
    (def y:double (janet-getnumber coords.items (+ i 1)))
    (set (aref ret (/ i 2)) (v/make x y)))
  (set *n (/ coords.len 2))
  (return ret))

(function indexed-to-vs-join-end
  "Get vectors from an indexed collection. Must be freed with janet_sfree."
  [coords:JanetView n:*int] -> *V2
  (if (band 1 coords.len) (janet-panic "expected an even number of point coordinates"))
  (def ret:*V2 (janet-smalloc (* (sizeof V2) (+ 1 (/ coords.len 2)))))
  (for [(var i:int 0) (< i coords.len) (+= i 2)]
    (def x:double (janet-getnumber coords.items (+ i 0)))
    (def y:double (janet-getnumber coords.items (+ i 1)))
    (set (aref ret (/ i 2)) (v/make x y)))
  # Re-add first point
  (def x:double (janet-getnumber coords.items 0))
  (def y:double (janet-getnumber coords.items 1))
  (set (aref ret (/ coords.len 2)) (v/make x y))
  # Set N to coords/2 + 1
  (set *n (+ 1 (/ coords.len 2)))
  (return ret))

###
### Image creation, basic utilties, saving and loading
###

(typedef Image
  (named-struct Image
    parent (* (named-struct Image))
    width int
    height int
    channels int
    data *uint8_t
    stride int))

(function gc-image :static
  [p:*void s:size_t] -> int
  (unused s)
  (def image:*Image p)
  (unless image->parent
    (janet-free image->data))
  (return 0))

(function gc-mark :static
  [p:*void s:size_t] -> int
  (unused s)
  (def image:*Image p)
  (when image->parent (janet-mark (janet-wrap-abstract image->parent)))
  (return 0))

(abstract-type Image
  :name "gfx2d/image"
  :gcmark gc-mark
  :gc gc-image)

(comp-unless (dyn :shader-compile)

  (function create-image :static
    "Make an abstract image object"
    [parent:*Image width:int height:int channel:int stride:int data:*uint8_t] -> *Image
    (def image:*Image (janet-abstract-threaded Image-ATP (sizeof Image)))
    (set image->parent parent)
    (set image->width width)
    (set image->height height)
    (set image->channels channel)
    (set image->stride stride)
    (set image->data data)
    (return image))

  (cfunction blank
    "Create a new blank image"
    [width:int height:int &opt channel:int=4] -> *Image
    (if (> 1 width) (janet-panic "width must be positive"))
    (if (> 1 height) (janet-panic "height must be positive"))
    (if (> 1 channel) (janet-panic "channel must be between 1 and 4 inclusive"))
    (if (< 4 channel) (janet-panic "channel must be between 1 and 4 inclusive"))
    (def data:*uint8_t (janet-malloc (* width height channel)))
    (memset data 0 (* width height channel))
    (return (create-image NULL width height channel (* width channel) data)))

  (cfunction viewport
    "Create a new image that shares backing memory with another image. This allow parallel drawing in different threads."
    [img:*Image x:int y:int width:int height:int &opt allow-trimming:bool=0] -> *Image
    (def channel:int img->channels)
    (if allow-trimming
      (do # Clip the viewport if over the boundaries of the backing image
        (set x (min2z img->width (max2z 0 x)))
        (set y (min2z img->height (max2z 0 y)))
        (set width (min2z width (- img->width x)))
        (set height (min2z height (- img->height y))))
      (do # do not allow viewport to extend beyond backing boundaries
        (if (or (<= width 0) (< x 0)) (janet-panic "viewport out of range"))
        (if (or (<= height 0) (< y 0)) (janet-panic "viewport out of range"))
        (if (> (+ x width) img->width) (janet-panic "viewport out of range"))
        (if (> (+ y height) img->height) (janet-panic "viewport out of range"))))
    (def data-window:*uint8_t (+ img->data (* y img->stride) (* x channel)))
    (return (create-image img width height channel img->stride data-window)))

  (cfunction load
    "Load an image from disk into a buffer"
    [path:cstring] -> *Image
    (def width:int 0)
    (def height:int 0)
    (def c:int 0)
    (def (img 'uint8_t) (stbi-load path &width &height &c 0))
    (unless img (janet-panic "failed to load image"))
    (return (create-image NULL width height c (* width c) img)))

  # Generate image-writing for each image type
  # TODO - hdr
  (each ft ["bmp" "tga" "png" "jpg"]
    (def extra-params
      (case ft
        "jpg" ['quality:int] # - jpeg quality
        []))
    (def extra-args
      (case ft
        "png" ['img->stride]
        "jpg" ['quality] # - jpeg quality
        []))
    (cfunction ,(symbol 'save- ft)
      ,(string "Save an image to a file as a " ft)
      [path:cstring img:*Image ,;extra-params] -> *Image
      (def check:int (,(symbol 'stbi-write- ft) path img->width img->height img->channels img->data ,;extra-args))
      (if-not check (janet-panic "failed to write image"))
      (return img)))

  (cfunction save
    "Save an image to a file, auto-detecting the format. Supports PNG, BMP, TGA, and JPEG."
    [path:cstring img:*Image &opt quality:int=100] -> *Image
    (def (c (const *char)) path)
    (while *c (++ c)) # find end
    (while (and (> c path) (not= ,(chr ".") *c)) (-- c))
    (if (<= c path) (janet-panicf "no file extension for %s" path))
    (cond
      (not (strcmp c ".png")) (save-png path img)
      (not (strcmp c ".jpg")) (save-jpg path img quality)
      (not (strcmp c ".jpeg")) (save-jpg path img quality)
      (not (strcmp c ".bmp")) (save-bmp path img)
      (not (strcmp c ".tga")) (save-tga path img)
      (janet-panicf "unknown file extension %s for %s" c path))
    (return img)))

(function image-get-pixel :static :inline
  "extract a pixel"
  [img:*Image x:int y:int] -> uint32_t
  (var color-accum:uint32_t 0)
  (def data:*uint8_t img->data)
  (def stride:int img->stride)
  (def channels:int img->channels)
  (for [(def c:int 0) (< c img->channels) (++ c)]
    (def comp:uint8_t (aref data (+ (- channels c 1) (* x channels) (* y stride))))
    (set color-accum (+ (cast uint32_t comp) (<< color-accum 8))))
  (return color-accum))

(function image-get-pixel-bc :static :inline
  "extract a pixel bounds checked"
  [img:*Image x:int y:int] -> uint32_t
  (if (< x 0) (return 0))
  (if (< y 0) (return 0))
  (if (>= x img->width) (return 0))
  (if (>= y img->height) (return 0))
  (return (image-get-pixel img x y)))

(function image-set-pixel :static :inline
  "set a pixel"
  [img:*Image x:int y:int color:uint32_t] -> void
  (def data:*uint8_t img->data)
  (def stride:int img->stride)
  (def channels:int img->channels)
  (for [(def c:int 0) (< c channels) (++ c)]
    (set (aref data (+ c (* x channels) (* y stride)))
         (band (>> color (* c 8)) 0xFF))))

(function image-set-pixel-bc :static :inline
  "set a pixel bounds checked"
  [img:*Image x:int y:int color:uint32_t] -> void
  (if (< x 0) (return))
  (if (< y 0) (return))
  (if (>= x img->width) (return))
  (if (>= y img->height) (return))
  (image-set-pixel img x y color))

(comp-unless (dyn :shader-compile)

  (cfunction pixel
    "Read a pixel. Slow, be careful to use this in a loop."
    [img:*Image x:int y:int] -> uint32
    (def color:uint32_t (image-get-pixel-bc img x y))
    (return color))

  (cfunction set-pixel
    "Set a pixel. Slow, be careful to use this in a loop."
    [img:*Image x:int y:int color:uint32] -> *Image
    (image-set-pixel-bc img x y color)
    (return img)))

##
## Color Constants
##

# For unpacking pixels from image buffers more easily
(typedef Color
  (named-struct Color
    r int
    g int
    b int
    a int))

(function clampz :static :inline
  [x:int min_x:int max_x:int] -> int
  (if (< x min_x) (return min_x))
  (if (> x max_x) (return max_x))
  (return x))

(function colorsplit :static :inline
  [color:uint32_t] -> Color
  (return
    (named-struct Color
      r (cast int (band color 0xFF))
      g (cast int (band (>> color 8) 0xFF))
      b (cast int (band (>> color 16) 0xFF))
      a (cast int (band (>> color 24) 0xFF)))))

(function colorjoin :static :inline
  [r:int g:int b:int a:int] -> uint32_t
  (return (+ r (<< g 8) (<< b 16) (<< a 24))))

(def- colors
  {:red 0xFF0000FF
   :green 0xFF00FF00
   :blue 0xFFFF0000
   :clear 0x00000000
   :black 0xFF000000
   :white 0xFFFFFFFF
   :cyan 0xFFFFFF00
   :yellow 0xFF00FFFF
   :magenta 0xFFFF00FF})

(eachp [name value] colors
  (@ define ,name ,value)
  (unless (dyn :shader-compile)
    (emit-cdef (symbol name) (string "color constant for " name) ~(janet-wrap-number ,value))))

(comp-unless (dyn :shader-compile)

  (cfunction rgb
    "Make an RGB color constant from components. Each component is a number from 0 to 1."
    [r:double g:double b:double &opt a:double=1.0] -> uint32_t
    (return (colorjoin (cast int (* 255 r))
                       (cast int (* 255 g))
                       (cast int (* 255 b))
                       (cast int (* 255 a)))))

  (cfunction rgb-pre-mul
    "Make an RRB color constants from components and premultiply the alpha"
    [r:double g:double b:double &opt a:double=1.0] -> uint32_t
    (return (colorjoin (cast int (* 255 r a))
                       (cast int (* 255 g a))
                       (cast int (* 255 b a))
                       (cast int (* 255 a))))))

###
### Blending modes
###

# TODO - optimize?
#  - check floating point speed (probably better)
#  - optimize for 1, 2, and 3 components as well
#  - More blend modes?

(comp-unless (dyn :shader-compile)

  (function blend-over :static :inline
    ```
    Blend over (normal alpha compositing, like a painter)
    final.A   = src.A + dest.A * (1 - src.A)
    final.RGB = ((src.RGB * src.A) + (dest.RGB * dest.A * (1 - src.A))) / final.A
    ```
    [dest:uint32_t src:uint32_t] -> uint32_t
    (def d:Color (colorsplit dest))
    (def s:Color (colorsplit src))
    # TODO - use floating point for blending (if faster)
    (def ainv:int (- 255 s.a))
    (def aa:int (+ (* 255 s.a) (* d.a ainv)))
    (when aa
      (def a:int (/ aa 255))
      (def r:int (/ (+ (* s.a s.r 255) (* d.r d.a ainv)) aa))
      (def g:int (/ (+ (* s.a s.g 255) (* d.g d.a ainv)) aa))
      (def b:int (/ (+ (* s.a s.b 255) (* d.b d.a ainv)) aa))
      (return (colorjoin r g b a)))
    (return dest))

  (function blend-under :static :inline
    ```
    Blend under (normal alpha compositing, like a painter). Invert src and dest from blend-over.
    final.A   = dest.A + src.A * (1 - dest.A)
    final.RGB = ((dest.RGB * dest.A) + (src.RGB * src.A * (1 - dest.A))) / final.A
    ```
    [dest:uint32_t src:uint32_t] -> uint32_t
    (return (blend-over src dest)))

  (function blend-premul :static :inline
    ```
    Blend over with premultiplied alpha (normal alpha compositing, like a painter)
    final.A   = src.A + dest.A * (1 - src.A)
    final.RGB = src.RGB + (dest.RGB * (1 - src.A))
    ```
    [dest:uint32_t src:uint32_t] -> uint32_t
    (def d:Color (colorsplit dest))
    (def s:Color (colorsplit src))
    # TODO - use floating point for blending (if faster)
    (def ainv:int (- 255 s.a))
    (def a:int (/ (+ (* s.a 255) (* d.a ainv)) 255))
    (def r:int (/ (+ (* s.r 255) (* d.r ainv)) 255))
    (def g:int (/ (+ (* s.g 255) (* d.g ainv)) 255))
    (def b:int (/ (+ (* s.b 255) (* d.b ainv)) 255))
    (return (colorjoin r g b a)))

  # Blend operators
  (each [name op] [['add '+] ['sub '-] ['lighten 'max2z] ['darken 'min2z]]
    (function ,(symbol 'blend- name) :static :inline
      ,(string "Blending function for dest = dest " op " src ")
      [dest:uint32_t src:uint32_t] -> uint32_t
      (def d:Color (colorsplit dest))
      (def s:Color (colorsplit src))
      (def r:int (clampz (,op d.r s.r) 0 0xFF))
      (def g:int (clampz (,op d.g s.g) 0 0xFF))
      (def b:int (clampz (,op d.b s.b) 0 0xFF))
      (def a:int (clampz (,op d.a s.a) 0 0xFF))
      (return (colorjoin r g b a)))))

###
### Basic Drawing (disabled when compiling shaders)
###

(typedef BlendFunc (fn [uint32_t uint32_t] -> uint32_t))

(comp-unless (dyn :shader-compile)

  (cfunction stamp
    "Copy one image onto another"
    [dest:*Image src:*Image &opt dx:int=0 dy:int=0] -> *Image
    (if (not= src->channels dest->channels) (janet-panic "image channels don't match"))
    (if (= src->data dest->data) (janet-panic "cannot stamp self"))
    (def xmin:int (? (< dx 0) (- dx) 0))
    (def ymin:int (? (< dy 0) (- dy) 0))
    (def xoverflow:int (- (+ dx src->width) dest->width))
    (def yoverflow:int (- (+ dy src->height) dest->height))
    (def xmax:int (? (< xoverflow 0) src->width (- src->width xoverflow)))
    (def ymax:int (? (< yoverflow 0) src->height (- src->height yoverflow)))
    (polymorph src->channels [1 2 3 4]
      (for [(var y:int ymin) (< y ymax) (++ y)]
        (for [(var x:int xmin) (< x xmax) (++ x)]
          (image-set-pixel dest (+ dx x) (+ dy y) (image-get-pixel src x y)))))
    (return dest))

  (cfunction unpack
    ```
    Extract the :width (integer), :height (integer), :channels (integer 1-4),
    stride (integer), and underlying :data (buffer) from an image and return a struct.
    ```
    [img:*Image] -> JanetStruct
    (def st:*JanetKV (janet-struct-begin 5))
    (janet-struct-put st (janet-ckeywordv "width") (janet-wrap-integer img->width))
    (janet-struct-put st (janet-ckeywordv "height") (janet-wrap-integer img->height))
    (janet-struct-put st (janet-ckeywordv "channels") (janet-wrap-integer img->channels))
    (janet-struct-put st (janet-ckeywordv "stride") (janet-wrap-integer img->stride))
    (janet-struct-put st (janet-ckeywordv "data") (janet-wrap-pointer img->data))
    (return (janet-struct-end st)))

  (cfunction crop
    "Create a smaller sub-image from a larger image"
    [img:*Image x1:int y1:int new-width:int new-height:int] -> *Image
    (return
      (stamp
        (blank new-width new-height img->channels)
        img (- x1) (- y1))))

  (cfunction copy
    "Create a duplicate image"
    [img:*Image] -> *Image
    (def new-img:*Image (blank img->width img->height img->channels))
    (def byte-count:size_t (* img->width img->height img->channels))
    (memcpy new-img->data img->data byte-count)
    (return new-img))

  (cfunction diff
    "Take the difference of two images"
    [a:*Image b:*Image] -> *Image
    (if (not= a->channels b->channels) (janet-panic "images must have the same number of channels"))
    (if (or (not= a->height b->height) (not= a->width b->width)) (janet-panic "images must have matching dimensions"))
    (def dest:*Image (blank a->width a->height a->channels))
    (polymorph a->channels [1 2 3 4]
      (for [(var y:int 0) (< y a->height) (++ y)]
        (for [(var x:int 0) (< x a->width) (++ x)]
          (def color:uint32_t (blend-sub (image-get-pixel a x y) (image-get-pixel b x y)))
          (image-set-pixel dest x y color))))
    (return dest))

  (function get-blend-func :static :inline
    [x:Janet] -> BlendFunc
    (if
      (janet-keyeq x "over") (return blend-over)
      (janet-keyeq x "under") (return blend-under)
      (janet-keyeq x "premul") (return blend-premul)
      (janet-keyeq x "add") (return blend-add)
      (janet-keyeq x "sub") (return blend-sub)
      #(janet-keyeq x "mul") (return blend-mul)
      (janet-keyeq x "darken") (return blend-darken)
      (janet-keyeq x "lighten") (return blend-lighten)
      (janet-panicf "unknown blend mode %v" x)))

  (cfunction stamp-blend
    "Copy on image onto another with blending"
    [dest:*Image src:*Image blend-mode:Janet &opt dx:int=0 dy:int=0] -> *Image
    (if (not= src->channels dest->channels) (janet-panic "image channels don't match"))
    (if (= src->data dest->data) (janet-panic "cannot stamp self"))
    (def blender:BlendFunc (get-blend-func blend-mode))
    (def xmin:int (? (< dx 0) (- dx) 0))
    (def ymin:int (? (< dy 0) (- dy) 0))
    (def xoverflow:int (- (+ dx src->width) dest->width))
    (def yoverflow:int (- (+ dy src->height) dest->height))
    (def xmax:int (? (< xoverflow 0) src->width (- src->width xoverflow)))
    (def ymax:int (? (< yoverflow 0) src->height (- src->height yoverflow)))
    (polymorph src->channels [1 2 3 4]
      # TODO - automatically add all blend modes here if we add more
      (polymorph-cond blender [blend-add blend-sub blend-over blend-under blend-premul blend-lighten blend-darken]
        (for [(var y:int ymin) (< y ymax) (++ y)]
          (for [(var x:int xmin) (< x xmax) (++ x)]
            (def src-color:uint32_t (image-get-pixel src x y))
            (def dest-color:uint32_t (image-get-pixel dest (+ dx x) (+ dy y)))
            (def final-color:uint32_t (blender dest-color src-color))
            (image-set-pixel-bc dest (+ dx x) (+ dy y) final-color)))))
    (return dest))

  (cfunction resize
    "Resize an image, resampling as needed"
    [in:*Image new-width:int new-height:int] -> *Image
    (def out:*Image (blank new-width new-height in->channels))
    (def layout:stbir_pixel_layout
      (cond-expression
        (= in->channels 1) STBIR-1CHANNEL
        (= in->channels 2) STBIR-2CHANNEL
        (= in->channels 3) STBIR-RGB
        STBIR-4CHANNEL))
    (stbir-resize-uint8-srgb in->data in->width in->height in->stride
                             out->data out->width out->height out->stride
                             layout)
    (return out)))

###
### Shader!
### Evaluate file with `(dyn :pixel-shader)` set to use a different pixel shader.
###

# TODO - pick blend mode with shader

(def shader-args (dyn :shader-args '[color:uint32_t]))
(def shader-params (map first (map type-split shader-args)))
(def default-shader '(return color))

# When compiling shaders, just declare the shader so we can define it later
(compwhen (dyn :shader-compile)
  (function shader :static :inline [x:int y:int ,;shader-args] -> uint32_t))

(comp-unless (dyn :shader-compile)
  (function shader :static :inline
    [x:int y:int ,;shader-args] -> uint32_t
    (return color)))

###
### Draw a circle
###

(cfunction circle
  "Draw a circle"
  [img:*Image x:double y:double r:double ,;shader-args] -> *Image
  (var x1:int (floor (- x r)))
  (var x2:int (ceil (+ x r)))
  (var y1:int (floor (- y r)))
  (var y2:int (ceil (+ y r)))
  (clip 0 (- img->width 1) 0 (- img->height 1) &x1 &y1)
  (clip 0 (- img->width 1) 0 (- img->height 1) &x2 &y2)
  (polymorph img->channels [1 2 3 4]
    (for [(def yy:int y1) (<= yy y2) (++ yy)]
      (for [(def xx:int x1) (<= xx x2) (++ xx)]
        (def dx:double (- xx x))
        (def dy:double (- yy y))
        (when (>= (* r r) (+ (* dx dx) (* dy dy)))
          (def c1:uint32_t (shader xx yy ,;shader-params))
          (image-set-pixel img xx yy c1)))))
  (return img))

(cfunction ring
  "Draw a ring"
  [img:*Image x:double y:double r-inner:double r-outer:double ,;shader-args] -> *Image
  (var x1:int (floor (- x r-outer)))
  (var x2:int (ceil (+ x r-outer)))
  (var y1:int (floor (- y r-outer)))
  (var y2:int (ceil (+ y r-outer)))
  (clip 0 (- img->width 1) 0 (- img->height 1) &x1 &y1)
  (clip 0 (- img->width 1) 0 (- img->height 1) &x2 &y2)
  (polymorph img->channels [1 2 3 4]
    (for [(def yy:int y1) (<= yy y2) (++ yy)]
      (for [(def xx:int x1) (<= xx x2) (++ xx)]
        (def dx:double (- xx x))
        (def dy:double (- yy y))
        (when (and
                (>= (* r-outer r-outer) (+ (* dx dx) (* dy dy)))
                (<= (* r-inner r-inner) (+ (* dx dx) (* dy dy))))
          (def c1:uint32_t (shader xx yy ,;shader-params))
          (image-set-pixel img xx yy c1)))))
  (return img))

###
### Built-in simple text rendering with CP437 BIOS fonts
###
### By default, it is nice to be able to render text without loading any fonts. Very limited, but should work
### well for simple use cases. The built-in font is an 8x8 monospace bitmap fron that contains all the characters
### of the 437 code page from IBM compatible computers.
###

(typedef BitmapFont
  (struct
    gw int
    gh int
    data (const 'uint8_t)))

# Defines default font data
(comp-unless (dyn :shader-compile)
  (include "default_font.h")
  (include "tall_font.h")
  (include "olive_font.h"))

(comp-unless (dyn :shader-compile)
  (function select-font :static :inline
    "Select one of the built-in fonts"
    [font-name:JanetKeyword] -> (const 'BitmapFont)
    (return
      (cond-expression
        (= 0 (janet_cstrcmp font-name "default"))
        &default-font
        (= 0 (janet_cstrcmp font-name "tall"))
        &tall-font
        (= 0 (janet_cstrcmp font-name "olive"))
        &olive-font
        &default-font))))

(comp-unless (dyn :shader-compile)
  (function utf8-read-codepoint :static :inline
    "Read a codepoint from a string, and advance the cursor to the next utf8 character"
    [(c '(const 'uint8_t))] -> int
    (when (< ''c 0x80)
      (def code:int (aref 'c 0))
      (++ 'c)
      (return code))
    (when (>= ''c 0xF0) # 4 byte
      (def code:int
        (+ (<< (band 0x3 (aref 'c 0)) 18)
           (<< (band 0x3F (aref 'c 1)) 12)
           (<< (band 0x3F (aref 'c 2)) 6)
           (<< (band 0x3F (aref 'c 3)) 0)))
      (set 'c (+ 4 'c))
      (return code))
    (when (>= ''c 0xE0) # 3 byte
      (def code:int
        (+ (<< (band 0x0F (aref 'c 0)) 12)
           (<< (band 0x3F (aref 'c 1)) 6)
           (<< (band 0x3F (aref 'c 2)) 0)))
      (set 'c (+ 3 'c))
      (return code))
    (when (>= ''c 0xC0) # 2 byte
      (def code:int
        (+ (<< (band 0x1F (aref 'c 0)) 6)
           (<< (band 0x3F (aref 'c 1)) 0)))
      (set 'c (+ 2 'c))
      (return code))
    # invalid utf-8, just increment and return 0
    (def code:int ''c)
    (++ 'c)
    (return code)))

(comp-unless (dyn :shader-compile)
  (function unicode-to-cp437 :static :inline
    "Convert characters from unicode to the old IBM code page used by the default font"
    [codepoint:int] -> int
    (if (and (>= codepoint 20) (< codepoint 0x7F)) # ascii
      (return codepoint))
    (switch codepoint
            0x263A (return 0x01) 0x263B (return 0x02) 0x2665 (return 0x03) 0x2666 (return 0x04)
            0x2663 (return 0x05) 0x2660 (return 0x06) 0x2022 (return 0x07) 0x25D8 (return 0x08)
            0x25CB (return 0x09) 0x25D9 (return 0x0A) 0x2642 (return 0x0B) 0x2640 (return 0x0C)
            0x266A (return 0x0D) 0x266B (return 0x0E) 0x263C (return 0x0F) 0x25BA (return 0x10)
            0x25C4 (return 0x11) 0x2195 (return 0x12) 0x203C (return 0x13) 0x00B6 (return 0x14)
            0x00A7 (return 0x15) 0x25AC (return 0x16) 0x21A8 (return 0x17) 0x2191 (return 0x18)
            0x2193 (return 0x19) 0x2192 (return 0x1A) 0x2190 (return 0x1B) 0x221F (return 0x1C)
            0x2194 (return 0x1D) 0x25B2 (return 0x1E) 0x25BC (return 0x1F) 0x2302 (return 0x7F)
            0x00C7 (return 0x80) 0x00FC (return 0x81) 0x00E9 (return 0x82) 0x00E2 (return 0x83)
            0x00E4 (return 0x84) 0x00E0 (return 0x85) 0x00E5 (return 0x86) 0x00E7 (return 0x87)
            0x00EA (return 0x88) 0x00EB (return 0x89) 0x00E8 (return 0x8A) 0x00EF (return 0x8B)
            0x00EE (return 0x8C) 0x00EC (return 0x8D) 0x00C4 (return 0x8E) 0x00C5 (return 0x8F)
            0x00C9 (return 0x90) 0x00E6 (return 0x91) 0x00C6 (return 0x92) 0x00F4 (return 0x93)
            0x00F6 (return 0x94) 0x00F2 (return 0x95) 0x00FB (return 0x96) 0x00F9 (return 0x97)
            0x00FF (return 0x98) 0x00D6 (return 0x99) 0x00DC (return 0x9A) 0x00A2 (return 0x9B)
            0x00A3 (return 0x9C) 0x00A5 (return 0x9D) 0x20A7 (return 0x9E) 0x0192 (return 0x9F)
            0x00E1 (return 0xA0) 0x00ED (return 0xA1) 0x00F3 (return 0xA2) 0x00FA (return 0xA3)
            0x00F1 (return 0xA4) 0x00D1 (return 0xA5) 0x00AA (return 0xA6) 0x00BA (return 0xA7)
            0x00BF (return 0xA8) 0x2310 (return 0xA9) 0x00AC (return 0xAA) 0x00BD (return 0xAB)
            0x00BC (return 0xAC) 0x00A1 (return 0xAD) 0x00AB (return 0xAE) 0x00BB (return 0xAF)
            0x2591 (return 0xB0) 0x2592 (return 0xB1) 0x2593 (return 0xB2) 0x2502 (return 0xB3)
            0x2524 (return 0xB4) 0x2561 (return 0xB5) 0x2562 (return 0xB6) 0x2556 (return 0xB7)
            0x2555 (return 0xB8) 0x2563 (return 0xB9) 0x2551 (return 0xBA) 0x2557 (return 0xBB)
            0x255D (return 0xBC) 0x255C (return 0xBD) 0x255B (return 0xBE) 0x2510 (return 0xBF)
            0x2514 (return 0xC0) 0x2534 (return 0xC1) 0x252C (return 0xC2) 0x251C (return 0xC3)
            0x2500 (return 0xC4) 0x253C (return 0xC5) 0x255E (return 0xC6) 0x255F (return 0xC7)
            0x255A (return 0xC8) 0x2554 (return 0xC9) 0x2569 (return 0xCA) 0x2566 (return 0xCB)
            0x2560 (return 0xCC) 0x2550 (return 0xCD) 0x256C (return 0xCE) 0x2567 (return 0xCF)
            0x2568 (return 0xD0) 0x2564 (return 0xD1) 0x2565 (return 0xD2) 0x2559 (return 0xD3)
            0x2558 (return 0xD4) 0x2552 (return 0xD5) 0x2553 (return 0xD6) 0x256B (return 0xD7)
            0x256A (return 0xD8) 0x2518 (return 0xD9) 0x250C (return 0xDA) 0x2588 (return 0xDB)
            0x2584 (return 0xDC) 0x258C (return 0xDD) 0x2590 (return 0xDE) 0x2580 (return 0xDF)
            0x03B1 (return 0xE0) 0x00DF (return 0xE1) 0x0393 (return 0xE2) 0x03C0 (return 0xE3)
            0x03A3 (return 0xE4) 0x03C3 (return 0xE5) 0x00B5 (return 0xE6) 0x03C4 (return 0xE7)
            0x03A6 (return 0xE8) 0x0398 (return 0xE9) 0x03A9 (return 0xEA) 0x03B4 (return 0xEB)
            0x221E (return 0xEC) 0x03C6 (return 0xED) 0x03B5 (return 0xEE) 0x2229 (return 0xEF)
            0x2261 (return 0xF0) 0x00B1 (return 0xF1) 0x2265 (return 0xF2) 0x2264 (return 0xF3)
            0x2320 (return 0xF4) 0x2321 (return 0xF5) 0x00F7 (return 0xF6) 0x2248 (return 0xF7)
            0x00B0 (return 0xF8) 0x2219 (return 0xF9) 0x00B7 (return 0xFA) 0x221A (return 0xFB)
            0x207F (return 0xFC) 0x00B2 (return 0xFD) 0x25A0 (return 0xFE) 0x00A0 (return 0xFF)
            (return 0x00))))

(comp-unless (dyn :shader-compile)
  (cfunction draw-simple-text
    "Draw text with a default, bitmap on an image. Font should be one of :default, :tall, or :olive."
    [img:*Image x:int y:int xscale:int yscale:int text:cstring color:uint32_t &opt (font-name keyword (janet-ckeyword "default"))] -> *Image
    (if (< xscale 1) (janet-panic "xscale must be at least 1"))
    (if (< yscale 1) (janet-panic "yscale must be at least 1"))
    (def (font (const *BitmapFont)) (select-font font-name))
    # Hardcoded glyph widths for the built-in font.
    (def gw:int font->gw)
    (def gh:int font->gh)
    (def bytes-per-row:int (/ (+ 7 gw) 8))
    (def bytes-per-char:int (* bytes-per-row gh))
    (var xx:int x)
    (var yy:int y)
    (var (c (const *uint8_t)) (cast (const *uint8_t) text))
    (while *c
      (def codepoint:int (utf8-read-codepoint &c))
      (if (= codepoint ,(chr "\n")) (do (set yy (+ yy (* yscale gh))) (set xx x) (continue)))
      (def cp437:int (unicode-to-cp437 codepoint))
      (for [(var row:int 0) (< row gh) (++ row)]
        (var glyph-row:unsigned 0)
        # Collect glyph row into bits, up to 32 bit wide
        (for [(var index:int 0) (< index bytes-per-row) (++ index)]
          (set glyph-row (bor (<< glyph-row 8)
                              (cast unsigned (aref font->data (+ (* bytes-per-char cp437) (* row bytes-per-row) index))))))
        # Rasterize row
        (for [(var col:int (- gw 1)) (>= col 0) (-- col)]
          (if (band 1 glyph-row)
            (for [(var yoff:int 0) (< yoff yscale) (++ yoff)]
              (for [(var xoff:int 0) (< xoff xscale) (++ xoff)]
                (def xxx:int (+ (* xscale col) xx xoff))
                (def yyy:int (+ (* yscale row) yy yoff))
                (when (and (>= xxx 0) (>= yyy 0) (< xxx img->width) (< yyy img->height))
                  (image-set-pixel img xxx yyy color)))))
          (set glyph-row (>> glyph-row 1))))
      (set xx (+ xx (* xscale gw))))
    (return img)))

(comp-unless (dyn :shader-compile)
  (cfunction measure-simple-text
    "Return the height and width of text as a tuple. Font should be one of :default, :tall, or :olive."
    [text:cstring &opt (font-name keyword (janet-ckeyword "default"))] -> JanetTuple
    (var w:int 0)
    (var xcursor:int 0)
    (def (font (const *BitmapFont)) (select-font font-name))
    (var h:int font->gh)
    (var (c (const *uint8_t)) (cast (const *uint8_t) text))
    (while 'c
      (def codepoint:int (utf8-read-codepoint &c))
      (if (= codepoint ,(chr "\n"))
        (do
          (set w (max2z xcursor w))
          (set xcursor 0)
          (+= h font->gh))
        (+= xcursor font->gw)))
    (set w (max2z xcursor w))
    (def (ret 'Janet) (janet-tuple-begin 2))
    (set (aref ret 0) (janet-wrap-integer w))
    (set (aref ret 1) (janet-wrap-integer h))
    (return (janet-tuple-end ret))))

###
### Path operations
###

# TODO - disambiguate open vs. closed paths. Currently, we are auto-closing paths when needed, but if we want
# to make a more general path abstraction, we should probably encode that in the path abstraction itself.

# TODO - instead of `step`, have a `flatness` parameter.
(comp-unless (dyn :shader-compile)
  (cfunction bezier-path
    "Generate piece-wise, linear path from bezier control points"
    [points:indexed &opt step:double=0.005] -> 'JanetArray
    (if (band 1 points.len) (janet-panic "expected an even number of point coordinates"))
    (if (< points.len 6) (janet-panic "expected at least 3 points"))
    (def plen:int points.len)
    (def dpoints:*double (janet-smalloc (* (sizeof double) (* 2 plen))))
    (def bpoints:*double (+ dpoints plen)) # working buffer for calculating parametric points
    (for [(var i:int 0) (< i points.len) (+= i 2)]
      (def x:double (janet-getnumber points.items i))
      (def y:double (janet-getnumber points.items (+ i 1)))
      (set (aref dpoints i) x)
      (set (aref dpoints (+ 1 i)) y))

    # Use De Casteljau's algorithm
    # TODO - join nearly collinear segments based on a concept of "flatness" to avoid explosion in number of segments.
    (def arr:*JanetArray (janet-array 10))
    (for [(var t:double 0) (<= t (+ 1.0 (* step 0.5))) (+= t step)]
      (if (>= t (- 1 (/ step 2))) (set t 1))
      (memcpy bpoints dpoints (* (sizeof double) plen))
      (for [(var i:int 2) (< i plen) (+= i 2)]
        (for [(var j:int 0) (< j (- plen i)) (+= j 2)]
          (set (aref bpoints (+ j 0)) (lerp (aref bpoints (+ j 0)) (aref bpoints (+ 2 j)) t))
          (set (aref bpoints (+ j 1)) (lerp (aref bpoints (+ j 1)) (aref bpoints (+ 3 j)) t))))
      (def x:double (aref bpoints 0))
      (def y:double (aref bpoints 1))
      (janet-array-push arr (janet-wrap-number x))
      (janet-array-push arr (janet-wrap-number y)))

    (janet-sfree dpoints)
    (return arr)))

###
### Plotting (1-pixel lines)
###

(comp-unless (dyn :shader-compile)

  (cfunction plot
    "Draw a 1 pixel line from (x1, y1) to (x2, y2)"
    [img:*Image x1:int y1:int x2:int y2:int color:uint32] -> *Image
    # Use Bresenham's algorithm to draw the line
    (def dx:int (cast int (abs (- x2 x1))))
    (def dy:int (- (cast int (abs (- y2 y1)))))
    (def sx:int (? (< x1 x2) 1 -1))
    (def sy:int (? (< y1 y2) 1 -1))
    (def err:int (+ dx dy))
    (var x:int x1)
    (var y:int y1)
    (while 1
      (when (and (>= x 0) (< x img->width) (>= y 0) (< y img->height))
        (image-set-pixel img x y color))
      (when (>= (* 2 err) dy)
        (if (== x x2) (return img))
        (+= err dy)
        (+= x sx))
      (when (<= (* 2 err) dx)
        (if (== y y2) (return img))
        (+= err dx)
        (+= y sy)))
    (return img))

  (cfunction plot-path
    "Plot 1 pixel lines over a path"
    [img:*Image points:indexed color:uint32_t &opt join-end:bool=0] -> *Image
    (var npoints:int 0)
    (var (vs 'V2))
    (if join-end
      (set vs (indexed-to-vs-join-end points &npoints))
      (set vs (indexed-to-vs points &npoints)))
    (for [(var i:int 1) (< i npoints) (++ i)]
      (plot img
            (round (.x (aref vs (- i 1))))
            (round (.y (aref vs (- i 1))))
            (round (.x (aref vs i)))
            (round (.y (aref vs i)))
            color))
    (return img))

  (cfunction plot-ring
    "Plot a 1 pixel thick ring around (x, y) with radius r."
    [img:*Image x:int y:int r:int color:uint32_t] -> *Image
    # Midpoint circle algorithm
    (def d:int (/ (- 5 (* r 4)) 4))
    (def dx:int 0)
    (def dy:int r)
    (while 1
      (image-set-pixel-bc img (+ x dx) (+ y dy) color)
      (image-set-pixel-bc img (+ x dx) (- y dy) color)
      (image-set-pixel-bc img (- x dx) (+ y dy) color)
      (image-set-pixel-bc img (- x dx) (- y dy) color)
      (image-set-pixel-bc img (+ x dy) (+ y dx) color)
      (image-set-pixel-bc img (+ x dy) (- y dx) color)
      (image-set-pixel-bc img (- x dy) (+ y dx) color)
      (image-set-pixel-bc img (- x dy) (- y dx) color)
      (if (< d 0)
        (+= d (+ 1 (* 2 dx)))
        (do
          (+= d (+ 1 (* 2 (- dx dy))))
          (-- dy)))
      (++ dx)
      (if (> dx dy) (break)))
    (return img)))

###
### Raster path fill reference version
###

(function cross2z :static :inline
  "2d cross product"
  [ax:int ay:int bx:int by:int] -> int
  (return (- (* ax by) (* ay bx))))

(function seg-seg-intersect :static :inline
  "Check if a line segment intersects another segment"
  [s0x:int s0y:int s1x:int s1y:int r0x:int r0y:int r1x:int r1y:int] -> int
  # One way, check if each segment bisects the other segment - check cross products have different signs
  #   - S0R0xS0S1 differs in sign from S0R1xS0S1 and
  #   - R0S0xR0R1 differs in sign from R0S1xR0R1
  # If ray R intersects with S0, that is an intersection, but an intersection with point S1 is not.
  # Components
  (def s0s1x:int (- s1x s0x))
  (def s0s1y:int (- s1y s0y))
  (def r0r1x:int (- r1x r0x))
  (def r0r1y:int (- r1y r0y))
  (def s0r1x:int (- r1x s0x))
  (def s0r1y:int (- r1y s0y))
  (def s0r0x:int (- r0x s0x))
  (def s0r0y:int (- r0y s0y))
  (def r0s1x:int (- s1x r0x))
  (def r0s1y:int (- s1y r0y))
  (def r0s0x:int (- s0x r0x))
  (def r0s0y:int (- s0y r0y))
  # Crosses
  (def a:int (cross2z s0r1x s0r1y s0s1x s0s1y))
  (def b:int (cross2z s0r0x s0r0y s0s1x s0s1y))
  (def c:int (cross2z r0s0x r0s0y r0r1x r0r1y))
  (def d:int (cross2z r0s1x r0s1y r0r1x r0r1y))
  # Checks
  (return
    (and
      (not= (< a 0) (> 0 b))
      (not= (< c 0) (> 0 d)))))

(cfunction fill-path-prototype
  "Fill a path with a solid color - very slow but straightforward implementation."
  [img:*Image points:indexed ,;shader-args] -> *Image
  # 1. Get bounds of the path and extract coordinates
  (var xmin:int INT32-MAX)
  (var ymin:int INT32-MAX)
  (var xmax:int INT32-MIN)
  (var ymax:int INT32-MIN)
  (if (band 1 points.len) (janet-panic "expected an even number of point coordinates"))
  (if (< points.len 6) (janet-panic "expected at least 3 points"))
  (def plen:int (+ 2 points.len))
  (def (ipoints 'int) (janet-smalloc (* (sizeof int) plen)))
  (for [(var i:int 0) (< i points.len) (set i (+ i 2))]
    (def x:int (round (janet-getnumber points.items i)))
    (def y:int (round (janet-getnumber points.items (+ i 1))))
    (set (aref ipoints i) x)
    (set (aref ipoints (+ 1 i)) y)
    (set xmin (? (> x xmin) xmin x))
    (set ymin (? (> y ymin) ymin y))
    (set xmax (? (> x xmax) x xmax))
    (set ymax (? (> y ymax) y ymax)))
  # Add first point to end
  (set (aref ipoints points.len) (aref ipoints 0))
  (set (aref ipoints (+ 1 points.len)) (aref ipoints 1))
  # 2. Clipping
  (def xmin1:int xmin) # clipping should not change how we trace rays
  (clip 0 (- img->width 1) 0 (- img->height 1) &xmin &ymin)
  (clip 0 (- img->width 1) 0 (- img->height 1) &xmax &ymax)
  # 3. Fill the bounds of the path, running a ray crossing test for each pixel and color when we have an odd number of intersections
  (polymorph img->channels [1 2 3 4]
    (for [(var y:int ymin) (<= y ymax) (set y (+ y 1))]
      (for [(var x:int xmin) (<= x xmax) (set x (+ x 1))]
        (var intersection-count:int 0)
        (for [(var i:int 2) (< i plen) (set i (+ i 2))]
          (def intersect:int
            (seg-seg-intersect
              (aref ipoints (+ i 0))
              (aref ipoints (+ i 1))
              (aref ipoints (+ i -2))
              (aref ipoints (+ i -1))
              x y
              (- xmin1 1) y))
          (set intersection-count (+ intersection-count intersect)))
        (when (band 1 intersection-count)
          (def c1:uint32_t (shader x y ,;shader-params))
          (image-set-pixel img x y c1)))))
  # 4. Cleanup
  (janet-sfree ipoints)
  (return img))

###
### Better, Faster fill path
###

# Allow for sorting intersesctions
(typedef Intersection
  (named-struct Intersection
    xcoord int
    sign int
    tail int
    endpoint int))

(function intersection-compare :static :inline
  "Compare two intersections"
  [a:Intersection b:Intersection] -> int
  (if (not= a.xcoord b.xcoord) (return (- b.xcoord a.xcoord)))
  (if (not= a.endpoint b.endpoint) (return (? a.endpoint 1 -1)))
  (if (not= a.sign b.sign) (return (? (> a.sign b.sign) 1 -1)))
  (if (not= a.tail b.tail) (return (? (> a.tail b.tail) 1 -1)))
  # Other fields unused
  (return 0))

(function scanline-test :static :inline
  "Check if a line segment intersects a scanline. If so, return the x coordinate of the intersection as well.
  Will return 1 or -1 if there is an intersction, depending on if the segment has positive or negative dy
  (Zero dy is defined to never intersect the scanline)."
  [x1:int y1:int x2:int y2:int y-scan:int (out 'Intersection)] -> int
  (def sign:int 1)
  (when (> y1 y2)
    (swap x1 x2 int)
    (swap y1 y2 int)
    (set sign (- sign)))
  (if (< y-scan y1) (return 0))
  (if (> y-scan y2) (return 0))
  (if (= y1 y2) (return 0))
  (def dy:int (- y2 y1))
  (def dx:int (- x2 x1))
  (def ny:int (- y-scan y1))
  (def nx:int (* ny dx))
  (def xcoord:int (/ (+ (/ dy 2) (* x1 dy) nx) dy))
  (def endpoint:int (or (= y1 y-scan) (= y2 y-scan)))
  (set out->xcoord xcoord)
  (set out->sign sign)
  (set out->endpoint endpoint)
  (set out->tail (? endpoint (* sign (? (= y1 y-scan) 1 -1)) 0))
  (return 1))

(function fill-path-impl
  [img:*Image (points 'V2) npoints:int ,;shader-args] -> void
  # 1. Get y bounds of the path and extract coordinates
  (var ymin:int INT32-MAX)
  (var ymax:int INT32-MIN)
  (def (ibuf 'Intersection) (janet-smalloc (* (sizeof Intersection) (* 2 npoints))))
  (each-i 0 npoints
    (set ymin (min2z ymin (round (. (aref points i) y))))
    (set ymax (max2z ymax (round (. (aref points i) y)))))
  # Clip
  (set ymin (clampz ymin 0 (- img->height 1)))
  (set ymax (clampz ymax 0 (- img->height 1)))
  (polymorph img->channels [1 2 3 4]
    (for [(var y:int ymin) (<= y ymax) (++ y)]
      # Collect intersections
      (var intersection-count:int 0)
      (each-i 0 (- npoints 1)
        (def pa:V2 (aref points (+ i 0)))
        (def pb:V2 (aref points (+ i 1)))
        (var intersect:Intersection)
        (def did-intersect:int
          (scanline-test
            (cast int (round pb.x))
            (cast int (round pb.y))
            (cast int (round pa.x))
            (cast int (round pa.y))
            y &intersect))
        (when did-intersect
          (for [(var j:int 0) (< j intersection-count) (++ j)]
            (when (> (intersection-compare intersect (aref ibuf j)) 0)
              (swap intersect (aref ibuf j) Intersection)))
          (set (aref ibuf intersection-count) intersect)
          (++ intersection-count)))

      # Discard some intersections
      (var last:Intersection (aref ibuf 0))
      (var cursor:int 1)
      (var nodrop:int 0)
      (each-i 1 intersection-count
        (def inter:Intersection (aref ibuf i))
        (var keep:int 1)
        # Discard
        (when (and (not nodrop) inter.endpoint last.endpoint)
          (when (and (not= inter.tail last.tail) (= inter.sign last.sign))
            (set keep 0)
            (set nodrop 1)))
        (when keep
          (set (aref ibuf cursor) inter)
          (++ cursor)
          (set nodrop 0)
          # Deal with certain order problems
          (set inter.xcoord (max2z inter.xcoord last.xcoord))
          (if (> 0 (intersection-compare inter last))
            (set last inter))))
      (set intersection-count cursor)

      # Draw scan lines
      (for [(var i:int 0) (< i (- intersection-count 1)) (+= i 2)]
        (def i1:Intersection (aref ibuf (+ i 0)))
        (def i2:Intersection (aref ibuf (+ i 1)))
        (def ix1:int i1.xcoord)
        (def ix2:int i2.xcoord)
        (def x1:int (clampz ix1 0 (- img->width 1)))
        (def x2:int (clampz (+ 1 ix2) 0 img->width))
        (for [(var x:int x1) (< x x2) (++ x)]
          (def c1:uint32_t (shader x y ,;shader-params))
          (image-set-pixel img x y c1)))))

  (janet-sfree ibuf))

(cfunction fill-path
  "Fill a path"
  [img:*Image points:indexed ,;shader-args] -> *Image
  (var npoints:int 0)
  (def (vs 'V2) (indexed-to-vs-join-end points &npoints))
  (fill-path-impl img vs npoints ,;shader-params)
  (janet-sfree vs)
  (return img))

(cfunction fill-rect
  "Fill a rectangle"
  [img:*Image x1:double y1:double x2:double y2:double ,;shader-args] -> *Image
  (def (vs (array V2))
    (array
      (v/make x1 y1)
      (v/make x1 y2)
      (v/make x2 y2)
      (v/make x2 y1)))
  (fill-path-impl img vs 4 ,;shader-params)
  (return img))

(cfunction stroke-path
  "Stroke a line along a path"
  [img:*Image points:indexed ,;shader-args &opt thickness:double=1 join-end:bool=0] -> *Image
  (var npoints:int 0)
  (var (vs 'V2))
  (if join-end
    (set vs (indexed-to-vs-join-end points &npoints))
    (set vs (indexed-to-vs points &npoints)))
  (each-i 1 npoints
    (def A:V2 (aref vs (- i 1)))
    (def B:V2 (aref vs i))
    (def AB:V2 (v/- B A))
    (def leg:V2 (v/s* thickness (v/rotate-cw-90 (v/norm AB))))
    (def p1:V2 (v/+ A leg))
    (def p2:V2 (v/- A leg))
    (def p3:V2 (v/- B leg))
    (def p4:V2 (v/+ B leg))
    (def (ps (array V2)) @[p1 p2 p3 p4 p1])
    (fill-path-impl img ps 5 ,;shader-params))
  (each-i 0 npoints
    (def P:V2 (aref vs i))
    (circle img P.x P.y thickness ,;shader-params))
  (janet-sfree vs:*V2) # self-test for mangling of type-grafted symbols
  (return img))

(comp-unless (dyn :shader-compile)
  (module-entry "gfx2d"))
