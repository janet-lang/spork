###
### gfx2d.janet
###
### Various simple CPU-based 2d graphics tools suitable for demos, visulizations, and charting.
###
### Includes:
### * Wrappers around stb_image for loading, saving, and modifying images.
###

### TODO
### [x] - save and load to file
### [x] - rect and circle primitive
### [x] - image blitting
### [x] - image cropping
### [x] - testing harness
### [x] - get/set individual pixels (mostly for testing)
### [x] - text w/ simple font
### [ ] - lines/paths (w/ thickness)
### [ ] - blending
### [x] - image resizing
### [ ] - bezier
### [x] - fill path (raycast rasterizer)
### [ ] - triangle strip/fan/list
### [ ] - LUTs (possibly w/ shaders)
### [ ] - Gradients (possibly w/ shaders)
### [ ] - Better 2D point abstraction
### [ ] - Affine transforms

### Stretch TODO
### [ ] - vector font rendering
### [ ] - anti-aliasing w/ MXAA
### [ ] - shaders using cjanet-jit
### [ ] - sub-images for rendering / alternatives to JanetBuffer for data storage
### [ ] - multithreading

(use ../spork/cjanet)

(include <janet.h>)
(include <string.h>)
(include <math.h>)
(include <assert.h>)

(@ define STBIW_WINDOWS_UTF8)

(include "stb_image.h")
(include "stb_image_write.h")
(include "stb_image_resize2.h")

###
### Image creation, basic utilties, saving and loading
###

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
     (assert 0)))

(defn- bind-image-code
  :cjanet-spliced-macro
  "Generate code to unpack an image."
  [img &opt sym-prefix]
  (default sym-prefix "")
  ~[(def ,(symbol sym-prefix 'width:int) 0)
    (def ,(symbol sym-prefix 'height:int) 0)
    (def ,(symbol sym-prefix 'channels:int) 0)
    (def (,(symbol sym-prefix 'buf) 'JanetBuffer) NULL)
    (unwrap-image
      ,img
      (addr ,(symbol sym-prefix 'buf))
      (addr ,(symbol sym-prefix 'width))
      (addr ,(symbol sym-prefix 'height))
      (addr ,(symbol sym-prefix 'channels)))
    (def (,(symbol sym-prefix 'data) 'uint8_t) ,(symbol sym-prefix 'buf->data))
    (def ,(symbol sym-prefix 'stride:int) (* ,(symbol sym-prefix 'channels) ,(symbol sym-prefix 'width)))])

(function wrap-image :static :inline
   "wrap an image"
   [(buf 'JanetBuffer) width:int height:int channels:int] -> JanetTuple
   (def (tup 'Janet) (janet-tuple-begin 4))
   (set (aref tup 0) (janet-wrap-buffer buf))
   (set (aref tup 1) (janet-wrap-integer width))
   (set (aref tup 2) (janet-wrap-integer height))
   (set (aref tup 3) (janet-wrap-integer channels))
   (return (janet-tuple-end tup)))

(function unwrap-image :static :inline
   "unwrap an image"
   [img:JanetTuple (data ''JanetBuffer) (width 'int) (height 'int) (channels 'int)] -> void
   (if (> 4 (janet-tuple-length img)) # allow for extra attributes
     (janet-panicf "expected image (length 4 tuple), got %V" (janet-wrap-tuple img)))
   (set (aref data 0) (janet-getbuffer img 0))
   (set (aref width 0) (janet-getinteger img 1))
   (set (aref height 0) (janet-getinteger img 2))
   (set (aref channels 0) (janet-getinteger img 3)))

(cfunction blank
  "Create a new blank image"
  [width:int height:int channel:int] -> tuple
  (if (> 1 width) (janet-panic "width must be positive"))
  (if (> 1 height) (janet-panic "height must be positive"))
  (if (> 1 channel) (janet-panic "channel must be between 1 and 4 inclusive"))
  (if (< 4 channel) (janet-panic "channel must be between 1 and 4 inclusive"))

  (def (buf 'JanetBuffer) (janet-buffer (* width height channel)))
  (set buf->count (* width height channel))
  (memset buf->data 0 (* width height channel))

  # Package buffer and dimensions in tuple
  (return (wrap-image buf width height channel)))

(cfunction load
  "Load an image from disk into a buffer"
  [path:cstring] -> tuple
  (def width:int 0)
  (def height:int 0)
  (def c:int 0)
  (def (img 'uint8_t) (stbi-load path ;width ;height ;c 0))
  (unless img (janet-panic "failed to load image"))

  # Copy into buffer
  (def (buf 'JanetBuffer) (janet-buffer (* width height c)))
  (memcpy buf->data img (* width height c))
  (set buf->count (* width height c))
  (stbi-image-free img) # TODO - remove this alloc, copy, free?

  # Package buffer and dimensions in tuple
  (return (wrap-image buf width height c)))

# Generate image-writing for each image type
# TODO - hdr
(each ft ["bmp" "tga" "png" "jpg"]
  (def extra-params
    (case ft
      "jpg" ['quality:int] # - jpeg quality
      []))
  (def extra-args
    (case ft
      "png" ['(* width channels)]
      "jpg" ['quality] # - jpeg quality
      []))
  (cfunction ,(symbol 'save- ft)
     ,(string "Save an image to a file as a " ft)
     [path:cstring img:tuple ,;extra-params] -> tuple
     (def width:int 0)
     (def height:int 0)
     (def channels:int 0)
     (def (buf 'JanetBuffer) NULL)
     (unwrap-image img ;buf ;width ;height ;channels)
     (def check:int (,(symbol 'stbi-write- ft) path width height channels buf->data ,;extra-args))
     (if-not check (janet-panic "failed to write image"))
     (return img)))

(defn- get-pixel
  :cjanet-block-macro
  "extract a pixel"
  [color-var x y &opt prefix]
  (default prefix "")
  (def channels (symbol prefix 'channels))
  (def data (symbol prefix 'data))
  (def stride (symbol prefix 'stride))
  ~(do
     (var color-accum:uint32_t 0)
     (for [(def c:int 0) (< c ,channels) (++ c)]
       (def comp:uint8_t (aref ,data (+ (- ,channels c 1) (* ,x ,channels) (* ,y ,stride))))
       (set color-accum (+ (cast uint32_t comp) (<< color-accum 8))))
     (set ,color-var color-accum)))

(defn- set-pixel
  :cjanet-block-macro
  "set a pixel"
  [x y color &opt prefix]
  (default prefix "")
  (def channels (symbol prefix 'channels))
  (def data (symbol prefix 'data))
  (def stride (symbol prefix 'stride))
  ~(for [(def c:int 0) (< c ,channels) (++ c)]
     (set (aref ,data (+ c (* ,x ,channels) (* ,y ,stride)))
        (band (>> ,color (* c 8)) 0xFF))))

(cfunction pixel
  "Read a pixel. Slow, be careful to use this in a loop."
  [img:tuple x:int y:int] -> uint32
  ,;(bind-image-code 'img)
  (var color:uint32_t 0)
  (get-pixel color x y)
  (if (< channels 4) (+= color 0xFF000000))
  (return color))

(cfunction set-pixel
  "Set a pixel. Slow, be careful to use this in a loop."
  [img:tuple x:int y:int color:uint32] -> tuple
  ,;(bind-image-code 'img)
  (set-pixel x y color)
  (return img))

##
## Color Constants
##

(def- colors
  {:red     0xFF0000FF
   :green   0xFF00FF00
   :blue    0xFFFF0000
   :clear   0x00000000
   :black   0xFF000000
   :white   0xFFFFFFFF
   :cyan    0xFFFFFF00
   :yellow  0xFF00FFFF
   :magenta 0xFFFF00FF})

(eachp [name value] colors
  (@ define ,name ,value)
  (emit-cdef (symbol name) (string "color constant for " name) ~(janet-wrap-number ,value)))

###
### Math helpers
###

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

(function lerp :static :inline
  [a:float b:float t:float] -> float
  (return (+ (* (- 1 t) a) (* t b))))

(function unlerp :static :inline
  [x:float x0:float x1:float] -> float
  (return (/ (- x x0) (- x1 x0)))) # is this useful (divide by zero)?

(function clamp :static :inline
  [x:float min_x:float max_x:float] -> float
  (if (< x min_x) (return min_x))
  (if (> x max_x) (return max_x))
  (return x))

(function clampz :static :inline
  [x:int min_x:int max_x:int] -> int
  (if (< x min_x) (return min_x))
  (if (> x max_x) (return max_x))
  (return x))

(function colorsplit :static :inline
  [color:uint32_t (r 'int) (g 'int) (b 'int) (a 'int)] -> void
  (set 'r (cast int (band color 0xFF)))
  (set 'g (cast int (band (>> color 8) 0xFF)))
  (set 'b (cast int (band (>> color 16) 0xFF)))
  (set 'a (cast int (band (>> color 24) 0xFF))))

(function colorjoin :static :inline
  [r:int g:int b:int a:int] -> uint32_t
  (return (+ r (<< g 8) (<< b 16) (<< a 24))))

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

(function barycentric :static :inline
  "calculate barycentric coordinates in 2d"
  [px:int py:int x1:int y1:int x2:int y2:int x3:int y3:int (t0 'float) (t1 'float) (t2 'float)] -> int
  (def v0x:int (- x2 x1))
  (def v0y:int (- y2 y1))
  (def v1x:int (- x3 x1))
  (def v1y:int (- y3 y1))
  (def v2x:int (- px x1))
  (def v2y:int (- py y1))
  (def denom_inv:double (/ 1 (cast double (- (* v0x v1y) (* v1x v0y)))))
  (def v:float (* denom_inv (- (* v2x v1y) (* v1x v2y))))
  (def w:float (* denom_inv (- (* v0x v2y) (* v2x v0y))))
  (def u:float (- 1.0 v w))
  (if t0 (set 't0 v))
  (if t1 (set 't1 w))
  (if t2 (set 't2 u))
  (let [epsilon:float 0.00001
        lowt:float (- epsilon)
        hit:float (+ 1 epsilon)]
    (return (and (>= u lowt) (<= u hit)
                 (>= v lowt) (<= v hit)
                 (>= w lowt) (<= w hit)))))

###
### Blending modes
###

(function blend-default [dest:uint32_t src:uint32_t] -> uint32_t
          (return src))

# Blend operators
(each [name op] [['add '+] ['sub '-] ['mul '*]]
  (function ,(symbol 'blend- name) :static :inline
      ,(string "Blending function for dest = dest " op " src ")
      [dest:uint32_t src:uint32_t] -> uint32_t
      (var dest-r:int 0)
      (var dest-g:int 0)
      (var dest-b:int 0)
      (var dest-a:int 0)
      (var src-r:int 0)
      (var src-g:int 0)
      (var src-b:int 0)
      (var src-a:int 0)
      (colorsplit dest ;dest-r ;dest-g ;dest-b ;dest-a)
      (colorsplit src ;src-r ;src-g ;src-b ;src-a)
      (def r:int (clampz (,op dest-r src-r) 0 0xFF))
      (def g:int (clampz (,op dest-g src-g) 0 0xFF))
      (def b:int (clampz (,op dest-b src-b) 0 0xFF))
      (def a:int (clampz (,op dest-a src-a) 0 0xFF))
      (return (colorjoin r g b a))))

###
### Basic Drawing
###

(cfunction stamp
  "Copy one image onto another"
  [dest:tuple src:tuple dx:int dy:int] -> tuple
  ,;(bind-image-code 'src "src-")
  ,;(bind-image-code 'dest "dest-")
  (if (not= src-channels dest-channels) (janet-panic "image channels don't match"))
  (if (= src-data dest-data) (janet-panic "cannot stamp self"))
  (def xmin:int (? (< dx 0) (- dx) 0))
  (def ymin:int (? (< dy 0) (- dy) 0))
  (def xoverflow:int (- (+ dx src-width) dest-width))
  (def yoverflow:int (- (+ dy src-height) dest-height))
  (def xmax:int (? (< xoverflow 0) src-width (- src-width xoverflow)))
  (def ymax:int (? (< yoverflow 0) src-height (- src-height yoverflow)))
  (polymorph src-channels [1 2 3 4]
    (for [(var y:int ymin) (< y ymax) (++ y)]
      (for [(var x:int xmin) (< x xmax) (++ x)]
        (var color:uint32_t 0)
        (get-pixel color x y "src-")
        (set-pixel (+ dx x) (+ dy y) color "dest-"))))
  (return dest))

(cfunction crop
  "Create a smaller sub-image from a larger image"
  [img:tuple x1:int y1:int new-width:int new-height:int] -> tuple
  ,;(bind-image-code 'img "img-")
  (return
    (stamp
      (blank new-width new-height img-channels)
      img (- x1) (- y1))))

(cfunction diff
  "Take the difference of two images"
  [a:tuple b:tuple] -> tuple
  ,;(bind-image-code 'a "a-")
  ,;(bind-image-code 'b "b-")
  (if (not= a-channels b-channels) (janet-panic "images must have the same number of channels"))
  (if (or (not= a-height b-height) (not= a-width b-width)) (janet-panic "images must have matching dimensions"))
  (def dest:JanetTuple (blank a-width a-height a-channels))
  ,;(bind-image-code 'dest "dest-")
  (polymorph a-channels [1 2 3 4]
    (for [(var y:int 0) (< y a-height) (++ y)]
      (for [(var x:int 0) (< x a-width) (++ x)]
        (var acolor:uint32_t 0)
        (var bcolor:uint32_t 0)
        (get-pixel acolor x y "a-")
        (get-pixel bcolor x y "b-")
        (def color:uint32_t (blend-sub acolor bcolor))
        (set-pixel x y color "dest-"))))
  (return dest))

(cfunction resize
  "Resize an image, resampling as needed"
  [input-img:JanetTuple new-width:int new-height:int] -> JanetTuple
  ,;(bind-image-code 'input-img 'in-)
  (def new-img:JanetTuple (blank new-width new-height in-channels))
  ,;(bind-image-code 'new-img 'out-)
  (def layout:stbir_pixel_layout
    (cond-expression
      (= in-channels 1) STBIR-1CHANNEL
      (= in-channels 2) STBIR-2CHANNEL
      (= in-channels 3) STBIR-RGB
      STBIR-4CHANNEL))
  (stbir-resize-uint8-srgb in-data in-width in-height in-stride
                           out-data out-width out-height out-stride
                           layout)
  (return new-img))

###
### Shader!
###

# TODO - customize
(function shader :static :inline
  [x:int y:int color:uint32_t] -> uint32_t
  (return color))

###
### Shader Kernels
###

(cfunction rect
  "Draw a rectangle on an image"
  [img:tuple x1:int y1:int x2:int y2:int color:uint32] -> tuple
  ,;(bind-image-code 'img)
  (clip 0 (- width 1) 0 (- height 1) ;x1 ;y1)
  (clip 0 (- width 1) 0 (- height 1) ;x2 ;y2)
  (polymorph channels [1 2 3 4]
    (for [(def y:int y1) (<= y y2) (++ y)]
      (for [(def x:int x1) (<= x x2) (++ x)]
        (def c1:uint32_t (shader x y color))
        (set-pixel x y c1))))
  (return img))

(cfunction circle
  "Draw a circle"
  [img:tuple x:int y:int r:double color:uint32] -> tuple
  ,;(bind-image-code 'img)
  (def fr:int (floor (- r)))
  (def cr:int (ceil r))
  (var x1:int fr)
  (var x2:int cr)
  (var y1:int fr)
  (var y2:int cr)
  (clip (- 0 x) (- width x 1) (- 0 y) (- height y 1) ;x1 ;y1)
  (clip (- 0 x) (- width x 1) (- 0 y) (- height y 1) ;x2 ;y2)
  (polymorph channels [1 2 3 4]
    (for [(def yy:int y1) (<= yy y2) (++ yy)]
      (for [(def xx:int x1) (<= xx x2) (++ xx)]
        (when (>= (* r r) (+ (* xx xx) (* yy yy)))
          (def c1:uint32_t (shader (+ x xx) (+ y yy) color))
          (set-pixel (+ x xx) (+ y yy) c1)))))
  (return img))

(cfunction line
  "Draw a line from x1,y1 to x2,y2"
  [img:tuple x1:int y1:int x2:int y2:int color:uint32] -> tuple
  ,;(bind-image-code 'img "img-")
  # Use Bresenham's algorithm to draw the line
  (def dx:int (cast int (abs (- x2 x1))))
  (def dy:int (- (cast int (abs (- y2 y1)))))
  (def sx:int (? (< x1 x2) 1 -1))
  (def sy:int (? (< y1 y2) 1 -1))
  (def err:int (+ dx dy))
  (var x:int x1)
  (var y:int y1)
  (while 1
    (when (and (>= x 0) (< x img-width) (>= y 0) (< y img-height))
      (def c1:uint32_t (shader x y color))
      (set-pixel x y c1 "img-"))
    (when (>= (* 2 err) dy)
      (if (== x x2) (return img))
      (+= err dy)
      (+= x sx))
    (when (<= (* 2 err) dx)
      (if (== y y2) (return img))
      (+= err dx)
      (+= y sy))))

(function triangle-impl :static :inline
  [img:JanetTuple x1:int y1:int x2:int y2:int x3:int y3:int color:uint32_t] -> JanetTuple
  # points 1 2 3 are sorted by non-decreasing y
  ,;(bind-image-code 'img)
  # 2. Use modified line algorithm for top half
  (for [(var y:int y1) (< y y2) (++ y)]
    (def yt-12:float (unlerp y y1 y2))
    (def yt-13:float (unlerp y y1 y3))
    (def x-a:int (cast int (floor (lerp x1 x2 yt-12))))
    (def x-b:int (cast int (ceil (lerp x1 x3 yt-13))))
    (sort2 x-a x-b int)
    (for [(var x:int x-a) (<= x x-b) (++ x)]
      (when (and (> x 0) (< x width) (> y 0) (< y height))
        (def c1:uint32_t (shader x y color))
        (set-pixel x y c1))))
  # 3. Use modified line algorithm for bottom half
  (for [(var y:int y2) (< y y3) (++ y)]
    (def yt-23:float (unlerp y y2 y3))
    (def yt-13:float (unlerp y y1 y3))
    (def x-a:int (cast int (floor (lerp x2 x3 yt-23))))
    (def x-b:int (cast int (ceil (lerp x1 x3 yt-13))))
    (sort2 x-a x-b int)
    (for [(var x:int x-a) (<= x x-b) (++ x)]
      (when (and (> x 0) (< x width) (> y 0) (< y height))
        (def c1:uint32_t (shader x y color))
        (set-pixel x y c1))))
  # 4. last row in case y2 == y3
  (def x-a:int x3)
  (def x-b:int (? (= y2 y3) x2 x3))
  (sort2 x-a x-b int)
  (for [(var x:int x-a) (<= x x-b) (++ x)]
    (when (and (> x 0) (< x width) (> y3 0) (< y3 height))
      (def c1:uint32_t (shader x y3 color))
      (set-pixel x y3 c1)))
  (return img))

(cfunction triangle
    "Fill a triangle"
    [img:JanetTuple x1:int y1:int x2:int y2:int x3:int y3:int color:uint32] -> JanetTuple
    # 1. Sort the coordinates by increasing y
    (if (< y1 y2)
      (if (< y1 y3)
        (if (< y2 y3)
          (return (triangle-impl img x1 y1 x2 y2 x3 y3 color))
          (return (triangle-impl img x1 y1 x3 y3 x2 y2 color)))
        (return (triangle-impl img x3 y3 x1 y1 x2 y2 color)))
      (if (< y2 y3)
        (if (< y1 y3)
          (return (triangle-impl img x2 y2 x1 y1 x3 y3 color))
          (return (triangle-impl img x2 y2 x3 y3 x1 y1 color)))
        (return (triangle-impl img x3 y3 x2 y2 x1 y1 color)))))

(cfunction triangle2
   "Fill a triangle alternate implementation"
   [img:JanetTuple x1:int y1:int x2:int y2:int x3:int y3:int color:uint32_t] -> JanetTuple
   (def xmin:int (min3z x1 x2 x3))
   (def xmax:int (max3z x1 x2 x3))
   (def ymin:int (min3z y1 y2 y3))
   (def ymax:int (max3z y1 y2 y3))
   ,;(bind-image-code 'img)
   (clip 0 (- width 1) 0 (- height 1) ;xmin ;ymin)
   (clip 0 (- width 1) 0 (- height 1) ;xmax ;ymax)
   (for [(var y:int ymin) (<= y ymax) (++ y)]
     (for [(var x:int xmin) (<= x xmax) (++ x)]
       (when (barycentric x y x1 y1 x2 y2 x3 y3 nil nil nil)
         (def c1:uint32_t (shader x y color))
         (set-pixel x y c1))))
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
(include "default_font.h")
(include "tall_font.h")
(include "olive_font.h")

(function select-font :static :inline
  "Select one of the built-in fonts"
  [font-name:JanetKeyword] -> (const 'BitmapFont)
  (return
    (cond-expression
      (= 0 (janet_cstrcmp font-name "default"))
      ;default-font
      (= 0 (janet_cstrcmp font-name "tall"))
      ;tall-font
      (= 0 (janet_cstrcmp font-name "olive"))
      ;olive-font
      ;default-font)))

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
  (return code))

(function unicode-to-cp437 :static :inline
  "Convert characters from unicode to the old IBM code page used by the default font"
  [codepoint:int] -> int
  (if (and (>= codepoint 20) (< codepoint 0x7F)) # ascii
    (return codepoint))
  (switch codepoint
    0x263A (return 0x01)
    0x263B (return 0x02)
    0x2665 (return 0x03)
    0x2666 (return 0x04)
    0x2663 (return 0x05)
    0x2660 (return 0x06)
    0x2022 (return 0x07)
    0x25D8 (return 0x08)
    0x25CB (return 0x09)
    0x25D9 (return 0x0A)
    0x2642 (return 0x0B)
    0x2640 (return 0x0C)
    0x266A (return 0x0D)
    0x266B (return 0x0E)
    0x263C (return 0x0F)
    0x25BA (return 0x10)
    0x25C4 (return 0x11)
    0x2195 (return 0x12)
    0x203C (return 0x13)
    0x00B6 (return 0x14)
    0x00A7 (return 0x15)
    0x25AC (return 0x16)
    0x21A8 (return 0x17)
    0x2191 (return 0x18)
    0x2193 (return 0x19)
    0x2192 (return 0x1A)
    0x2190 (return 0x1B)
    0x221F (return 0x1C)
    0x2194 (return 0x1D)
    0x25B2 (return 0x1E)
    0x25BC (return 0x1F)
    0x2302 (return 0x7F)
    0x00C7 (return 0x80)
    0x00FC (return 0x81)
    0x00E9 (return 0x82)
    0x00E2 (return 0x83)
    0x00E4 (return 0x84)
    0x00E0 (return 0x85)
    0x00E5 (return 0x86)
    0x00E7 (return 0x87)
    0x00EA (return 0x88)
    0x00EB (return 0x89)
    0x00E8 (return 0x8A)
    0x00EF (return 0x8B)
    0x00EE (return 0x8C)
    0x00EC (return 0x8D)
    0x00C4 (return 0x8E)
    0x00C5 (return 0x8F)
    0x00C9 (return 0x90)
    0x00E6 (return 0x91)
    0x00C6 (return 0x92)
    0x00F4 (return 0x93)
    0x00F6 (return 0x94)
    0x00F2 (return 0x95)
    0x00FB (return 0x96)
    0x00F9 (return 0x97)
    0x00FF (return 0x98)
    0x00D6 (return 0x99)
    0x00DC (return 0x9A)
    0x00A2 (return 0x9B)
    0x00A3 (return 0x9C)
    0x00A5 (return 0x9D)
    0x20A7 (return 0x9E)
    0x0192 (return 0x9F)
    0x00E1 (return 0xA0)
    0x00ED (return 0xA1)
    0x00F3 (return 0xA2)
    0x00FA (return 0xA3)
    0x00F1 (return 0xA4)
    0x00D1 (return 0xA5)
    0x00AA (return 0xA6)
    0x00BA (return 0xA7)
    0x00BF (return 0xA8)
    0x2310 (return 0xA9)
    0x00AC (return 0xAA)
    0x00BD (return 0xAB)
    0x00BC (return 0xAC)
    0x00A1 (return 0xAD)
    0x00AB (return 0xAE)
    0x00BB (return 0xAF)
    0x2591 (return 0xB0)
    0x2592 (return 0xB1)
    0x2593 (return 0xB2)
    0x2502 (return 0xB3)
    0x2524 (return 0xB4)
    0x2561 (return 0xB5)
    0x2562 (return 0xB6)
    0x2556 (return 0xB7)
    0x2555 (return 0xB8)
    0x2563 (return 0xB9)
    0x2551 (return 0xBA)
    0x2557 (return 0xBB)
    0x255D (return 0xBC)
    0x255C (return 0xBD)
    0x255B (return 0xBE)
    0x2510 (return 0xBF)
    0x2514 (return 0xC0)
    0x2534 (return 0xC1)
    0x252C (return 0xC2)
    0x251C (return 0xC3)
    0x2500 (return 0xC4)
    0x253C (return 0xC5)
    0x255E (return 0xC6)
    0x255F (return 0xC7)
    0x255A (return 0xC8)
    0x2554 (return 0xC9)
    0x2569 (return 0xCA)
    0x2566 (return 0xCB)
    0x2560 (return 0xCC)
    0x2550 (return 0xCD)
    0x256C (return 0xCE)
    0x2567 (return 0xCF)
    0x2568 (return 0xD0)
    0x2564 (return 0xD1)
    0x2565 (return 0xD2)
    0x2559 (return 0xD3)
    0x2558 (return 0xD4)
    0x2552 (return 0xD5)
    0x2553 (return 0xD6)
    0x256B (return 0xD7)
    0x256A (return 0xD8)
    0x2518 (return 0xD9)
    0x250C (return 0xDA)
    0x2588 (return 0xDB)
    0x2584 (return 0xDC)
    0x258C (return 0xDD)
    0x2590 (return 0xDE)
    0x2580 (return 0xDF)
    0x03B1 (return 0xE0)
    0x00DF (return 0xE1)
    0x0393 (return 0xE2)
    0x03C0 (return 0xE3)
    0x03A3 (return 0xE4)
    0x03C3 (return 0xE5)
    0x00B5 (return 0xE6)
    0x03C4 (return 0xE7)
    0x03A6 (return 0xE8)
    0x0398 (return 0xE9)
    0x03A9 (return 0xEA)
    0x03B4 (return 0xEB)
    0x221E (return 0xEC)
    0x03C6 (return 0xED)
    0x03B5 (return 0xEE)
    0x2229 (return 0xEF)
    0x2261 (return 0xF0)
    0x00B1 (return 0xF1)
    0x2265 (return 0xF2)
    0x2264 (return 0xF3)
    0x2320 (return 0xF4)
    0x2321 (return 0xF5)
    0x00F7 (return 0xF6)
    0x2248 (return 0xF7)
    0x00B0 (return 0xF8)
    0x2219 (return 0xF9)
    0x00B7 (return 0xFA)
    0x221A (return 0xFB)
    0x207F (return 0xFC)
    0x00B2 (return 0xFD)
    0x25A0 (return 0xFE)
    0x00A0 (return 0xFF)
    (return 0x00)))

(cfunction draw-simple-text
  "Draw text with a default, bitmap on an image"
  [img:JanetTuple x:int y:int xscale:int yscale:int text:cstring color:uint32_t &opt (font-name keyword (janet-ckeyword "default"))] -> JanetTuple
  ,;(bind-image-code 'img)
  (if (< xscale 1) (janet-panic "xscale must be at least 1"))
  (if (< yscale 1) (janet-panic "yscale must be at least 1"))
  (def (font (const 'BitmapFont)) (select-font font-name))
  # Hardcoded glyph widths for the built-in font.
  (def gw:int font->gw)
  (def gh:int font->gh)
  (def bytes-per-row:int (/ (+ 7 gw) 8))
  (def bytes-per-char:int (* bytes-per-row gh))
  (var xx:int x)
  (var yy:int y)
  (var (c (const 'uint8_t)) (cast (const 'uint8_t) text))
  (while 'c
    (def codepoint:int (utf8-read-codepoint ;c))
    (if (= codepoint ,(chr "\n")) (do (set yy (+ yy (* yscale gh))) (set xx x) (continue)))
    (def cp437:int (unicode-to-cp437 codepoint))
    (for [(var row:int 0) (< row gh) (++ row)]
      (def glyph-row:unsigned 0)
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
              (when (and (>= xxx 0) (>= yyy 0) (< xxx width) (< yyy height))
                (def c1:uint32_t (shader xxx yyy color))
                (set-pixel xxx yyy c1)))))
        (set glyph-row (>> glyph-row 1))))
    (set xx (+ xx (* xscale gw))))
  (return img))

###
### Raster path fill
###

(function cross2 :static :inline
  "2d cross product"
  [ax:int ay:int bx:int by:int] -> int
  (return (- (* ax by) (* ay bx))))

(cfunction seg-seg-intersect :static :inline
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
  (def a:int (cross2 s0r1x s0r1y s0s1x s0s1y))
  (def b:int (cross2 s0r0x s0r0y s0s1x s0s1y))
  (def c:int (cross2 r0s0x r0s0y r0r1x r0r1y))
  (def d:int (cross2 r0s1x r0s1y r0r1x r0r1y))
  # Checks
  (return
    (and
      (not= (< a 0) (> 0 b))
      (not= (< c 0) (> 0 d)))))

(cfunction fill-path
  "Fill a path with a solid color"
  [img:JanetTuple points:indexed color:uint32_t] -> JanetTuple
  ,;(bind-image-code 'img)
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
    (def x:int (janet-getinteger points.items i))
    (def y:int (janet-getinteger points.items (+ i 1)))
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
  (clip 0 (- width 1) 0 (- height 1) ;xmin ;ymin)
  (clip 0 (- width 1) 0 (- height 1) ;xmax ;ymax)
  # 3. Fill the bounds of the path, running a ray crossing test for each pixel and color when we have an odd number of intersections
  (polymorph channels [1 2 3 4]
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
          (def c1:uint32_t (shader x y color))
          (set-pixel x y c1)))))
  # 4. Cleanup
  (janet-sfree ipoints)
  (return img))

###
### Better, Faster fill path
###

(function scanline-test
  "Check if a line segment intesects a scanline. If so, return the x coord of the intersection as well"
  [x1:int y1:int x2:int y2:int y-scan:int (xout 'int)] -> int
  (if (< y2 y1) (return (scanline-test x2 y2 x1 y1 y-scan xout)))
  (if (< y-scan y1) (return 0))
  (if (>= y-scan y2) (return 0))
  (if (= y1 y2) (return 0))
  (def dy:int (- y2 y1))
  (assert (> dy 0))
  (def dx:int (- x2 x1))
  (def py:int (- y2 y-scan))
  (def ny:int (- y-scan y1))
  (def px:int (/ (* py dx) dy))
  (def nx:int (/ (* ny dx) dy))
  (set 'xout (/ (+ (- x2 px) (+ x1 nx)) 2))
  (return 1))

(cfunction fill-path-2
  "Fill a path with a solid color"
  [img:JanetTuple points:indexed color:uint32_t] -> JanetTuple
  ,;(bind-image-code 'img)
  # 1. Get y bounds of the path and extract coordinates
  (var ymin:int INT32-MAX)
  (var ymax:int INT32-MIN)
  (if (band 1 points.len) (janet-panic "expected an even number of point coordinates"))
  (if (< points.len 6) (janet-panic "expected at least 3 points"))
  (def plen:int (+ 2 points.len))
  (def (ipoints 'int) (janet-smalloc (* (sizeof int) (* 2 plen))))
  (def (ibuf 'int) (+ ipoints plen)) # Use this buffer for sorting x coordinates for scan lines
  (for [(var i:int 0) (< i points.len) (set i (+ i 2))]
    (def x:int (janet-getinteger points.items i))
    (def y:int (janet-getinteger points.items (+ i 1)))
    (set (aref ipoints i) x)
    (set (aref ipoints (+ 1 i)) y)
    (set ymin (? (> y ymin) ymin y))
    (set ymax (? (> y ymax) y ymax)))
  # Add first point to end
  (set (aref ipoints points.len) (aref ipoints 0))
  (set (aref ipoints (+ 1 points.len)) (aref ipoints 1))
  # 2. Y Clipping
  (set ymin (clampz ymin 0 (- height 1)))
  (set ymax (clampz ymax 0 (- height 1)))
  # 3. Iterate over scanlines
  (polymorph channels [1 2 3 4]
    (for [(var y:int ymin) (<= y ymax) (set y (+ y 1))]
      # Collect and sort x intercepts for all segments into ibuf
      (var intersection-count:int 0)
      (for [(var i:int 2) (< i plen) (set i (+ i 2))]
        (var xcoord:int 0)
        (def intersect:int
          (scanline-test 
            (aref ipoints (+ i 0))
            (aref ipoints (+ i 1))
            (aref ipoints (+ i -2))
            (aref ipoints (+ i -1))
            y ;xcoord))
        (when intersect
          (for [(var j:int 0) (< j intersection-count) (++ j)]
            (if (< xcoord (aref ibuf j))
              (swap xcoord (aref ibuf j) int)))
          (set (aref ibuf intersection-count) xcoord)
          (++ intersection-count)))
      # Draw scan lines
      (for [(var i:int 0) (< i intersection-count) (set i (+ i 2))]
        (def x1:int (clampz (aref ibuf i) 0 (- width 1)))
        (def x2:int (clampz (aref ibuf (+ i 1)) 0 (- width 1)))
        (for [(var x:int x1) (<= x x2) (++ x)]
          (def c1:uint32_t (shader x y color))
          (set-pixel x y c1)))))
  # 4. Cleanup
  (janet-sfree ipoints)
  (return img))

(module-entry "spork_gfx2d")
