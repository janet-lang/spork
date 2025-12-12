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
### [ ] - text w/ simple font
### [ ] - lines/paths (w/ thickness)
### [ ] - blending
### [ ] - image resizing
### [ ] - bezier
### [ ] - flood fill
### [ ] - triangle strip/fan/list
### [ ] - LUTs (possibly w/ shaders)
### [ ] - Gradients (possibly w/ shaders)
### [ ] - Better 2D point abstraction

### Stretch TODO
### [ ] - anti-aliasing w/ MXAA
### [ ] - shaders using cjanet-jit
### [ ] - sub-images for rendering / alternatives to JanetBuffer for data storage
### [ ] - multithreading

(use ../spork/cjanet)

(include <janet.h>)
(include <string.h>)
(include <math.h>)

(@ define STBIW_WINDOWS_UTF8)

(include "stb_image.h")
(include "stb_image_write.h")
(include "stb_image_resize2.h")

###
### Image creation, basic utilties, saving and loading
###

(function wrap-image
   "wrap an image"
   [(buf (* JanetBuffer)) width:int height:int channels:int] -> JanetTuple
   (def (tup (* Janet)) (janet-tuple-begin 4))
   (set (aref tup 0) (janet-wrap-buffer buf))
   (set (aref tup 1) (janet-wrap-integer width))
   (set (aref tup 2) (janet-wrap-integer height))
   (set (aref tup 3) (janet-wrap-integer channels))
   (return (janet-tuple-end tup)))

(function unwrap-image
   "unwrap an image"
   [img:JanetTuple (data (** JanetBuffer)) (width (* int)) (height (* int)) (channels (* int))] -> void
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

  (def (buf (* JanetBuffer)) (janet-buffer (* width height channel)))
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
  (def (img (* char)) (stbi-load path ;width ;height ;c 0))
  (unless img (janet-panic "failed to load image"))

  # Copy into buffer
  (def (buf (* JanetBuffer)) (janet-buffer (* width height c)))
  (memcpy buf->data img (* width height c))
  (set buf->count (* width height c))
  (stbi-image-free img) # TODO - remove this alloc, copy, free?

  # Package buffer and dimensions in tuple
  (return (wrap-image buf width height c)))

# Generate image-writing for each image type
# TODO - hdr
(each ft ["bmp" "tga" "png" "jpg"]
  (def extra-args
    (case ft
      "png" ['(* width channels)]
      "jpg" [100] # - jpeg quality
      []))
  (cfunction ,(symbol 'save- ft)
     ,(string "Save an image to a file as a " ft)
     [path:cstring img:tuple] -> tuple
     (def width:int 0)
     (def height:int 0)
     (def channels:int 0)
     (def (buf (* JanetBuffer)) NULL)
     (unwrap-image img ;buf ;width ;height ;channels)
     (def check:int (,(symbol 'stbi-write- ft) path width height channels buf->data ,;extra-args))
     (if-not check (janet-panic "failed to write image"))
     (return img)))

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
  (emit-cdef (symbol name) (string "color constant for " name) ~(janet-wrap-number ,value)))

##
## DRAWING
##

(defn- bind-image-code
  :cjanet-spliced-macro
  "Generate code to unpack an image."
  [img &opt sym-prefix]
  (default sym-prefix "")
  ~[(def ,(symbol sym-prefix 'width:int) 0)
    (def ,(symbol sym-prefix 'height:int) 0)
    (def ,(symbol sym-prefix 'channels:int) 0)
    (def (,(symbol sym-prefix 'buf) (* JanetBuffer)) NULL)
    (unwrap-image
      ,img
      (addr ,(symbol sym-prefix 'buf))
      (addr ,(symbol sym-prefix 'width))
      (addr ,(symbol sym-prefix 'height))
      (addr ,(symbol sym-prefix 'channels)))
    (def (,(symbol sym-prefix 'data) (* char)) ,(symbol sym-prefix 'buf->data))
    (def ,(symbol sym-prefix 'stride:int) (* ,(symbol sym-prefix 'channels) ,(symbol sym-prefix 'width)))])

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

(defn- case?
  :cjanet-expression-macro
  "Make a case statement with the ternary operator"
  [& cases]
  (assert (odd? (length cases)))
  (def case-value (take 2 cases))
  (def [case value] case-value)
  (if (> 2 (length case-value))
    case
    ~(? ,case ,value ,(case? ;(drop 2 cases)))))

###
### Math helpers
###

(defn- sort2
  :cjanet-block-macro
  "sort 2 variables such that x is less the y. If not, swap them."
  [x y typ]
  ~(when (< ,y ,x)
     (def (tmp ,typ) ,x)
     (set ,x ,y)
     (set ,y tmp)))

(function lerp [a:float b:float t:float] -> float (return (+ (* (- 1 t) a) (* t b))))
(function unlerp [x:float x0:float x1:float] -> float (return (/ (- x x0) (- x1 x0)))) # is this usefule (divide by zero)?
(function clamp [x:float min_x:float max_x:float] -> float
          (if (< x min_x) (return min_x))
          (if (> x max_x) (return max_x))
          (return x))

(function clip
  [xmin:int xmax:int ymin:int ymax:int
   (x (* int)) (y (* int))] -> void
  (set 'x (clamp 'x xmin xmax))
  (set 'y (clamp 'y ymin ymax)))

(function max3z [a:int b:int c:int] -> int
  (return (? (> a b)
     (? (> a c) a c)
     (? (> b c) b c))))

(function min3z [a:int b:int c:int] -> int
  (return (? (< a b)
     (? (< a c) a c)
     (? (< b c) b c))))

(function barycentric
  "calculate barycentric coordinates in 2d"
  [px:int py:int x1:int y1:int x2:int y2:int x3:int y3:int (t0 (* float)) (t1 (* float)) (t2 (* float))] -> int
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
### C Functions
###

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

(cfunction rect
  "Draw a rectangle on an image"
  [img:tuple x1:int y1:int x2:int y2:int color:uint32] -> tuple
  ,;(bind-image-code 'img)
  (clip 0 (- width 1) 0 (- height 1) ;x1 ;y1)
  (clip 0 (- width 1) 0 (- height 1) ;x2 ;y2)
  (for [(def y:int y1) (<= y y2) (++ y)]
    (for [(def x:int x1) (<= x x2) (++ x)]
      (set-pixel x y color)))
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
  (for [(def yy:int y1) (<= yy y2) (++ yy)]
    (for [(def xx:int x1) (<= xx x2) (++ xx)]
      (if (>= (* r r) (+ (* xx xx) (* yy yy)))
        (set-pixel (+ x xx) (+ y yy) color))))
  (return img))

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
  (for [(var y:int ymin) (< y ymax) (++ y)]
    (for [(var x:int xmin) (< x xmax) (++ x)]
      (var color:uint32_t 0)
      (get-pixel color x y "src-")
      (set-pixel (+ dx x) (+ dy y) color "dest-")))
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
  (for [(var y:int 0) (< y a-height) (++ y)]
    (for [(var x:int 0) (< x a-width) (++ x)]
      (var color:uint32_t 0)
      (for [(var c:int (- a-channels 1)) (>= c 0) (-- c)]
        (def ac:int (aref a-data (+ c (* x a-channels) (* y a-stride))))
        (def bc:int (aref b-data (+ c (* x a-channels) (* y a-stride))))
        (def cc:int (- a b)) # TODO - reuse this
        (if (> 0x00 cc) (set cc 0x00))
        (if (< 0xFF cc) (set cc 0xFF))
        (set color (+ (cast uint32_t cc) (<< color 8))))
      (set-pixel x y color "dest-")))
  (return dest))

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
      (set-pixel x y color "img-"))
    (when (>= (* 2 err) dy)
      (if (== x x2) (return img))
      (+= err dy)
      (+= x sx))
    (when (<= (* 2 err) dx)
      (if (== y y2) (return img))
      (+= err dx)
      (+= y sy))))

(function triangle-impl
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
      (set-pixel x y color)))
  # 3. Use modified line algorithm for bottom half
  (for [(var y:int y2) (< y y3) (++ y)]
    (def yt-23:float (unlerp y y2 y3))
    (def yt-13:float (unlerp y y1 y3))
    (def x-a:int (cast int (floor (lerp x2 x3 yt-23))))
    (def x-b:int (cast int (ceil (lerp x1 x3 yt-13))))
    (sort2 x-a x-b int)
    (for [(var x:int x-a) (<= x x-b) (++ x)]
      (set-pixel x y color)))
  # 4. last row in case y2 == y3
  (def x-a:int x3)
  (def x-b:int (? (= y2 y3) x2 x3))
  (sort2 x-a x-b int)
  (for [(var x:int x-a) (<= x x-b) (++ x)]
    (set-pixel x y3 color))
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
   "Fill a triangle 2"
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
         (def color1:uint32_t (? (band 1 (+ (>> x 2) (>> y 2))) color 0))
         (set-pixel x y color1))))
   (return img))

(cfunction resize
  "Resize an image, resampling as needed"
  [input-img:JanetTuple new-width:int new-height:int] -> JanetTuple
  ,;(bind-image-code 'input-img 'in-)
  (def new-img:JanetTuple (blank new-width new-height in-channels))
  ,;(bind-image-code 'new-img 'out-)
  (def layout:stbir_pixel_layout
    (case?
      (= in-channels 1) STBIR-1CHANNEL
      (= in-channels 2) STBIR-2CHANNEL
      (= in-channels 3) STBIR-RGB
      STBIR-4CHANNEL))
  (stbir-resize-uint8-srgb in-data in-width in-height in-stride
                           out-data out-width out-height out-stride
                           layout)
  (return new-img))

(module-entry "spork_gfx2d")
