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
### [ ] - image cropping
### [ ] - testing harness
### [ ] - text w/ simple font
### [ ] - ellipses
### [ ] - arcs
### [ ] - lines/paths
### [ ] - blending
### [ ] - image resizing
### [ ] - bezier
### [ ] - flood fill
### [ ] - triangle strip/fan/list
### [ ] - LUTs (possibly w/ shaders)

### Stretch TODO
### [ ] - anti-aliasing w/ MXAA
### [ ] - shaders using cjanet-jit
### [ ] - sub-images for rendering / alternatives to JanetBuffer for data storage
### [ ] - multithreading

(use ../spork/cjanet)

(include <janet.h>)
(include <string.h>)
(include <math.h>)

(@ define STB_IMAGE_IMPLEMENTATION)
(@ define STB_IMAGE_WRITE_IMPLEMENTATION)
(@ define STBIW_WINDOWS_UTF8)

(include "stb_image.h")
(include "stb_image_write.h")

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
  (def (img (* char)) (stbi-load path (addr width) (addr height) (addr c) 0))
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
     (unwrap-image img (addr buf) (addr width) (addr height) (addr channels))
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

# TODO - remove for clipping
(function check-bound
  [coord:int imgbound:int (coord-name (* char))] -> void
  (if (or (< coord 0) (>= coord imgbound))
    (janet-panicf "coordinate %s is out of bounds, got %d, expected in range [0, %d)" coord-name coord imgbound)))

###
### C Functions
###

(cfunction rect
  "Draw a rectangle on an image"
  [img:tuple x1:int y1:int x2:int y2:int color:uint32] -> tuple
  ,;(bind-image-code 'img)
  (check-bound x1 width "x1")
  (check-bound x2 width "x2")
  (check-bound y1 height "y1")
  (check-bound y2 height "y2")
  (for [(def y:int y1) (<= y y2) (++ y)]
    (for [(def x:int x1) (<= x x2) (++ x)]
      (set-pixel x y color)))
  (return img))

(cfunction circle
  "Draw a circle"
  [img:tuple x:int y:int r:double color:uint32] -> tuple
  ,;(bind-image-code 'img)
  (check-bound x width "x")
  (check-bound y height "y")
  (def fr:int (floor (- r)))
  (def cr:int (ceil r))
  (for [(def yy:int fr) (<= yy cr) (++ yy)]
    (for [(def xx:int fr) (<= xx cr) (++ xx)]
      (if (>= (* r r) (+ (* xx xx) (* yy yy)))
        (set-pixel (+ x xx) (+ y yy) color))))
  (return img))

(cfunction stamp
  "Copy one image onto another"
  [dest:tuple src:tuple dx:int dy:int] -> tuple
  ,;(bind-image-code 'src "src-")
  ,;(bind-image-code 'dest "dest-")
  (if (not= src-channels dest-channels) (janet-panic "image channels don't match"))
  (if (== src-data dest-data) (janet-panic "cannot stamp self"))
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

(cfunction line
  "Draw a line from x1,y1 to x2,y2"
  [img:tuple x1:int y1:int x2:int y2:int color:uint32] -> tuple
  ,;(bind-image-code 'img "img-")
  # TODO - add clipping
  # Use Bresenham's algorithm to draw the line
  (def dx:int (- x2 x1))
  (def dy:int (- y2 y1))
  (def errory:int 0)
  (def y:int y1)
  (for [(def x:int x1) (<= x x2) (++ x)]
    (set-pixel x y color "img-")
    (set errory (+ errory dy))
    (if (>= errory 0)
      (do
        (++ y)
        (set errory (- errory dx)))))
  (return img))

(module-entry "spork_gfx2d")
