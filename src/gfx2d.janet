###
### gfx2d.janet
###
### Various simple CPU-based 2d graphics tools suitable for demos, visulizations, and charting.
###
### Includes:
### * Wrappers around stb_image for loading, saving, and modifying images.
###

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

# Generate loaders for each image type
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
   :green   0x00FF00FF
   :blue    0x0000FFFF
   :clear   0x00000000
   :black   0x000000FF
   :white   0xFFFFFFFF
   :cyan    0x00FFFFFF
   :yellow  0xFFFF00FF
   :magenta 0xFF00FFFF})

(eachp [name value] colors
  (emit-cdef (symbol name) (string "color constant for " name) ~(janet-wrap-number ,value)))

##
## drawing functions
##

# TODO - clipping?

(function check-bound
  [coord:int imgbound:int (coord-name (* char))] -> void
  (if (or (< coord 0) (>= coord imgbound))
    (janet-panicf "coordinate %s is out of bounds, got %d, expected in range [0, %d)" coord-name coord imgbound)))

(defn- common-draw-prefix
  "Function body prefix for drawing functions that is boilerplate.
  Unpacks arguments and does some checks, and creates variables for things
  like height, width, and pixel data."
  []
  ~[(def width:int 0)
    (def height:int 0)
    (def channels:int 0)
    (def (buf (* JanetBuffer)) NULL)
    (unwrap-image img (addr buf) (addr width) (addr height) (addr channels))
    (def (data (* char)) buf->data)
    (def stride:int (* channels width))])

(defn- put-color
  "code gen for writing colors"
  [x y color]
  ~(for [(def c:int 0) (< c channels) (++ c)]
    (set (aref data (+ c (* ,x channels) (* ,y stride)))
        (band (>> ,color (- 24 (* c 8))) 0xFF))))

(cfunction rect
  "Draw a rectangle on an image"
  [img:tuple x1:int y1:int x2:int y2:int color:uint32] -> tuple
  ,;(common-draw-prefix)
  (check-bound x1 width "x1")
  (check-bound x2 width "x2")
  (check-bound y1 height "y1")
  (check-bound y2 height "y2")
  (for [(def y:int y1) (<= y y2) (++ y)]
    (for [(def x:int x1) (<= x x2) (++ x)]
      ,(put-color 'x 'y 'color)))
  (return img))

(cfunction circle
  "Draw a circle"
  [img:tuple x:int y:int r:double color:uint32] -> tuple
  ,;(common-draw-prefix)
  (check-bound x width "x")
  (check-bound y height "y")
  (def fr:int (floor (- r)))
  (def cr:int (ceil r))
  (for [(def yy:int fr) (<= yy cr) (++ yy)]
    (for [(def xx:int fr) (<= xx cr) (++ xx)]
      (if (>= (* r r) (+ (* xx xx) (* yy yy)))
        ,(put-color '(+ x xx) '(+ y yy) 'color))))
  (return img))

(module-entry "spork_gfx2d")
