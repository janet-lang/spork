(use spork/cjanet)

###
### Generate bitmaps for the built-in retro console fonts encoded in TTFs.
### We seek to minimize memory usage for these fonts. Technically, this should be
### similar to how these fonts were stored in BIOS roms but it is much easier to find
### TTF extractions/reproductions of these fonts and build an internal representation
### ourselves.
###

(def cp437-to-unicode
  @[32 9786 9787 9829 9830 9827 9824 8226 9688 9675 9689 9794 9792 9834 9835 9788 9658 9668 8597 8252 20 21 22 23 24 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59 60 61 62 63 64 65 66 67 68 69 70 71 72 73 74 75 76 77 78 79 80 81 82 83 84 85 86 87 88 89 90 91 92 93 94 95 96 97 98 99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120 121 122 123 124 125 126 8962 199 252 233 226 228 224 229 231 234 235 232 239 238 236 196 197 201 230 198 244 246 242 251 249 255 214 220 162 163 165 8359 402 225 237 243 250 241 209 170 186 191 8976 172 189 188 161 171 187 9617 9618 9619 9474 9508 9569 9570 9558 9557 9571 9553 9559 9565 9564 9563 9488 9492 9524 9516 9500 9472 9532 9566 9567 9562 9556 9577 9574 9568 9552 9580 9575 9576 9572 9573 9561 9560 9554 9555 9579 9578 9496 9484 9608 9604 9612 9616 9600 945 223 915 960 931 963 181 964 934 920 937 948 8734 966 949 8745 8801 177 8805 8804 8992 8993 247 8776 176 8729 183 8730 8319 178 9632])

(eprint (length cp437-to-unicode))

(begin-jit
  :module-name "make-font"
  :cflags @["-Ideps/stb" "-Ideps"])

(include <janet.h>)
(include <string.h>)
(include <math.h>)
(include <assert.h>)

(@ define STB_IMAGE_WRITE_IMPLEMENTATION)
(@ define STB_TRUETYPE_IMPLEMENTATION)
(@ define STBIW_WINDOWS_UTF8)

(include "stb_truetype.h")
(include "stb_image_write.h")
(include <string.h>)

(declare (cp437-lookup (array int)) :static :const (array ,;cp437-to-unicode))

(cfunction load-font
  "Load a font from disk and create compressed font"
  [header-dest:cstring cname:cstring path:cstring gw:int gh:int] -> int

  (def fnt:stbtt-fontinfo)
  (def font-buffer:*char (malloc (<< 1 24)))
  (def f:*FILE (fopen path "rb"))
  (unless f
    (janet-panic "no font file found"))
  (assert (fread font-buffer 1 (<< 1 24) f))
  (fclose f)

  # Raster pixel data from stb truetype
  (def img-data:*char (malloc (* gw gh 256 4)))
  (memset img-data 0 (* gw gh 256 4))

  # Extract CP 437 fonts into a bitmap
  (if (not (stbtt-InitFont &fnt font-buffer 0)) (janet-panic "bad font"))
  (def scale:float (stbtt-ScaleForPixelHeight &fnt gh))
  (printf "scale: %f\n" scale)

  # Iterate glyphs to find the overall offset for characters
  (var x0min:int 10000)
  (var y0min:int 10000)
  (var x1max:int -10000)
  (var y1max:int -10000)
  (for [(var cp437:int 0) (< cp437 0x100) (++ cp437)]
    (def unicode:int (aref cp437-lookup cp437))
    (def glyph:int (stbtt-FindGlyphIndex &fnt unicode))
    (if (not glyph) (continue))
    (var x0:int 0)
    (var y0:int 0)
    (var x1:int 0)
    (var y1:int 0)
    (stbtt-GetGlyphBitmapBox &fnt glyph scale scale &x0 &y0 &x1 &y1)
    # (printf "glyph %d (x0, y0, x1, y1) = (%d, %d, %d, %d)\n" glyph x0 y0 x1 y1)
    (set x0min (? (< x0 x0min) x0 x0min))
    (set y0min (? (< y0 y0min) y0 y0min))
    (set x1max (? (> x1 x1max) x1 x1max))
    (set y1max (? (> y1 y1max) y1 y1max)))

  (assert (>= gw (- x1max x0min)))
  (assert (>= gh (- y1max y0min)))
  (assert (< 0 (- x1max x0min)))
  (assert (< 0 (- y1max y0min)))
  (def xoff:int (- x0min))
  (def yoff:int (- y0min))
  (printf "xoff, yoff = %d, %d\n" xoff yoff)

  # Iterate glyphs
  (for [(var cp437:int 0) (< cp437 0x100) (++ cp437)]
    (def unicode:int (aref cp437-lookup cp437))
    (def glyph:int (stbtt-FindGlyphIndex &fnt unicode))
    (if (not glyph) (continue))
    (var x0:int 0)
    (var y0:int 0)
    (var x1:int 0)
    (var y1:int 0)
    (stbtt-GetGlyphBitmapBox &fnt glyph scale scale &x0 &y0 &x1 &y1)
    (def pw:int (* 16 gw))
    (def x:int (+ x0 xoff (* gw (% cp437 16))))
    (def y:int (+ y0 yoff (* gh (/ cp437 16))))
    # (printf "rendering at (x, y) = (%d, %d)\n" x y)
    (stbtt-MakeGlyphBitmap &fnt (+ img-data x (* pw y)) gw gh pw scale scale glyph))

  # Write image for debugging
  (def pngname:*void (malloc 1000))
  (memset pngname 0 1000)
  (strcat pngname header-dest)
  (strcat pngname ".png")
  (stbi-write-png pngname (* 16 gw) (* 16 gh) 1 img-data (* 16 gw))

  # Now extract pixels into 1 bit per pixel buffer.
  # Print the buffer as a C array
  (def fp:*FILE (fopen header-dest "wb"))
  (fprintf fp "unsigned char %s_data[] = {\n" cname)
  (for [(var y:int 0) (< y (* 16 gh)) (set y (+ y gh))]
    (for [(var x:int 0) (< x (* 16 gw)) (set x (+ x gw))]
      (fprintf fp "\n")
      # iterate over 8x8 chunks so each glyph is 64 bits contiguous in memory
      (for [(var yy:int y) (< yy (+ y gh)) (++ yy)]
        (var next-byte:int 0)
        (for [(var xx:int x) (< xx (+ x gw)) (++ xx)]
          (if (aref img-data (+ xx (* 16 gw yy)))
            (set next-byte (+ 1 (<< next-byte 1)))
            (set next-byte (<< next-byte 1))))
        (fprintf fp "0x%.2X, " next-byte))))
  (fprintf fp "\n\n};\n")
  (fprintf fp "const BitmapFont %s = {\n" cname)
  (fprintf fp " .gw = %d,\n" gw)
  (fprintf fp " .gh = %d,\n" gh)
  (fprintf fp " .data = %s_data\n};\n" cname)
  (fclose fp)

  (printf "Made font %s\n" path)

  (return 0))

(end-jit)

### Save the font to a file so we can look at it
(load-font "deps/default_font.h" "default_font" "deps/fonts/Px437_DG_One.ttf" 8 8)
(load-font "deps/tall_font.h" "tall_font" "deps/fonts/Px437_Nix8810_M16.ttf" 8 16)
(load-font "deps/olive_font.h" "olive_font" "deps/fonts/Px437_OlivettiThin_8x16.ttf" 8 16)
