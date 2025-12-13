(use spork/cjanet)

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

(cfunction load-font
  "Load a font from disk and create compressed font"
  [path:cstring gw:int gh:int] -> int

  (def fnt:stbtt-fontinfo)
  (def font-buffer (malloc (<< 1 23)))
  (def f (fopen path "rb"))
  (assert (fread font-buffer 1 (<< 1 23) f))
  (fclose f)

  # Raster pixel data from stb truetype
  (def (img-data (* char)) (malloc (* gw gh 256)))
  (memset img-data 0 (* gw gh 256))

  # Extract CP 437 fonts into a bitmap
  (if (not (stbtt-InitFont ;fnt font-buffer 0)) (janet-panic "bad font"))
  (def scale:float (stbtt-ScaleForPixelHeight ;fnt gh))
  (printf "scale: %f\n" scale)

  # Iterate glyphs
  (for [(var glyph:int 1) (<= glyph 0x100) (++ glyph)]
    #(def glyph:int (stbtt-FindGlyphIndex ;fnt codepoint))
    #(if (= 0 glyph) (continue))
    (var lsb:int 0)
    (var advance:int 0)
    (var ascent:int 0)
    (var descent 0)
    (var x0:int 0)
    (var y0:int 0)
    (var x1:int 0)
    (var y1:int 0)
    (stbtt-GetGlyphHMetrics ;fnt glyph ;advance ;lsb)
    (stbtt-GetGlyphBitmapBox ;fnt glyph scale scale ;x0 ;y0 ;x1 ;y1)
    (printf "glyph %d (x0, y0, x1, y1, advance, lsb) = (%d, %d, %d, %d, %d, %d)\n" glyph x0 y0 x1 y1 advance lsb)
    (def pw:int (* 16 gw))
    #(def x:int (* gw (% codepoint 16)))
    #(def y:int (* gh (/ codepoint 16)))
    (def x:int (+ x0 (* gw (% (- glyph 1) 16))))
    (def y:int (+ y0 -1 gh (* gh (/ (- glyph 1) 16))))
    (stbtt-MakeGlyphBitmap ;fnt (+ img-data x (* pw y)) gw gh pw scale scale glyph))
  (free font-buffer)

  # Write image for debugging
  (stbi-write-png "deps/cp437_font.png" (* 16 gw) (* 16 gh) 1 img-data (* 16 gw))

  # Now extract pixels into 1 bit per pixel buffer.
  # Print the buffer as a C array
  (def fp (fopen "deps/cp437_font.h" "wb"))
  (fprintf fp "unsigned char cp437_font_data[] = {\n")
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
  (fclose fp)

  (printf "Made font %s\n" path)

  (return 0))

(end-jit)

### Save the font to a file so we can look at it
(load-font "deps/fonts/Px437_DG_One.ttf" 8 8)
#(load-font "deps/fonts/Px437_Nix8810_M16.ttf" 8 16)
