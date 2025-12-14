(use spork/cjanet)

###
### Generate bitmaps for the built-in retro console fonts encoded in TTFs.
### We seek to minimize memory usage for these fonts. Technically, this should be
### similar to how these fonts were stored in BIOS roms but it is much easier to find
### TTF extractions/reproductions of these fonts and build an internal representation
### ourselves.
###

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

(cfunction load-font
  "Load a font from disk and create compressed font"
  [header-dest:cstring cname:cstring path:cstring gw:int gh:int] -> int

  (def fnt:stbtt-fontinfo)
  (def font-buffer (malloc (<< 1 24)))
  (def f (fopen path "rb"))
  (unless f
    (janet-panic "no font file found"))
  (assert (fread font-buffer 1 (<< 1 24) f))
  (fclose f)

  # Raster pixel data from stb truetype
  (def (img-data (* char)) (malloc (* gw gh 256)))
  (memset img-data 0 (* gw gh 256))

  # Extract CP 437 fonts into a bitmap
  (if (not (stbtt-InitFont ;fnt font-buffer 0)) (janet-panic "bad font"))
  (def scale:float (stbtt-ScaleForPixelHeight ;fnt gh))
  (printf "scale: %f\n" scale)

  # Iterate glyphs to find the overall offset for characters
  (var x0min:int 10000)
  (var y0min:int 10000)
  (var x1max:int -10000)
  (var y1max:int -10000)
  (for [(var glyph:int 1) (<= glyph 0x100) (++ glyph)]
    (var x0:int 0)
    (var y0:int 0)
    (var x1:int 0)
    (var y1:int 0)
    (stbtt-GetGlyphBitmapBox ;fnt glyph scale scale ;x0 ;y0 ;x1 ;y1)
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
  (for [(var glyph:int 1) (<= glyph 0x100) (++ glyph)]
    (var x0:int 0)
    (var y0:int 0)
    (var x1:int 0)
    (var y1:int 0)
    (stbtt-GetGlyphBitmapBox ;fnt glyph scale scale ;x0 ;y0 ;x1 ;y1)
    (def pw:int (* 16 gw))
    (def x:int (+ x0 xoff (* gw (% (- glyph 1) 16))))
    (def y:int (+ y0 yoff (* gh (/ (- glyph 1) 16))))
    # (printf "rendering at (x, y) = (%d, %d)\n" x y)
    (stbtt-MakeGlyphBitmap ;fnt (+ img-data x (* pw y)) gw gh pw scale scale glyph))

  # Clean up font data
  (free font-buffer)

  # Write image for debugging
  (def pngname (malloc 1000))
  (memset pngname 0 1000)
  (strcat pngname header-dest)
  (strcat pngname ".png")
  (stbi-write-png pngname (* 16 gw) (* 16 gh) 1 img-data (* 16 gw))
  (free pngname)

  # Now extract pixels into 1 bit per pixel buffer.
  # Print the buffer as a C array
  (def fp (fopen header-dest "wb"))
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
