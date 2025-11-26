###
### Use cjanet JIT to use libpng
###

(use ../spork/cjanet)

(begin-jit
  :module-name "pngexample"
  :cflags ["-Werror"]
  :lflags ["-lpng" "-lz"]
  :quiet true
  :build-type :release)

(include <stdio.h>)
(include <stdlib.h>)
(include <png.h>)

(cfunction
  make-png
  "Write a PNG to a file `out` with a specified width and height"
  [out:cstring width:int height:int] -> int
  (def bit-depth:int 32)
  (def byte-depth:int (/ bit-depth 8))

  # Generate pixel data
  (def data:png_bytep (malloc (* byte-depth width height)))
  (def rows:png_bytepp (malloc (* height (sizeof png_bytep))))

  (for [(def i:int 0) (< i height) (++ i)]
    (set (aref rows i) (+ data (* i width byte-depth))))

  # Paint it red
  (for [(def y:int 0) (< y height) (++ y)]
    (for [(def x:int 0) (< x width) (++ x)]
      (set (aref (aref rows y) (+ (* 4 x) 0)) 0xFF)
      (set (aref (aref rows y) (+ (* 4 x) 1)) 0)
      (set (aref (aref rows y) (+ (* 4 x) 2)) 0)
      (set (aref (aref rows y) (+ (* 4 x) 3)) 0xFF)))

  # Now write out PNG
  (def (fp (* FILE)) (fopen out "wb"))
  (if (not fp)
    (do
      (perror "error opening file for writing")
      (exit 1)))

  (def png:png_structp (png-create-write-struct PNG_LIBPNG_VER_STRING NULL NULL NULL))
  (def info:png_infop (png-create-info-struct png))

  (if (setjmp (png-jmpbuf png))
    (do
      (fprintf stderr "failed writing png")
      (exit 1)))

  (png-init-io png fp)
  (png-set-IHDR
    png info width height
    8 PNG_COLOR_TYPE_RGBA PNG_INTERLACE_NONE
    PNG_COMPRESSION_TYPE_DEFAULT PNG_FILTER_TYPE_DEFAULT)
  (png-write-info png info)
  (png-write-image png rows)
  (png-write-end png NULL)

  # Clean up
  (free rows)
  (free data)
  (fclose fp)

  (return 0))

(end-jit) # creates and loads shared object

# Now use it
(os/mkdir "tmp")
(make-png "tmp/out.png" 100 100)

# janet examples/cjanet-libpng.janet && feh tmp/out.png
