###
### Use cjanet JIT and libpng to generate a large fractal.
###

(use ../spork/cjanet)

# Define the pixel shader up here - takes x and y
(def madelbrot
  '(do
     (def iterations:int 40)
     (def xfloat:double (+ -2.00 (/ (cast double x) width 0.4)))
     (def yfloat:double (+ -1.25 (/ (cast double y) height 0.4)))
     (def c:complex (+ xfloat (* I yfloat)))
     (var X:complex 0.0)
     (for [(def i:int 0) (< i iterations) (++ i)]
       (set X (+ (* X X) c)))
     (def mag2:double (pow (cabs X) 0.01))
     (cond
       #(> 0.9 mag2) (set color 0x000000FF)
       (> 1.0 mag2) (set color 0x000000FF)
       (> 2 mag2) (set color 0x330700FF)
       (> 5 mag2) (set color 0x771100FF)
       (> 15 mag2) (set color 0xbb5500FF)
       (> 100 mag2) (set color 0xee8800FF)
       (> 1000 mag2) (set color 0xFFFF00FF)
       (> 10000 mag2) (set color 0xFFFF33FF)
       (> 100000 mag2) (set color 0xFFFF66FF)
       (> 1000000 mag2) (set color 0xFFFF99FF)
       (set color 0xFFFFFFFF))))

(begin-jit
  :module-name "pngexample"
  :cflags ["-Werror"]
  :lflags ["-lpng" "-lz"]
  :cache true
  #:smart-libs false
  #:cc "tcc" - doesn't quite work on linux due to executable stack issue
  #:verbose true
  #:quiet true
  :build-type :release)

(include <stdio.h>)
(include <stdlib.h>)
(include <png.h>)
(include <complex.h>)
(include <math.h>)

(cfunction
  make-png
  "Write a PNG to a file `out` with a specified width and height"
  [out:cstring width:int height:int] -> int
  (def bit-depth:int 32)
  (def byte-depth:int (/ bit-depth 8))

  # Allocate pixel data
  (printf "generating %s\n" out)
  (def data:png-bytep (malloc (* byte-depth width height)))
  (def rows:png-bytepp (malloc (* height (sizeof png-bytep))))
  (for [(def i:int 0) (< i height) (++ i)]
    (set (aref rows i) (+ data (* i width byte-depth))))

  # Paint it red
  (for [(def y:int 0) (< y height) (++ y)]
    (for [(def x:int 0) (< x width) (++ x)]
      (var color:uint32_t 0)
      ,madelbrot # inline "pixel shader" program
      (set (aref (aref rows y) (+ (* 4 x) 0)) (band 0xFF (>> color 24)))
      (set (aref (aref rows y) (+ (* 4 x) 1)) (band 0xFF (>> color 16)))
      (set (aref (aref rows y) (+ (* 4 x) 2)) (band 0xFF (>> color 8)))
      (set (aref (aref rows y) (+ (* 4 x) 3)) (band 0xFF color))))

  # Now write out PNG
  (printf "writing %s\n" out)
  (def (fp (* FILE)) (fopen out "wb"))
  (unless fp
    (perror "error opening file for writing")
    (exit 1))

  (def png:png-structp (png-create-write-struct PNG-LIBPNG-VER-STRING NULL NULL NULL))
  (def info:png-infop (png-create-info-struct png))

  (when (setjmp (png-jmpbuf png))
    (fprintf stderr "failed writing png")
    (exit 1))

  (png-init-io png fp)
  (png-set-IHDR
    png info width height
    8 PNG-COLOR-TYPE-RGBA PNG-INTERLACE-NONE
    PNG-COMPRESSION-TYPE-DEFAULT PNG-FILTER-TYPE-DEFAULT)
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
(make-png "tmp/out.png" 4096 4096)

# janet examples/cjanet-libpng.janet && feh tmp/out.png
