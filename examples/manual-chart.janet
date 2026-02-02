###
### Draw a chart but only use the provided axes - draw contents yourself 
###

(import spork/gfx2d)
(import spork/charts)
(import spork/test)

# Set default styling to dark!
(charts/dark-mode)

# Get axes
(def canvas
  (test/timeit
    # how long to render the bitmap (not including interpreter startup and saving image)?
    (do
      (def canvas (gfx2d/blank 1024 1024))
      (gfx2d/fill-rect canvas 0 0 1024 1024 gfx2d/black)
      (def [view convert]
        (charts/draw-axes
          canvas
          :padding 4
          :format-y |(string/format "$%.2f" $)
          :x-label "Units"
          :y-label "Dollars"
          :x-min 0
          :x-max 100
          :y-min 0
          :y-max 100))

      # Now draw. Add whatever you need here, it will be clipped inside the axis
      (loop [i :range [0 1000 5]] # plot 200 lines for a cool pattern
        (gfx2d/plot view ;(convert -30 -10) ;(convert 110 i) gfx2d/red))
      (gfx2d/draw-simple-text view ;(convert 2 99) "Hello from up here" gfx2d/cyan)
      (gfx2d/draw-simple-text view ;(convert 50 2) "Hello from down here" gfx2d/cyan)

      # Lets add a legend in the top right corner
      (def legend-args [:labels ["Thing 1" "Thing 2"] :frame true :padding 4])
      (def [lw lh] (charts/draw-legend nil ;legend-args))
      (def {:width vw :height vh} (gfx2d/unpack view))
      (def legend-view (gfx2d/viewport view (- vw lw 10) 10 lw lh true))
      (charts/draw-legend legend-view ;legend-args)
      canvas)))

# Save it
(os/mkdir "tmp")
(gfx2d/save "tmp/out.bmp" canvas)
(print "Wrote image to tmp/out.bmp")
