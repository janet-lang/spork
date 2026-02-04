(import spork/charts)
(import spork/http)
(import spork/json)
(import spork/gfx2d)

###
### GTK Stub to show image
###

(ffi/context "/usr/lib/libgtk-3.so" :lazy true)

(ffi/defbind
  gtk-application-new :ptr
  [title :string flags :uint])

(ffi/defbind
  g-signal-connect-data :ulong
  [a :ptr b :ptr c :ptr d :ptr e :ptr f :int])

(ffi/defbind
  g-application-run :int
  [app :ptr argc :int argv :ptr])

(ffi/defbind
  gtk-application-window-new :ptr
  [a :ptr])

(ffi/defbind
  gtk-container-add :void
  [a :ptr b :ptr])

(ffi/defbind
  gtk-widget-show-all :void
  [a :ptr])

(ffi/defbind
  gtk-image-new-from-pixbuf :ptr
  [buf :ptr])

(ffi/context "/usr/lib/libgdk-3.so" :lazy true)

(ffi/defbind
  gdk-pixbuf-new-from-data :ptr
  [data :ptr
   colorspace :int
   has-alpha :bool
   bits-per-sample :int
   width :int
   height :int
   stride :int
   destroy-dn :ptr
   gpointer :ptr])

(defn show-gtk-image [img]

  (def {:channels c :width w :height h :stride s :data data} (gfx2d/unpack img))
  (assert (<= 3 c 4) "only 3 and 4 channel images are supported")

  (defn on-active
    [app]
    (def window (gtk-application-window-new app))
    (def pixbuf (gdk-pixbuf-new-from-data
                  data
                  0
                  (if (= c 3) false true)
                  8
                  w
                  h
                  s
                  nil
                  nil))
    (def chart (gtk-image-new-from-pixbuf pixbuf))
    (gtk-container-add window chart)
    (gtk-widget-show-all window))

  # Run the app
  (def app (gtk-application-new "org.guitool.a.b" 0))
  (def cb (ffi/trampoline :default))
  (g-signal-connect-data app "activate" cb on-active nil 1)
  (g-application-run app 0 nil))

###
### Run as script or compile with quickbin
### janet-pm quickbin examples/gtk-show-chart.janet weather
###

(defn main [&]

  (charts/dark-mode)

  (def canvas (gfx2d/blank (* 1 1024) (* 1 1024)))
  (gfx2d/fill-rect canvas 0 0 10000 10000 gfx2d/black)
  (def [view convert]
    (charts/draw-axes
      canvas
      :padding 4
      :format-y |(string/format "$%.2f" $)
      :x-label "Units"
      :y-label "Dollars"
      :y-min 0
      :grid :stipple
      :x-ticks (range 0 11)
      :x-min 0
      :x-max 10
      :y-max 100))

  (charts/plot-line-graph
    :canvas view
    :to-pixel-space convert
    :x-column :x
    :y-column [:y :z]
    :x-colors (fn [_ y index] (gfx2d/rgb (* y 0.01) 0.2 0.2))
    :data {:x (range 0 10.1 0.1)
           :y (seq [x :range [0 10.1 0.1]] (+ 50 (* 40 (math/sin (* 1 x)))))
           :z (seq [x :range [0 10.1 0.1]] (+ 50 (* 40 (math/cos (* 1 x)))))}
    :super-sample 4
    :circle-points true
    :point-radius 10
    :line-style :stroke)

  # Lets add a legend in the top right corner
  (def legend-args [:labels [:y :z] :legend-map {:y "Thing 1" :z "Thing 2"} :frame true :padding 14])
  (def [lw lh] (charts/draw-legend nil ;legend-args))
  (def {:width vw :height vh} (gfx2d/unpack view))
  (def legend-view (gfx2d/viewport view (- vw lw 10) 10 lw lh true))
  (charts/draw-legend legend-view ;legend-args)

  (show-gtk-image canvas))
