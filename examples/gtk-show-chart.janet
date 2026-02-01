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

  (def location
    {:name "Austin, TX"
     :latitude 30.27
     :longitude -97.74})

  (def past-days 5)
  (def y-columns [:temperature_2m :wind_speed_10m :relative_humidity_2m :precipitation_probability])

  # Get weather data
  (def url
    (string
      "http://api.open-meteo.com/v1/forecast?"
      "latitude=" (string/format "%.3f" (get location :latitude))
      "&longitude=" (string/format "%.3f" (get location :longitude))
      "&past_days=" past-days
      "&hourly=" (string/join y-columns ",")
      "&temperature_unit=fahrenheit"))
  (print "Getting weather data from " url " ...")
  (def raw-data (get (http/request "GET" url) :body))
  (print "Got weather data!")
  (def structured (json/decode raw-data true))
  (def data-frame (get structured :hourly))

  # Special handling for time series (x coordinates are time stamps)
  (def timestamps (get data-frame :time))
  (put data-frame :time (range (length timestamps)))
  (def x-ticks (seq [[i x] :pairs timestamps :when (string/has-suffix? "T00:00" x)] i))
  (defn format-x [x] (slice (first (string/split "T" (get timestamps (math/round x) ""))) 5))

  # Render a nice chart
  (def weather-chart
    (charts/line-chart
      :title (string "What is the forecast in " (get location :name) "?")
      :x-label "Hourly Measurements"
      :color-seed 4
      :width 960
      :height 540
      :data data-frame
      :x-column :time
      :y-columns y-columns
      :grid :stipple
      :color-map {:temperature_2m 0xFF0000FF
                  :wind_speed_10m 0xFFFF0000
                  :relative_humidity_2m 0xFF96AF00}
      :y-label "°F / kmph / %"
      :legend-map {:temperature_2m "Temperature (°F)"
                   :wind_speed_10m "Wind Speed (km/h)"
                   :relative_humidity_2m "Humidity (%)"
                   :precipitation_probability "Precipitation Probability (%)"}
      :x-ticks x-ticks
      :format-x format-x
      :font :olive
      :legend :top))

  (show-gtk-image weather-chart))
