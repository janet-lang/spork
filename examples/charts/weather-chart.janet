###
### Download weather data and render it into a chart weather.png. Demonstrate
### how to make a nice utility script to generate charts from web data.
###

(import spork/gfx2d)
(import spork/charts)
(import spork/json)
(import spork/http)
(import spork/test)

# Bring your own location!

(def location
  {:name "Austin, TX"
   :latitude 30.27
   :longitude -97.74})

#(def location
#  {:name "Death Valley, CA"
#   :latitude 36.246944
#   :longitude -116.816944})

#(def location
#  {:name "Nome, AK"
#   :latitude 64.503889
#   :longitude -165.399444})

# Parameters of request
(defn get-weather
  []
  (def output "tmp/weather.png")
  (os/mkdir "tmp")
  (def past-days 5)

  (def y-columns [:temperature_2m :wind_speed_10m :relative_humidity_2m :precipitation_probability])

  # Get weather data
  (def url
    (string
      "http://api.open-meteo.com/v1/forecast?"
      "latitude=" (string/format "%.3f" (get location :latitude))
      "&longitude=" (string/format "%.3f" (get location :longitude))
      "&past_days=" past-days
      "&timezone=Etc/UTC"
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

  # Find the "now" location for a horizontal line
  (def ts-parse (peg/compile ~{:n (/ ':d+ ,scan-number)
                               :main (* :n "-" :n "-" :n "T" :n ":" :n)}))
  (def {:year Y :month-day D :hours H :month M} (os/date))
  (def now-index (find-index |(if-let [[y m d h s] (peg/match ts-parse $)] (and (= y Y) (= m (inc M))
                                                                                (= d (inc D)) (= h H)))
                             timestamps))

  # Then split our forecase data into before and after now.
  (each col y-columns
    (def historic-data (array/slice (get data-frame col)))
    (def forecast-data (array/slice (get data-frame col)))
    (for i 0 (length historic-data)
      (when (< i now-index) (set (forecast-data i) nil))
      (when (> i now-index) (set (historic-data i) nil)))
    (put data-frame col historic-data)
    (put data-frame (keyword col "-forecast") forecast-data))
  (def extended-y-columns [;y-columns ;(map |(keyword $ "-forecast") y-columns)])
  (def styles (tabseq [c :in extended-y-columns]
                      c (if (string/has-suffix? "-forecast" c)
                          :stroke-stipple
                          :stroke)))

  # So fetch (disable this for black-on-white instead of white-on-black, looks more professional, less cool)
  (charts/dark-mode)

  # Render a nice chart
  # Play with the syling here to customize the chart.
  (charts/line-chart
    :title (string "What is the forecast in " (get location :name) "?")
    :x-label "Hourly Measurements"
    :width (* 2 960)
    :height (* 2 540)
    :data data-frame
    :x-column :time
    :y-column extended-y-columns
    :color-map {:temperature_2m 0xFF0000dd
                :wind_speed_10m 0xFFdd0000
                :relative_humidity_2m 0xFF96AF00
                :temperature_2m-forecast 0xFF0000dd
                :wind_speed_10m-forecast 0xFFdd0000
                :relative_humidity_2m-forecast 0xFF96AF00}
    :y-label "°F / kmph / %"
    :legend-map {:temperature_2m "Temperature (°F)"
                 :wind_speed_10m "Wind Speed (km/h)"
                 :relative_humidity_2m "Humidity (%)"
                 :precipitation_probability "Precipitation Probability (%)"}
    :legend-labels y-columns
    :line-style-per-column styles
    :x-ticks x-ticks
    :format-x format-x
    # :transpose true
    # :y-ticks x-ticks
    # :format-y format-x
    # :font :olive
    # :olive, :default, and :tall are built-in bitmap fonts that don't need an external TTF file.
    :font (gfx2d/load-font "examples/fonts/Roboto-Regular.ttf" 29.5)
    :padding 40 # default padding is 16, larger padding looks better for larger fonts and images.
    :super-sample 4 # Super-sampled anti-aliasing is a great way to get nice charts.
    # It is a bit slow to render, especially for large images (4x super-sampled is 16 times the pixels). Going beyond 4 is usually superfluous.
    :stroke-thickness 1
    :grid :solid # grid can be :none (nil), :solid, or :stipple
    #:grid :stipple
    :legend :top # legend can be :none (nil), :top, :top-left, :top-right, :bottom-left, :bottom-right
    #:legend :top-right
    #:x-minor-ticks 12
    #:y-minor-ticks 10
    :annotate (fn [view to-pixel to-metric]
                (def [x-now _] (to-pixel now-index 0))
                (gfx2d/draw-simple-text view (+ 2 x-now) 2 "Now" gfx2d/white :olive 2 2)
                (gfx2d/plot view x-now 0 x-now 10000 gfx2d/white))
    :save-as output)

  (print "Wrote chart to " output)
  (flush))

(defn loop-weather
  []
  (forever
    (get-weather)
    (ev/sleep 30)))

(defn main [&] (get-weather))
