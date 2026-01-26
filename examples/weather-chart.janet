###
### Download weather data and render it into a chart weather.png
###

(import spork/charts)
(import spork/json)
(import spork/http)

# Get weather data

(def url "http://api.open-meteo.com/v1/forecast?latitude=30.27&longitude=-97.74&past_days=5&hourly=temperature_2m,relative_humidity_2m,wind_speed_10m&temperature_unit=fahrenheit")
(print "Getting weather data...") (flush)
(def raw-data (get (http/request "GET" url) :body))
(print "Got weather data! body is " (length raw-data) " bytes") (flush)
(def structured (json/decode raw-data true))
(spit "weather.jdn" (string/format "%j" structured))
(def df (get structured :hourly))

# Special handling for time series (x coordinates are time stamps)
(def timestamps (get df :time))
(put df :time (range (length timestamps)))
(def x-ticks (seq [[i x] :pairs timestamps :when (string/has-suffix? "T00:00" x)] i))
(defn format-x [x] (slice (first (string/split "T" (get timestamps (math/round x) ""))) 5))

# Render a nice chart
(charts/line-chart
  #:title "What is the Weather?"
  :color-seed 4
  :width 1024
  :height 512
  :data df
  :x-column :time
  :y-columns [:temperature_2m :wind_speed_10m :relative_humidity_2m]
  :color-map {:temperature_2m 0xFF0000FF :wind_speed_10m 0xFFFF0000 :relative_humidity_2m 0xFF96AF00}
  :y-suffix " °F/kmph/%"
  # :color-seed 101 # for arbitrary colors
  :legend-map {:temperature_2m "Temperature (°F)" :wind_speed_10m "Wind Speed (km/h)" :relative_humidity_2m "Humidity (%)"}
  :x-ticks x-ticks
  :format-x format-x
  :font :olive
  :grid true
  :legend :top
  :save-as "weather.png")
