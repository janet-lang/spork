###
### Download weather data and render it into a chart weather.png
###

(import spork/charts)
(import spork/json)
(import spork/http)

# Parameters of request
(def output "tmp/weather.png")
(os/mkdir "tmp")
(def past-days 5)

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

# Make sure chart can handle missing data
# (put data-frame :precipitation_probability (slice (get data-frame :precipitation_probability) 0 200))
# TODO - decide if we should interpolate or remove missing data
#(for i 100 200
#  (set ((data-frame :precipitation_probability) i) nil))

# So fetch!
#(charts/dark-mode)

# Render a nice chart
(charts/line-chart
  :title (string "What is the forecast in " (get location :name) "?")
  :x-label "Hourly Measurements"
  :color-seed 4
  :width 960
  :height 540
  :data data-frame
  :x-column :time
  :y-columns y-columns
  :color-map {:temperature_2m 0xFF0000FF
              :wind_speed_10m 0xFFFF0000
              :relative_humidity_2m 0xFF96AF00}
  :y-label "°F / kmph / %"
  :legend-map {:temperature_2m "Temperature (°F)"
               :wind_speed_10m "Wind Speed (km/h)"
               :relative_humidity_2m "Humidity (%)"
               :precipitation_probability "Precipitation Probability (%)"}
  :line-style :stipple #{:temperature_2m :stroke}
  :x-ticks x-ticks
  :format-x format-x
  :font :olive
  #:grid :solid
  :legend :top
  #:font :default
  #:x-minor-ticks 12
  #:y-minor-ticks 10
  :save-as output)

(print "Wrote chart to " output)
