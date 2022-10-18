(use ../spork/test)
(import ../spork/cron)

(start-suite)

(defn format-time
  "Convert an integer time since epoch to generally readable string."
  [time &opt local]
  (unless time (break ""))
  (def {:hours hours
        :minutes minutes
        :seconds seconds
        :month month
        :month-day month-day
        :year year} (os/date time local))
  (string/format "%d-%.2d-%.2d %.2d:%.2d:%.2d"
                 year (inc month) (inc month-day)
                 hours minutes seconds))

(defn get-sequence
  "Print the next n timestamps of a cron sequence"
  [cron n &opt start-time local]
  (var time start-time)
  (def cron (if (bytes? cron) (cron/parse-cron cron) cron))
  (def arr @[])
  (repeat n
    (set time (cron/next-timestamp cron time local))
    (array/push arr (format-time time local)))
  arr)

(defn print-sequence
  [cron n &opt start-time local]
  (map print (get-sequence cron n start-time local)))

# Pick a stable start time
(def stable-start 1665693611)

(assert (deep= (get-sequence "10 14 * jan mon,tue" 10 stable-start)
               @["2023-01-02 14:10:00"
                 "2023-01-03 14:10:00"
                 "2023-01-09 14:10:00"
                 "2023-01-10 14:10:00"
                 "2023-01-16 14:10:00"
                 "2023-01-17 14:10:00"
                 "2023-01-23 14:10:00"
                 "2023-01-24 14:10:00"
                 "2023-01-30 14:10:00"
                 "2023-01-31 14:10:00"]) "sequence 1")

(end-suite)
