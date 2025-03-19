###
### cron.janet
### 
### Timer library for interfacing with the UNIX crontab format.
###
### The cron format support is based on the unix cron syntax, with an optional
### seconds field. Each field can be a comma separated list of individual values
### or a range of values.
### A range has three variants as follows
###   Two values with a "-" between them, optionally followed by a "/" and a step value.
###   An asterisk ("*") followed by a "/" and a step value. This implies every "step" value.
###   A single value followed by a "/" and a step value. This implies every "step" value
###     starting with the single value. i.e. 2/3 implies every 3 units from 2 to max units.
### A single asterisk ("*") can be used to denote all possible values.
###
### The fields:
###  * minutes: 0-59
###  * hours: 0-23
###  * day of month: 1-31
###  * month: 1-12. Also allowed are the following month codes in any case:
###    jan,feb,mar,apr,may,jun,jul,aug,sep,oct,nov,dec
###  * day of week: 0-7, where 0 or 7 is sunday, monday is 1, etc. allows the following day codes (any case):
###    sun,mon,tue,wed,thu,fri,sat
###  * seconds (optional): 0-59
###
### Cron schedules are represented as tuples of 7 values, a string representation, followed
### by 6 bitmaps representing matching timestamps. Bitmaps are represented as any byte sequence.
###
### [string-rep minutes hours day-of-month month day-of-week seconds]
###
### Note that we have second precision here as opposed to minute precision
###

(defn- bitmap-set
  "Set a bit in a buffer"
  [bitmap i]
  (def idx (math/floor (* 0.125 i)))
  (def mask (blshift 1 (% i 8)))
  (set (bitmap idx) (bor mask (in bitmap idx)))
  bitmap)

(defn- bitmap-setrange
  "Add a range of values to a bitmap"
  [bitmap start end &opt step]
  (default step 1)
  (loop [i :range-to [start end step]]
    (bitmap-set bitmap i))
  bitmap)

(defn- bitmap-check
  "Check if a given index is set in a bitmap"
  [bitmap i]
  (def idx (math/floor (* 0.125 i)))
  (def mask (blshift 1 (% i 8)))
  (def test (band mask (in bitmap idx)))
  (not= 0 test))

(defn- bitmap-cycle
  "Traverse a bitmap to find the first set bit. Will
  wrap around to the beginning to find a set bit if one is not found"
  [bitmap start]
  (var ret nil)
  (for i start (* 8 (length bitmap))
    (if ret (break))
    (if (bitmap-check bitmap i) (set ret i)))
  # wrap around logic
  (for i 0 start
    (if ret (break))
    (if (bitmap-check bitmap i) (set ret i)))
  ret)

###
### Cron format parsing
###

(def- month-codes
  (invert ["jan"
           "feb"
           "mar"
           "apr"
           "may"
           "jun"
           "jul"
           "aug"
           "sep"
           "oct"
           "nov"
           "dec"]))

(def- dow-codes
  (invert ["sun" "mon" "tue" "wed" "thu" "fri" "sat"]))

(defn- capture-range
  [start &opt end step]
  (default end start)
  (default step 1)
  (if (= start :*)
    [start start end]
    (if (and (string? start) (string/has-suffix? "*" start))
      [(scan-number (string/replace "*" "" start)) :* end]
      [start end step])))

(def- cron-peg
  (peg/compile
    ~{:3code '(some :a)
      :num (+ (/ ':d+ ,scan-number) :3code)
      :star (* "*" (constant :*))
      :start (/ ':d+ ,|(string $0 "*"))
      :num-range (* :num "-" :num)

      :slash-left (+ :star :num-range :start)
      :slash (* :slash-left "/" :num)

      :range (+ :slash :num-range :num)
      :range-val (/ :range ,capture-range)
      :range-group (group (* :range-val (any (* "," :range-val))))

      :field (+ :range-group :star)

      :main (* :field :s+ :field :s+ :field :s+ :field :s+ :field :s*
               (? (* :field :s*)) -1)}))

(defn- validate-ranges
  "Processed parsed ranges for a cron field"
  [debug-name ranges bitmap mini maxi &opt offset codes]
  (default offset 0)
  (string
    (if (= :* ranges)
      (bitmap-setrange bitmap (+ offset mini) (+ offset maxi))
      (do
        (each [start end step] ranges
          (def start (if (= :* start)
                       mini
                       (if (and codes (string? start))
                         (- (assert (in codes (string/ascii-lower start)) (string "invalid code " start)) offset)
                         start)))
          (def end (if (= :* end)
                     maxi
                     (if (and codes (string? end))
                       (- (assert (in codes (string/ascii-lower end)) (string "invalid code " end)) offset)
                       end)))
          (assert (>= start mini) (string "invalid " debug-name ": " start))
          (assert (<= end maxi) (string "invalid " debug-name ": " end))
          (bitmap-setrange bitmap (+ offset start) (+ offset end) step))
        bitmap))))

(defn parse-cron
  "Parse a cron string into a valid cron schedule object"
  [str]
  (def minute-buffer (buffer/new-filled 8 0))
  (def second-buffer (buffer/new-filled 8 0))
  (def hour-buffer (buffer/new-filled 3 0))
  (def month-buffer (buffer/new-filled 2 0))
  (def dow-buffer (buffer/new-filled 1 0))
  (def dom-buffer (buffer/new-filled 4 0))
  (def matched (peg/match cron-peg str))
  (def [minute-ranges
        hour-ranges
        dom-ranges
        month-ranges
        dow-ranges
        second-ranges] (assert matched "failed to parse cron fields"))
  (default second-ranges @[[0 0 1]])
  # unix cron quirk - if both dow and dom are not asterix, then special behavior
  [str
   (validate-ranges "minute" minute-ranges minute-buffer 0 59)
   (validate-ranges "hour" hour-ranges hour-buffer 0 23)
   (validate-ranges "day of month" dom-ranges dom-buffer 1 31 -1)
   (validate-ranges "month" month-ranges month-buffer 1 12 -1 month-codes)
   (validate-ranges "day of week" dow-ranges dow-buffer 0 7 0 dow-codes)
   (validate-ranges "second" second-ranges second-buffer 0 59)
   (and (not= :* dom-ranges) (not= :* dow-ranges))])

(defn check
  "Check if a given time matches a cron specifier."
  [cron &opt time local]
  (default time (os/time))
  (def cron (if (bytes? cron) (parse-cron cron) cron))
  (def {:hours h
        :minutes m
        :month M
        :month-day Md
        :week-day wd
        :year-day d
        :seconds s}
    (os/date time local))
  (def [_ minutes hours day-of-month month day-of-week seconds dow-or-dom] cron)
  (def day-correct
    (if dow-or-dom
      (or
        (bitmap-check day-of-month Md)
        (bitmap-check day-of-week wd))
      (and
        (bitmap-check day-of-month Md)
        (bitmap-check day-of-week wd))))
  (and
    day-correct
    (bitmap-check seconds s)
    (bitmap-check minutes m)
    (bitmap-check hours h)
    (bitmap-check month M)))

(defn- next-candidate
  "Get a conservative estimate for the next timestamp available."
  [cron &opt time local]
  (def [_ cron-m cron-h cron-Md cron-M cron-wd cron-s dow-or-dom] cron)
  (def {:hours h
        :minutes m
        :seconds s
        :month M
        :month-day Md
        :week-day wd
        :year y
        :year-day d}
    (os/date time local))

  # Ensure correct month
  (def next-M (bitmap-cycle cron-M M))
  (when (not= next-M M)
    (def next-y (if (< next-M M) (inc y) y))
    (break
      (os/mktime {:hours 0 :minutes 0 :seconds 0 :month next-M :year next-y :month-day 0} local)))

  # Ensure correct day (increment by 1 day at a time)
  (def next-Md (bitmap-cycle cron-Md Md))
  (def next-wd (bitmap-cycle cron-wd wd))
  (def month-day-correct (= next-Md Md))
  (def week-day-correct (= next-wd wd))
  (def day-correct
    (if dow-or-dom
      (or month-day-correct week-day-correct)
      (and month-day-correct week-day-correct)))
  (unless day-correct
    (break (os/mktime {:hours 0 :minutes 0 :seconds 0 :month M :year y :month-day (inc Md)} local)))

  # Ensure correct hour
  (def next-h (bitmap-cycle cron-h h))
  (when (not= next-h h)
    (def next-Md (if (< next-h h) (inc Md) Md))
    (break
      (os/mktime {:hours next-h :minutes 0 :seconds 0 :month M :year y :month-day next-Md} local)))

  # Ensure correct minute (allow hour overflows)
  (def next-m (bitmap-cycle cron-m m))
  (when (not= next-m m)
    (def next-h (if (< next-m m) (inc h) h))
    (break
      (os/mktime {:hours next-h :minutes next-m :seconds 0 :month M :year y :month-day Md} local)))

  # Ensure correct second (allow minute overflows)
  (def next-s (bitmap-cycle cron-s s))
  (when (not= next-s s)
    (def next-m (if (< next-s s) (inc m) m))
    (break
      (os/mktime {:hours next-h :minutes next-m :seconds next-s :month M :year y :month-day Md} local)))

  time)

(defn next-timestamp
  "Given a cron schedule, get the next instance on the cron tab after time"
  [cron &opt time local]
  (default time (os/time))
  (def cron (if (bytes? cron) (parse-cron cron) cron))
  (var test-time (+ 1 time)) # 1 second resolution
  (var recur-limit 300)
  (while (not (check cron test-time local))
    (if (zero? (-- recur-limit)) (error "could not find next timestamp"))
    (set test-time (next-candidate cron test-time local)))
  test-time)
