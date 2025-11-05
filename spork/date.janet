### 
### date.janet
### 
### Utility wrappers around `os/time` and `os/date` for working
### with dates, particularly formatting them as strings (with 
### `date/to-string`) and reading dates from strings (with 
### `date/from-string`).
###

(import ./schema)

(setdyn :doc "Utility wrappers around `os/time` and `os/date` for doing simple time-intelligent work with dates.")

(def- date-schema
  ~(and :struct
        (props
          :dst :boolean
          :hours :number
          :minutes :number
          :month :number
          :month-day :number
          :seconds :number
          :week-day :number
          :year :number
          :year-day :number)))

(def date?
  ``
  A `date` is a struct that contains at least the following keys: 

  - `:dst`
  - `:year`
  - `:year-day`
  - `:month`
  - `:month-day`
  - `:week-day`
  - `:hours`
  - `:minutes`
  - `:seconds`

  The `(os/date)` function returns well-formed `date`s of this type.
  ``
  ((schema/make-predicate date-schema)))

(def assert-date
  ``
  A `date` is a struct that contains at least the following keys: 

  - `:dst`
  - `:year`
  - `:year-day`
  - `:month`
  - `:month-day`
  - `:week-day`
  - `:hours`
  - `:minutes`
  - `:seconds`

  The `(os/date)` function returns well-formed `date`s of this type.
  ``
  ((schema/make-validator date-schema)))

(defn utc-now
  ``
  Returns a well-formed `date` representing the current datetime in UTC. 
  Equivalent to `(os/date)`.
  ``
  []
  (os/date))

(defn local-now
  ``
  Returns a well-formed date representing the current datetime in the 
  local time zone. Equivalent to `(os/date nil true)`.
  ``
  []
  (os/date nil true))

(defn compare
  ``
  Compares `date` values (`d1` and `d2`).

  Returns -1 if `d1` is strictly less than `d2`, 1 if `d2` is 
  strictly less than `d1`, and 0 if they are exactly the same. 
  ``
  [d1 d2]
  (assert-date d1)
  (assert-date d2)
  (let [t1 (os/mktime d1)
        t2 (os/mktime d2)]
    (cmp t1 t2)))

(defn lt
  ``
  Equivalent to `(< ;(map os/mktime [d1 d2]))`.
  ``
  [& dates]
  (each date dates (assert-date date))
  (< ;(map os/mktime dates)))

(defn gt
  ``
  Equivalent to `(> ;(map os/mktime [d1 d2]))`.
  ``
  [& dates]
  (each date dates (assert-date date))
  (> ;(map os/mktime dates)))

(def- format-peg
  ~{:y2 (/ '"yy" :y2)
    :y4 (/ '"yyyy" :y4)
    :y (+ :y4 :y2)
    :m1 (/ '"M" :m1)
    :m2 (/ '"MM" :m2)
    :m3 (/ '"MMM" :m3)
    :m4 (/ '"MMMM" :m4)
    :m (+ :m4 :m3 :m2 :m1)
    :d1 (/ '"d" :d1)
    :d2 (/ '"dd" :d2)
    :d (+ :d2 :d1)
    :h1 (/ '"h" :h1)
    :h2 (/ '"hh" :h2)
    :h (+ :h2 :h1)
    :H1 (/ '"H" :H1)
    :H2 (/ '"HH" :H2)
    :H (+ :H2 :H1)
    :min (/ '"mm" :min)
    :sec (/ '"ss" :sec)
    :am (/ '(+ "am" "pm" "AM" "PM") :am)
    :sep (/ '1 :sep)
    :main (some (+ :y :d :h :H :min :sec :m :am :sep))})

(def- short-month-map
  ["jan" "feb" "mar" "apr" "may" "jun"
   "jul" "aug" "sep" "oct" "nov" "dec"])

(def- long-month-map
  ["january" "february" "march" "april" "may" "june"
   "july" "august" "september" "october" "november" "december"])

(defn from-string
  ``
  Construct a well-formed `date` from a string (`date-str`), using 
  a second string to indicate the format to be read (`format-str`). 
  Returns a new struct that will pass the `date?` validation function. 

  `format-str` is case sensitive and can understand any of:

  - `y` = Year (either 2 or 4 characters)
  - `M` = Month (between 1 and 4 characters)
  - `d` = Day of month (1 or 2 characters)
  - `H` = Hour, 24-hour format (1 or 2 characters)
  - `h` = Hour, 12-hour format (1 or 2 characters)
  - `m` = Minute (must be 2 characters)
  - `s` = Second (must be 2 characters)
  - `AM` or `PM` = AM/PM (either upper- or lower-case, use with `h`)

  Any other characters (e.g. `:`, ` `, `,`, `-` or `/`) will be treated 
  as separators and ignored.
  ``
  [date-str format-str]

  (def date-peg
    ~@{:y2 (/ (number (2 :d)) ,|[:year (+ 2000 $)])
       :y4 (/ (number (4 :d)) ,|[:year $])
       :m1 (/ (number (at-most 2 :d)) ,|[:month (dec $)])
       :m2 (/ (number (2 :d)) ,|[:month (dec $)])
       :m3 (/ (accumulate (3 ':w))
              ,|[:month (index-of (string/ascii-lower $)
                                  short-month-map)])
       :m4 (/ (accumulate (at-most 9 ':w))
              ,|[:month (index-of (string/ascii-lower $)
                                  long-month-map)])
       :d1 (/ (number (at-most 2 :d)) ,|[:month-day (dec $)])
       :d2 (/ (number (2 :d)) ,|[:month-day (dec $)])
       :h1 (/ (number (at-most 2 :d)) ,|[:hours-12 $])
       :h2 (/ (number (2 :d)) ,|[:hours-12 $])
       :H1 (/ (number (at-most 2 :d)) ,|[:hours $])
       :H2 (/ (number (2 :d)) ,|[:hours $])
       :min (/ (number (2 :d)) ,|[:minutes $])
       :sec (/ (number (2 :d)) ,|[:seconds $])
       :am (/ (accumulate (2 ':w)) ,|[:am/pm (string/ascii-lower $)])
       :sep 1})

  (def format-code
    (peg/match format-peg format-str))

  (put date-peg :main ['* ;format-code -1])

  (def peg-matches
    (peg/match date-peg date-str))

  (assert peg-matches (string/format "could not match date with provided format: %q and %q" date-str format-str))

  (def parsed-date
    (from-pairs peg-matches))

  # Handle AM/PM and 12-hour time formats
  (when-let [am/pm (parsed-date :am/pm)
             orig (last am/pm)
             hours (or (parsed-date :hours)
                       (parsed-date :hours-12))]
    (put parsed-date :am/pm nil)
    (put parsed-date :hours-12 nil)
    (cond
      (and (= "pm" am/pm) (< hours 12))
      (put parsed-date :hours (+ 12 hours))

      (and (= "am" am/pm) (= hours 12))
      (put parsed-date :hours 0)))

  (os/date (os/mktime (struct/with-proto (os/date 0) ;(kvs parsed-date)))))

(defmacro- capitalize [str]
  (with-syms [$str]
    ~(let [,$str ,str]
       (string (string/ascii-upper (string/slice ,$str 0 1))
               (string/slice ,$str 1)))))

(defn to-string
  ``
  Convert a well-formed `date` (`date`) to a string, using a string 
  (`format-str`) to indicate the expected output format. Returns a 
  string representing the formatted date.

  `format-str` is case sensitive and can understand any of:

  - `y` = Year (either 2 or 4 characters)
  - `M` = Month (between 1 and 4 characters)
  - `d` = Day of month (1 or 2 characters)
  - `H` = Hour, 24-hour format (1 or 2 characters)
  - `h` = Hour, 12-hour format (1 or 2 characters)
  - `m` = Minute (must be 2 characters)
  - `s` = Second (must be 2 characters)
  - `AM` or `PM` = AM/PM (either upper- or lower-case, use with `h`)

  Any other characters (e.g. `:`, ` `, `,`, `-` or `/`) will be 
  treated as separators and preserved identically.

  Well-formatted `dates` are structs as returned by the built-in 
  function `os/date`. Verify that a struct is well-formed using the 
  `date?` validation function.
  ``
  [date format-str]
  (assert-date date)

  (def my-format-peg (struct/to-table format-peg))
  (put my-format-peg :sep ['/ '(<- 1) |[:sep $]])
  (def format-code (peg/match my-format-peg format-str))

  (def out-arr @[])

  (each element format-code
    (array/push
      out-arr
      (case element
        :y2 (string/slice (string/format "%04d" (date :year)) -3)
        :y4 (string/format "%04d" (date :year))
        :m1 (string (inc (date :month)))
        :m2 (string/format "%02d" (inc (date :month)))
        :m3 (string/ascii-upper (short-month-map (date :month)))
        :m4 (capitalize (long-month-map (date :month)))
        :d1 (string (inc (date :month-day)))
        :d2 (string/format "%02d" (inc (date :month-day)))
        :h1 (string (if (= 0 (date :hours)) 12 (% (date :hours) 12)))
        :h2 (string/format "%02d" (if (= 0 (date :hours)) 12 (% (date :hours) 12)))
        :H1 (string (date :hours))
        :H2 (string/format "%02d" (date :hours))
        :min (string/format "%02d" (date :minutes))
        :sec (string/format "%02d" (date :seconds))
        :am (if (> (date :hours) 11) "PM" "AM")
        (last element))))

  (string/join out-arr))

(defn- leap-year? [year]
  (cond
    (= (% year 400) 0) true
    (= (% year 100) 0) false
    (= (% year 4) 0) true
    false))

(defmacro- do-add-or-sub
  [f date years months days hours minutes seconds]
  (with-syms [$f $date $years $months $days
              $hours $minutes $seconds]
    ~(let [,$f ,f
           ,$date ,date
           ,$years ,years
           ,$months ,months
           ,$days ,days
           ,$hours ,hours
           ,$minutes ,minutes
           ,$seconds ,seconds]

       (var time (os/mktime (struct/with-proto (os/date 0) ;(kvs ,$date))))

       (when ,$days (set time (,$f time (* ,$days 60 60 24))))
       (when ,$hours (set time (,$f time (* ,$hours 60 60))))
       (when ,$minutes (set time (,$f time (* ,$minutes 60))))
       (when ,$seconds (set time (,$f time ,$seconds)))

       (def ret-date (thaw (os/date time)))

       (when ,$years
         (update ret-date :year |(,$f $ ,$years))
         (when (and (not (leap-year? (ret-date :year)))
                    (= 1 (ret-date :month))
                    (= 28 (ret-date :month-day)))
           (put ret-date :month-day 27)))

       (when ,$months
         (update ret-date :month |(,$f $ ,$months))
         (let [month (ret-date :month)
               day (ret-date :month-day)
               leap-day (if (leap-year? (ret-date :year)) 1 0)]
           (cond
             (= month 1)
             (put ret-date :month-day (min day (+ 28 leap-day)))

             (has-value? [3 5 8 10] month)
             (put ret-date :month-day (min day 30)))))

       (os/date (os/mktime (struct/with-proto (os/date 0) ;(kvs ret-date)))))))

(defn add
  ``
  Add time to a well-formed `date` (`date`). Returns a new struct 
  that will pass the `date?` validation function.
  ``
  [date &named years months days hours minutes seconds]
  (assert-date date)
  (do-add-or-sub + date years months days hours minutes seconds))

(defn sub
  ``
  Subtract time from a well-formed `date` (`date`). Returns a new struct 
  that will pass the `date?` validation function.
  ``
  [date &named years months days hours minutes seconds]
  (assert-date date)
  (do-add-or-sub - date years months days hours minutes seconds))

(defn diff
  ``
  Subtract two well-formed `date`s (`later-date` and `earlier-date`),
  returning a time in seconds. 

  Not commutative, i.e., order matters. If `earlier-date` precedes 
  `later-date`, the resulting duration will be negative.
  ``
  [later-date earlier-date]
  (assert-date later-date)
  (assert-date earlier-date)
  (- (os/mktime later-date) (os/mktime earlier-date)))

(defn between?
  ``
  Given three well-formed `date`s (`date`, `start`, and `end`), 
  will return `true` if `date` occurs between `start` and `end`.
  ``
  [date start end]
  (or (lt start date end)
      (gt start date end)))
