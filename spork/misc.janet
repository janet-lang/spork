###
### misc.janet
###
### One-off functions that don't need their own module.
###

(defn dedent
  ```
  Remove indentation after concatenating the arguments. Works by removing
  leading whitespace, and then removing that same pattern of whitepsace after
  new lines.
  ```
  [& xs]
  (def x (string ;xs))
  (def first-letter (find-index (fn [c] (and (not= c (chr "\n"))
                                             (not= c (chr " "))
                                             (not= c (chr "\t"))
                                             (not= c (chr "\r")))) x))
  (if (not first-letter) (break ""))
  (def leading-whitespace (string/slice x 0 first-letter))
  (def indent (last (string/split "\n" leading-whitespace)))
  (if (and indent (not= indent ""))
    (let [y (string/replace-all (string "\n" indent) "\n" (string/replace indent "" x))]
      # Remove trailing newline to mimic long string newline omission.
      (if (= (chr "\n") (last y))
        (slice y 0 -2)
        y))
    x))

(defmacro set*
  ```
  Parallel `set` function.  Takes a list of targets and
  expressions, evaluates all the expressions, and then
  assigns them to the targets.  Each target can be a variable
  or a 2-tuple, just like in the normal `set` special form.
  ```
  [tgts exprs]
  (when (not= (length tgts) (length exprs))
    (error "Expected tgts and exprs to have same length"))
  (def code @['do])
  (def syms @[])
  (loop [e :in exprs]
    (def sym (gensym))
    (array/push syms sym)
    (array/push code (tuple 'def sym e)))
  (loop [[i t] :pairs tgts]
    (array/push code (tuple 'set t (in syms i))))
  (tuple ;code))

(defn print-table
  ```
  Iterate through the rows of a data structure and print a table in a human
  readable way, with padding and heading information. Can optionally provide
  a function use to print a row, as well as optionally select column keys
  for each row. Lastly, a `header-mapping` dictionary can be provided that
  changes the printed header names my mapping column keys to the desired
  header name. If no mapping is found, then the column key will be used as
  the header name. Returns nil.
  ```
  [data &opt columns header-mapping]
  (var colkeys columns)
  (def column-widths @{})
  (def processed @[])
  (default header-mapping {})

  # Preprocess rows
  (each row data
    (unless colkeys
      (set colkeys (sorted (keys row))))
    (def newrow @[])
    (each key colkeys
      (def item (string (in row key)))
      (set (column-widths key) (max (length item) (get column-widths key 0)))
      (array/push newrow item))
    (array/push processed newrow))

  # Apply width of header names
  (each key colkeys
    (def header (get header-mapping key key))
    (set (column-widths key) (max (length header) (get column-widths key 0))))

  # Generate format string
  (var bar-width 0)
  (def fbuf @"")
  (each key colkeys
    (def width (+ 2 (get column-widths key 6)))
    (+= bar-width width)
    (buffer/push fbuf "%" (string width) "s"))
  (def format-string (string fbuf))

  # Print header
  (each key colkeys
    (def header (get header-mapping key key))
    (def str
      (string header
              (string/repeat " " (- (column-widths key) (length header) -2))))
    (prin str))
  (print)
  (print (string/repeat "═" bar-width))

  # Finally body
  (each row processed
    (printf format-string ;row)))

(defn- default-traversal-predicate
  [x]
  (def has-kids
    (in
      {:array true
       :tuple true
       :struct true
       :table true
       :fiber true}
      (type x)))
  (if has-kids x))

(defn dfs
  ```
  Do a depth first, pre-order traversal over a data structure.
  Also allow for callbacks before and after visiting the children
  of a node. Also allow for a custom `get-children` function to
  change traversal as needed. Will detect cycles if an empty table
  is passed as the `seen` parameter, which is used to cached values
  that have been visited.
  ```
  [data visit-leaf &opt node-before node-after get-children seen]
  (default get-children default-traversal-predicate)
  (when seen
    (if (in seen data) (break))
    (put seen data true))
  (if-let [node (get-children data)]
    (do
      (when node-before (node-before node))
      (each child node
        (dfs child visit-leaf node-before node-after get-children seen))
      (when node-after (node-after node)))
    (visit-leaf data)))


###### Array Helpers ######

(defn array/randomize
  ```
  Randomizes array using the fisher-yates shuffle, takes an optional random
  number generator.
  ```
  [arr &opt rng]
  (default rng (math/rng (os/cryptorand 8)))
  (def l (length arr))
  (loop [i :range [0 l]]
    (def a (- (- l i) 1))
    (def b (math/rng-int rng (+ a 1)))
    (def tmp (arr a))
    (put arr a (arr b))
    (put arr b tmp))
  arr)

(def randomize-array
  "Deprecated not scoped version of `array/randomize`"
  {:deprecated :normal}
  array/randomize)

##### String Helpers #####

(defn string/trim-prefix
  "Trim the specified prefix of a string if it has one"
  [prefix str]
  (if (string/has-prefix? prefix str)
    (slice str (length prefix) -1)
    str))

(def trim-prefix
  "Deprecated not scoped version of `string/prefix`"
  {:deprecated :normal}
  string/trim-prefix)

(defn string/trim-suffix
  "Trim the specified suffix of a string if it has one"
  [suffix str]
  (if (string/has-suffix? suffix str)
    (slice str 0 (* -1 (+ 1 (length suffix))))
    str))

(def trim-suffix
  "Deprecated not scoped version of `string/suffix`"
  {:deprecated :normal}
  string/trim-suffix)

(defmacro log
  ```
  Print to a dynamic binding stream if that stream is set, otherwise do
  nothing. Evaluate to nil.  
  For example, `(log :err "value error: %V" my-value)` will print 
  to `(dyn :err)` only if `(dyn :err)` has been set.
  ```
  [level & args]
  (def to (gensym))
  ~(when-let [,to (,dyn ,level)]
     (,xprintf ,to ,;args)))

(defn map-keys
  ```
  Returns new table with function `f` applied to `data`'s
  keys recursively.
  ```
  [f data]
  (-> (seq [[k v] :pairs data]
        [(f k) (if (dictionary? v) (map-keys f v) v)])
      flatten
      splice
      table))

(defn map-vals
  "Returns new table with function `f` applied to `data`'s values."
  [f data]
  (def res @{})
  (loop [[k v] :pairs data] (put res k (f v)))
  res)

(defn select-keys
  "Returns new table with selected `keyz` from dictionary `data`."
  [data keyz]
  (def res @{})
  (loop [k :in keyz :when (data k)]
    (put res k (data k)))
  res)

(defmacro cond->
  ```
  Threading conditional macro. It takes `val` to mutate,
  and `clauses` pairs with condition and operation to which the `val`,
  is put as first argument. All conditions are tried and
  for truthy conditions the operation is executed.
  Returns the value mutated if any condition is truthy.
  ```
  [val & clauses]
  (with-syms [res]
    ~(do
       (var ,res ,val)
       ,;(map
           (fn [[cnd ope]]
             (def ope (if (tuple? ope) ope (tuple ope)))
             (tuple
               'if cnd
               (tuple 'set res
                      (tuple (first ope) res
                             ;(tuple/slice ope 1 -1)))))
           (partition 2 clauses))
       ,res)))

(defmacro cond->>
  ```
  Threading conditional macro. It takes `val` to mutate,
  and `clauses` pairs of condition and operation to which the `val`,
  is put as last argument. All conditions are tried and
  for truthy the operation is ran.
  Returns mutated value if any condition is truthy.
  ```
  [val & clauses]
  (with-syms [res]
    ~(do
       (var ,res ,val)
       ,;(map
           (fn [[cnd ope]]
             (def ope (if (tuple? ope) ope (tuple ope)))
             (tuple
               'if cnd
               (tuple 'set res (tuple ;ope res))))
           (partition 2 clauses))
       ,res)))

(defn make
  ```
  Creates new table from even number of kvs pairs in a variadic `table-or-pairs`
  arguments, or take existing table if it is one argument. Then sets tables'
  prototype to `prototype`.
  Factory function for creating new objects from prototypes.
  ```
  [prototype & table-or-pairs]
  (def object
    (if (one? (length table-or-pairs))
      (in table-or-pairs 0) (table ;table-or-pairs)))
  (table/setproto object prototype))

(defmacro do-var
  ```
  Convenience macro for defining varible named `v` with value `d` before `body`
  and returning it after evaluating `body`, that presumably modifies `v`.
  ```
  [v d & body]
  ~(do (var ,v ,d) ,;body ,v))

(defmacro do-def
  ```
  Convenience macro for defining constant named `c` with value `d` before `body`
  and returning it after evaluating `body`, that presumably modifies 
  the `c` refered content. For example buffer, table or array.
  ```
  [c d & body]
  ~(do (def ,c ,d) ,;body ,c))

(defmacro- cap*
  [out & body]
  (with-syms [o]
    ~(as-macro ,do-var ,o @""
               (with-dyns [,out ,o] ,;body))))

(defmacro capout
  "Captures the standart output."
  [& body]
  ~(as-macro ,cap* :out ,;body))

(defmacro caperr
  "Captures the error output of the variadic `body`."
  [& body]
  ~(as-macro ,cap* :err ,;body))

(defmacro vars
  "Defines many variables as in let `bindings`."
  [& bindings]
  ~(upscope
     ,;(seq [[n v] :in (partition 2 bindings)] (tuple 'var n v))))

(defmacro defs
  "Defines many constants as in let `bindings`."
  [& bindings]
  ~(upscope
     ,;(seq [[n v] :in (partition 2 bindings)] (tuple 'def n v))))

(defn always
  "Return a function that discards any arguments and always returns `x`."
  [x]
  (fn [&] x))

(defn second
  "Get the second element from an indexed data structure."
  [xs]
  (get xs 1))

(defn third
  "Get the third element from an indexed data structure."
  [xs]
  (get xs 2))

(defn penultimate
  "Get the second-to-last element from an indexed data structure."
  [xs]
  (get xs (- (length xs) 2)))

(defn antepenultimate
  "Get the third-to-last element from an indexed data structure."
  [xs]
  (get xs (- (length xs) 3)))

(defn int/
  "Perform integer division."
  [& xs]
  (math/trunc (/ ;xs)))

(defmacro gett
  "Recursive macro (get). Similar to get-in, but keys are variadic argument."
  [ds & keys]
  (reduce (fn [t key] (tuple get t key)) ds keys))

(defmacro until
  ```
  Repeat `body` while the `cnd` is false.
  Equivalent to (while (not cnd) ;body).
  ```
  [cnd & body]
  ~(while (not ,cnd) ,;body))

(defn table/filter
  ```
  Filter a key-value structure info a table. Semantics are the same as for
  built-in `filter`, except that `pred` takes two arguments (key and value.)
  Does not consider prototypes.
  ```
  [pred dict]
  (->> dict
       (pairs)
       (filter (fn [[k v]] (pred k v)))
       (from-pairs)))

(defn buffer/reverse
  "Reverse a buffer in-place."
  [buf]
  (def max (dec (length buf)))
  (for i 0 (int/ (length buf) 2)
    (def i* (- max i))
    (def tmp (buf i))
    (put buf i (buf i*))
    (put buf i* tmp))
  buf)

(def- int-alphabet "0123456789abcdefghijklmnopqrstuvwxyz")
(defn string->int
  ```
  Parse an integer in the given base. Defaults to decimal (base 10). Differs
  from scan-number in that this does not recognize floating point notation.
  ```
  [str &opt base]
  (default base 10)
  (if-not (<= 2 base (length int-alphabet))
    (error "invalid base"))
  (def usable-alphabet (slice int-alphabet 0 base))
  (var value 0)
  (each char str
    (def char
      (if (<= (chr "A") char (chr "Z"))
        (+ char (- (chr "a") (chr "A")))
        char))
    (def digit (index-of char int-alphabet))
    (if (or (nil? digit) (> digit base))
      (error "malformed integer"))
    (set value (+ (* value base) digit)))
  value)

(defn int->string
  "Stringify an integer in a particular base. Defaults to decimal (base 10)."
  [int &opt base]
  (default base 10)
  (if-not (<= 2 base (length int-alphabet))
    (error "invalid base"))
  (if (not= int (math/trunc int))
    (error "number is not an integer"))
  (def buf
    (buffer/new
      (+
        (math/ceil (/ (math/log (math/abs int)) (math/log base)))
        (if (< int 0) 1 0))))
  (var int int)
  (def neg? (< int 0))
  (if neg?
    (set int (- int)))
  (while (not= int 0)
    (def digit (mod int base))
    (buffer/push buf (int-alphabet digit))
    (set int (int/ int base)))
  (if neg?
    (buffer/push buf "-"))
  (buffer/reverse buf)
  (string buf))

(defn first-index-where
  ```
  Return the index of the first element for which `f` returns true. Scans the
  data structure linearly.
  ```
  [f xs]
  (var index nil)
  (eachp [i v] xs
    (when (f v)
      (set index i)
      (break)))
  index)

(defn array/insert-sorted
  ```
  Insert elements in `arr` such that it remains sorted by the comparator. If
  `arr` is not sorted beforehand, the results are undefined. Returns `arr`.
  ```
  [arr <? & xs]
  (each x xs
    (array/insert
      arr
      (or (first-index-where (partial <? x) arr) -1)
      x))
  arr)

(defn array/insert-sorted-by
  ```
  Insert elements in `arr` such that it remains sorted by the value returned
  when `f` is called with the element, comparing the values with <. If `arr` is
  not sorted beforehand, the results are undefined. Returns `arr`.
  ```
  [arr f & xs]
  (each x xs
    (array/insert
      arr
      (or (first-index-where |(< (f x) (f $)) arr) -1)
      x))
  arr)

(def peg-grammar
  "Custom peg grammar with crlf and to end."
  (merge (dyn :peg-grammar)
         ~{:crlf "\r\n"
           :cap-to-crlf (* '(to :crlf) :crlf)
           :toe (to -1)
           :boundaries (+ :s (set ",.?!_-/|\\"))
           :split (any (+ :boundaries '(some :a)))}))

(defn setup-peg-grammar
  "Merges `peg-grammar` into `:peg-grammar` `dyn`"
  []
  (setdyn :peg-grammar peg-grammar))

(def pgp/word-list
  "Mapping from hex string < 100 to pgp words. 
  One for odd and one for even position."
  {"00" ["aardvark" "adroitness"] "01" ["absurd" "adviser"]
   "02" ["accrue" "aftermath"] "03" ["acme" "aggregate"]
   "04" ["adrift" "alkali"] "05" ["adult" "almighty"]
   "06" ["afflict" "amulet"] "07" ["ahead" "amusement"]
   "08" ["aimless" "antenna"] "09" ["Algol" "applicant"]
   "0A" ["allow" "Apollo"] "0B" ["alone" "armistice"]
   "0C" ["ammo" "article"] "0D" ["ancient" "asteroid"]
   "0E" ["apple" "Atlantic"] "0F" ["artist" "atmosphere"]
   "10" ["assume" "autopsy"] "11" ["Athens" "Babylon"]
   "12" ["atlas" "backwater"] "13" ["Aztec" "barbecue"]
   "14" ["baboon" "belowground"] "15" ["backfield" "bifocals"]
   "16" ["backward" "bodyguard"] "17" ["banjo" "bookseller"]
   "18" ["beaming" "borderline"] "19" ["bedlamp" "bottomless"]
   "1A" ["beehive" "Bradbury"] "1B" ["beeswax" "bravado"]
   "1C" ["befriend" "Brazilian"] "1D" ["Belfast" "breakaway"]
   "1E" ["berserk" "Burlington"] "1F" ["billiard" "businessman"]
   "20" ["bison" "butterfat"] "21" ["blackjack" "Camelot"]
   "22" ["blockade" "candidate"] "23" ["blowtorch" "cannonball"]
   "24" ["bluebird" "Capricorn"] "25" ["bombast" "caravan"]
   "26" ["bookshelf" "caretaker"] "27" ["brackish" "celebrate"]
   "28" ["breadline" "cellulose"] "29" ["breakup" "certify"]
   "2A" ["brickyard" "chambermaid"] "2B" ["briefcase" "Cherokee"]
   "2C" ["Burbank" "Chicago"] "2D" ["button" "clergyman"]
   "2E" ["buzzard" "coherence"] "2F" ["cement" "combustion"]
   "30" ["chairlift" "commando"] "31" ["chatter" "company"]
   "32" ["checkup" "component"] "33" ["chisel" "concurrent"]
   "34" ["choking" "confidence"] "35" ["chopper" "conformist"]
   "36" ["Christmas" "congregate"] "37" ["clamshell" "consensus"]
   "38" ["classic" "consulting"] "39" ["classroom" "corporate"]
   "3A" ["cleanup" "corrosion"] "3B" ["clockwork" "councilman"]
   "3C" ["cobra" "crossover"] "3D" ["commence" "crucifix"]
   "3E" ["concert" "cumbersome"] "3F" ["cowbell" "customer"]
   "40" ["crackdown" "Dakota"] "41" ["cranky" "decadence"]
   "42" ["crowfoot" "December"] "43" ["crucial" "decimal"]
   "44" ["crumpled" "designing"] "45" ["crusade" "detector"]
   "46" ["cubic" "detergent"] "47" ["dashboard" "determine"]
   "48" ["deadbolt" "dictator"] "49" ["deckhand" "dinosaur"]
   "4A" ["dogsled" "direction"] "4B" ["dragnet" "disable"]
   "4C" ["drainage" "disbelief"] "4D" ["dreadful" "disruptive"]
   "4E" ["drifter" "distortion"] "4F" ["dropper" "document"]
   "50" ["drumbeat" "embezzle"] "51" ["drunken" "enchanting"]
   "52" ["Dupont" "enrollment"] "53" ["dwelling" "enterprise"]
   "54" ["eating" "equation"] "55" ["edict" "equipment"]
   "56" ["egghead" "escapade"] "57" ["eightball" "Eskimo"]
   "58" ["endorse" "everyday"] "59" ["endow" "examine"]
   "5A" ["enlist" "existence"] "5B" ["erase" "exodus"]
   "5C" ["escape" "fascinate"] "5D" ["exceed" "filament"]
   "5E" ["eyeglass" "finicky"] "5F" ["eyetooth" "forever"]
   "60" ["facial" "fortitude"] "61" ["fallout" "frequency"]
   "62" ["flagpole" "gadgetry"] "63" ["flatfoot" "Galveston"]
   "64" ["flytrap" "getaway"] "65" ["fracture" "glossary"]
   "66" ["framework" "gossamer"] "67" ["freedom" "graduate"]
   "68" ["frighten" "gravity"] "69" ["gazelle" "guitarist"]
   "6A" ["Geiger" "hamburger"] "6B" ["glitter" "Hamilton"]
   "6C" ["glucose" "handiwork"] "6D" ["goggles" "hazardous"]
   "6E" ["goldfish" "headwaters"] "6F" ["gremlin" "hemisphere"]
   "70" ["guidance" "hesitate"] "71" ["hamlet" "hideaway"]
   "72" ["highchair" "holiness"] "73" ["hockey" "hurricane"]
   "74" ["indoors" "hydraulic"] "75" ["indulge" "impartial"]
   "76" ["inverse" "impetus"] "77" ["involve" "inception"]
   "78" ["island" "indigo"] "79" ["jawbone" "inertia"]
   "7A" ["keyboard" "infancy"] "7B" ["kickoff" "inferno"]
   "7C" ["kiwi" "informant"] "7D" ["klaxon" "insincere"]
   "7E" ["locale" "insurgent"] "7F" ["lockup" "integrate"]
   "80" ["merit" "intention"] "81" ["minnow" "inventive"]
   "82" ["miser" "Istanbul"] "83" ["Mohawk" "Jamaica"]
   "84" ["mural" "Jupiter"] "85" ["music" "leprosy"]
   "86" ["necklace" "letterhead"] "87" ["Neptune" "liberty"]
   "88" ["newborn" "maritime"] "89" ["nightbird" "matchmaker"]
   "8A" ["Oakland" "maverick"] "8B" ["obtuse" "Medusa"]
   "8C" ["offload" "megaton"] "8D" ["optic" "microscope"]
   "8E" ["orca" "microwave"] "8F" ["payday" "midsummer"]
   "90" ["peachy" "millionaire"] "91" ["pheasant" "miracle"]
   "92" ["physique" "misnomer"] "93" ["playhouse" "molasses"]
   "94" ["Pluto" "molecule"] "95" ["preclude" "Montana"]
   "96" ["prefer" "monument"] "97" ["preshrunk" "mosquito"]
   "98" ["printer" "narrative"] "99" ["prowler" "nebula"]
   "9A" ["pupil" "newsletter"] "9B" ["puppy" "Norwegian"]
   "9C" ["python" "October"] "9D" ["quadrant" "Ohio"]
   "9E" ["quiver" "onlooker"] "9F" ["quota" "opulent"]
   "A0" ["ragtime" "Orlando"] "A1" ["ratchet" "outfielder"]
   "A2" ["rebirth" "Pacific"] "A3" ["reform" "pandemic"]
   "A4" ["regain" "Pandora"] "A5" ["reindeer" "paperweight"]
   "A6" ["rematch" "paragon"] "A7" ["repay" "paragraph"]
   "A8" ["retouch" "paramount"] "A9" ["revenge" "passenger"]
   "AA" ["reward" "pedigree"] "AB" ["rhythm" "Pegasus"]
   "AC" ["ribcage" "penetrate"] "AD" ["ringbolt" "perceptive"]
   "AE" ["robust" "performance"] "AF" ["rocker" "pharmacy"]
   "B0" ["ruffled" "phonetic"] "B1" ["sailboat" "photograph"]
   "B2" ["sawdust" "pioneer"] "B3" ["scallion" "pocketful"]
   "B4" ["scenic" "politeness"] "B5" ["scorecard" "positive"]
   "B6" ["Scotland" "potato"] "B7" ["seabird" "processor"]
   "B8" ["select" "provincial"] "B9" ["sentence" "proximate"]
   "BA" ["shadow" "puberty"] "BB" ["shamrock" "publisher"]
   "BC" ["showgirl" "pyramid"] "BD" ["skullcap" "quantity"]
   "BE" ["skydive" "racketeer"] "BF" ["slingshot" "rebellion"]
   "C0" ["slowdown" "recipe"] "C1" ["snapline" "recover"]
   "C2" ["snapshot" "repellent"] "C3" ["snowcap" "replica"]
   "C4" ["snowslide" "reproduce"] "C5" ["solo" "resistor"]
   "C6" ["southward" "responsive"] "C7" ["soybean" "retraction"]
   "C8" ["spaniel" "retrieval"] "C9" ["spearhead" "retrospect"]
   "CA" ["spellbind" "revenue"] "CB" ["spheroid" "revival"]
   "CC" ["spigot" "revolver"] "CD" ["spindle" "sandalwood"]
   "CE" ["spyglass" "sardonic"] "CF" ["stagehand" "Saturday"]
   "D0" ["stagnate" "savagery"] "D1" ["stairway" "scavenger"]
   "D2" ["standard" "sensation"] "D3" ["stapler" "sociable"]
   "D4" ["steamship" "souvenir"] "D5" ["sterling" "specialist"]
   "D6" ["stockman" "speculate"] "D7" ["stopwatch" "stethoscope"]
   "D8" ["stormy" "stupendous"] "D9" ["sugar" "supportive"]
   "DA" ["surmount" "surrender"] "DB" ["suspense" "suspicious"]
   "DC" ["sweatband" "sympathy"] "DD" ["swelter" "tambourine"]
   "DE" ["tactics" "telephone"] "DF" ["talon" "therapist"]
   "E0" ["tapeworm" "tobacco"] "E1" ["tempest" "tolerance"]
   "E2" ["tiger" "tomorrow"] "E3" ["tissue" "torpedo"]
   "E4" ["tonic" "tradition"] "E5" ["topmost" "travesty"]
   "E6" ["tracker" "trombonist"] "E7" ["transit" "truncated"]
   "E8" ["trauma" "typewriter"] "E9" ["treadmill" "ultimate"]
   "EA" ["Trojan" "undaunted"] "EB" ["trouble" "underfoot"]
   "EC" ["tumor" "unicorn"] "ED" ["tunnel" "unify"]
   "EE" ["tycoon" "universe"] "EF" ["uncut" "unravel"]
   "F0" ["unearth" "upcoming"] "F1" ["unwind" "vacancy"]
   "F2" ["uproot" "vagabond"] "F3" ["upset" "vertigo"]
   "F4" ["upshot" "Virginia"] "F5" ["vapor" "visitor"]
   "F6" ["village" "vocalist"] "F7" ["virus" "voyager"]
   "F8" ["Vulcan" "warranty"] "F9" ["waffle" "Waterloo"]
   "FA" ["wallet" "whimsical"] "FB" ["watchword" "Wichita"]
   "FC" ["wayside" "Wilmington"] "FD" ["willow" "Wyoming"]
   "FE" ["woodlark" "yesteryear"] "FF" ["Zulu" "Yucatan"]})

(defn pgp/hex->word
  "Returns pgp word for hex string <100"
  [hex position] (get-in pgp/word-list [hex (% position 2)]))

(defn pgp/hexs->words
  ```
  Returns an array of pgp words for arbitrary long string of hexs.
  Sanitizes out the white space from hex-string.
  ```
  [hex-string]
  (def hexs
    (peg/match ~(some (+ :s (cmt '(* :h :h) ,string/ascii-upper)
                         -1 (error (constant "bad hex"))))
               hex-string))
  (seq [[i hex] :pairs hexs]
    (if-let [ghex (pgp/hex->word hex i)]
      ghex (error (string "wrong hex " hex)))))

(defn pgp/word->hex
  "Returns a hex number as string for the pgp word."
  [word]
  (do-var
    ret nil
    (loop [[h ws] :pairs pgp/word-list
           :when (index-of word ws)]
      (set ret h)
      (break))))

(defn pgp/words->hexs
  "Returns an array of hexs from the string with pgp words."
  [words-string]
  (seq [w :in (peg/match :split words-string)]
    (if-let [gw (pgp/word->hex w)]
      gw (error (string "unknown pgp word " w)))))
