### argparse.janet
###
### A library for parsing command-line arguments
###
### Copyright 2021 © Calvin Rose and contributors

(defn- pad-right
  "Pad a string on the right with some spaces."
  [str n]
  (def len (length str))
  (if (>= len n)
    str
    (string str (string/repeat " " (- n len)))))

(defn argparse
  ```
  Parse `(dyn :args)` according to options. If the arguments are incorrect,
  will return nil and print usage information.

  Each option is a table or struct that specifies a flag or option
  for the script. The name of the option should be a string, specified
  via `(argparse/argparse \"...\" op1-name {...} op2-name {...} ...)`. A help option
  and usage text is automatically generated for you.

  The keys in each option table are as follows:

  * `:kind` - What kind of option is this? One of `:flag`, `:multi`, `:option` `:accumulate`,
    or `:subcommand`. A flag can either be on or off, a multi is a flag that can be provided
    multiple times, each time adding 1 to a returned integer, an option is a key that
    will be set in the returned table, and accumulate means an option can be specified
    0 or more times, each time appending a value to an array.
  * `:short` - Single letter for shorthand access.
  * `:help` - Help text for the option, explaining what it is.
  * `:default` - Default value for the option.
  * `:required` - Whether or not an option is required.
  * `:short-circuit` - Whether or not to stop parsing and fail if this option is hit.
  * `:action` - A function that will be invoked when the option is parsed.
  * `:args-expected` - For subcommands, the max number of positional arguments.
  * `:args-required` - For subcommands, whether or not to require `args-expected` positional arguments.
  
  There is also a special option name `:default` that will be invoked on arguments
  that do not start with a -- or -. Use this option to collect unnamed
  arguments to your script. This is separate from the `:default` key in option specifiers.

  After "--", every argument is treated as an unnamed argument.

  Once parsed, values are accessible in the returned table by the name
  of the option. For example `(result \"verbose\")` will check if the verbose
  flag is enabled.
  ```
  [description &keys options]

  # Add default help option
  (def options (merge
                 @{"help" {:kind :flag
                           :short "h"
                           :help "Show this help message."
                           :action :help
                           :short-circuit true}}
                 options))

  # Create shortcodes
  (def shortcodes @{})
  (loop [[k v] :pairs options :when (string? k)]
    (if-let [code (v :short)]
      (put shortcodes (code 0) {:name k :handler v})))

  # Results table and other things 
  (def res @{:order @[] :subcommands @[]})
  (def args (dyn :args))
  (def arglen (length args))
  (var scanning true)
  (var bad false)
  (var i 1)
  (var process-options? true)

  # Show usage
  (defn usage
    [& msg]
    # Only show usage once.
    (if bad (break))
    (set bad true)
    (set scanning false)
    (unless (empty? msg)
      (print "usage error: " ;msg) 
      (print))
    (def subcommands @"")
    (def flags @"")
    (def opdoc @"")
    (def reqdoc @"")
    (loop [[name handler] :in (sort (pairs options))]
      (def short (handler :short))
      (when short (buffer/push-string flags short))
      (when (string? name)
        (let [kind (handler :kind)
              usage-prefix (string
                            ;(if short [" -" short ", "] ["     "])
                            (if-not (= kind :subcommand) "--" "") name
                            ;(if (index-of kind [:option :accumulate])
                              [" " (or (string 
                                        "[" (handler :value-name) "]") 
                                       "[VALUE]")
                                ;(if-let [d (handler :default)]
                                  ["=" d]
                                  [])]
                              []))
              usage-fragment (string
                              (pad-right (string usage-prefix " ") 40)
                              (if-let [h (handler :help)] h "")
                              "\n")] 
            (if (= kind :subcommand)
              (buffer/push-string subcommands usage-fragment)
              (buffer/push-string
               (if (handler :required)
                 reqdoc opdoc)
               usage-fragment)))))
    (print "usage: " (get args 0) " " (unless (empty? subcommands) "[subcommand] {positional arguments}") " [options] ... ") 
    (print)       
    (print description)
    (print)
    (unless (empty? subcommands) 
            (print " Subcommands:")
            (print subcommands))
    (unless (empty? reqdoc)
      (print " Required:")
      (print reqdoc))
    (unless (empty? opdoc)
      (print " Optional:")
      (print opdoc)))

  # Handle an option
  (defn handle-option
    [name handler]
    (array/push (res :order) name)
    (case (handler :kind)
      :flag (put res name true)
      :multi (do
               (var count (or (get res name) 0))
               (++ count)
               (put res name count))
      :option (if-let [arg (get args i)]
                (do
                  (put res name arg)
                  (++ i))
                (usage "missing argument for " name))
      :accumulate (if-let [arg (get args i)]
                    (do
                      (def arr (or (get res name) @[]))
                      (array/push arr arg)
                      (++ i)
                      (put res name arr))
                    (usage "missing argument for " name)) 
      :subcommand (do (put res name @[])
                      (array/push (res :subcommands) name))
      #default
       (usage "unknown option kind: " (handler :kind)))

    # Allow actions to be dispatched while scanning
    (when-let [action (handler :action)]
              (cond
                (= action :help) (usage)
                (function? action) (action)))

    # Early exit for things like help
    (when (handler :short-circuit)
      (set scanning false)))

  # Store last subcommand 
  (var last-subcommand nil)
  
  # Iterate command line arguments and parse them
  # into the run table.
  (while (and scanning (< i arglen))
    (def arg (get args i))
    (cond
      # `--` turns off option processing so that
      # the rest of arguments are treated like unnamed arguments.
      (and (= "--" arg) process-options?)
      (do
        (set process-options? false)
        (++ i))

      # long name (--name)
      (and (string/has-prefix? "--" arg) process-options?)
      (let [name (string/slice arg 2)
            handler (get options name)]
        (++ i)
        (if handler
          (handle-option name handler)
          (usage "unknown option " name)))

      # short names (-flags)
      (and (string/has-prefix? "-" arg) process-options?)
      (let [flags (string/slice arg 1)]
        (++ i)
        (each flag flags
          (if-let [x (get shortcodes flag)]
            (let [{:name name :handler handler} x]
              (handle-option name handler))
            (usage "unknown flag " arg))))
      
      # subcommands
      (and process-options? (index-of arg (keys options))) 
      (let [handler (get options arg)]
        (++ i)
        (when handler
          (set last-subcommand arg)
          (handle-option arg handler)))

      (and process-options? last-subcommand)
      (do (++ i)
          (let [handler (get options last-subcommand)
                old-posarg-array (in res last-subcommand)
                new-posarg-array (array/push old-posarg-array arg)]
            (if-let [num (handler :args-expected)]
              (if (<= (length (res last-subcommand)) num)
                (put res last-subcommand new-posarg-array)
                (usage "unexpected positional argument: " arg))
              (put res last-subcommand new-posarg-array))))
      
      # default
      (if-let [handler (options :default)]
        (handle-option :default handler)
        (usage "could not handle option " arg))))

  # Handle defaults, required options
  (loop [[name handler] :pairs options]
    (when (nil? (res name))
      (when (handler :required)
        (usage "option " name " is required"))
      (put res name (handler :default)))
    (when (and (handler :args-expected) (handler :args-required) (get res name))
      (when (not= (handler :args-expected) 
                  (length (get res name)))
        (usage "missing positional arguments for subcommand " name))))

  # Remove unused structures from results  
  (when (empty? (res :subcommands))
    (put res :subcommands nil))
  
  (if-not bad res))
