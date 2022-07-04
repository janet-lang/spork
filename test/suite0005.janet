(use ../spork/test)
(import ../spork/argparse)

(start-suite 5)

(def argparse-params
  ["A simple CLI tool. An example to show the capabilities of argparse."
   "debug" {:kind :flag
            :short "d"
            :help "Set debug mode."}
   "verbose" {:kind :multi
              :short "v"
              :help "Print debug information to stdout."}
   "key" {:kind :option
          :short "k"
          :help "An API key for getting stuff from a server."
          :required true}
   "expr" {:kind :accumulate
           :short "e"
           :help "Search for all patterns given."}
   "thing" {:kind :option
            :help "Some option?"
            :default "123"}
   "subcommand" {:kind :subcommand
                 :help "A subcommand with up to two optional positional arguments."
                 :args-expected 2
                 :args-required false}
   "strict-sc" {:kind :subcommand
                 :help "A subcommand with one required positional argument."
                 :args-expected 1
                 :args-required true}])

(with-dyns [:args @["testcase.janet" "-k" "100"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (when (res "debug") (error (string "bad debug: " (res "debug"))))
  (when (res "verbose") (error (string "bad verbose: " (res "verbose"))))
  (assert (= (res "key") "100") (string "bad key: " (res "key")))
  (assert-not (res "expr") (string "bad expr: " (res "expr")))
  (assert (= (res "thing") "123") (string "bad thing: " (res "thing"))))

(with-dyns [:args @["testcase.janet" "-k" "100" "--thing"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert-not res "Option \"thing\" missing arg, but result is non-nil."))

(with-dyns [:args @["testcase.janet" "-k" "100" "-e" "foo" "-e"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert-not res "Option \"expr\" missing arg, but result is non-nil."))

(with-dyns [:args @["testcase.janet" "-k" "100" "-v" "--thing" "456" "-d" "-v"
                    "-e" "abc" "-vvv" "-e" "def"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert (res "debug") (string "bad debug: " (res "debug")))
  (assert (= (res "verbose") 5) (string "bad verbose: " (res "verbose")))
  (assert (= (tuple ;(res "expr")) ["abc" "def"])
          (string "bad expr: " (string/join (res "expr") " ")))
  (assert (= (res "thing") "456") (string "bad thing: " (res "thing")))
  (assert (= (tuple ;(res :order))
             ["key" "verbose" "thing" "debug" "verbose"
              "expr" "verbose" "verbose" "verbose" "expr"])
          (string "bad order: " (string/join (res :order) " "))))

(with-dyns [:args @["testcase.janet" "server"]]
  (def res (suppress-stdout (argparse/argparse
                              "A simple CLI tool."
                              :default {:kind :option})))
  (assert (= (res :default) "server")
          (string "bad default " (res :default))))

(with-dyns [:args @["testcase.janet" "server" "run"]]
  (def res (suppress-stdout (argparse/argparse
                              "A simple CLI tool."
                              :default {:kind :accumulate})))
  (assert (and (deep= (res :default) @["server" "run"]))
          (string "bad default " (res :default))))

(with-dyns [:args @["testcase.janet" "-k" "100" "--fake"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert-not res "Option \"fake\" is not valid, but result is non-nil."))

(with-dyns [:args @["testcase.janet" "-l" "100" "--" "echo" "-n" "ok" "subcommand"]]
  (def res (suppress-stdout (argparse/argparse "A simple CLI tool"
                                               "length" {:kind :option
                                                         :short "l"
                                                         :help "key"}
                                               :default {:kind :accumulate})))
  (assert res "arguments were not parsed correctly in the presence of `--`.")
  (def {"length" len :default cmd-args} res)
  (assert (= len "100")
          "option was not parsed correctly in the presence of `--`.")
  (assert (= ["echo" "-n" "ok" "subcommand"] (tuple ;cmd-args))
          "unnamed arguments after `--` were not parsed correctly.")) 

(with-dyns [:args @["testcase.janet" "-k" "100" "subcommand" "optional-param1" "optional-param2"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert res "arguments were not parsed correctly in the presence of `subcommand`.")
  (def {"subcommand" opt-params-array :subcommands subcommand-list} res)
  (assert (and opt-params-array (= 2 (length opt-params-array))) 
          "optional parameters were not parsed correctly.")
  (assert (and subcommand-list (= 1 (length subcommand-list)))
          "subcommand was not saved to `:subcommand` array correctly.")) 
                            
(with-dyns [:args @["testcase.janet" "subcommand" "optional-param1" "-k" "100"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert res "arguments were not parsed correctly with subcommand before flag.")
  (def {"subcommand" opt-params-array :subcommands subcommand-list} res)
  (assert (and opt-params-array (= 1 (length opt-params-array))) 
          "optional parameter was not parsed correctly with subcommand before flag.")
  (assert (and subcommand-list (= 1 (length subcommand-list)))
          "subcommand was not saved to `:subcommand` array correctly with subcommand before flag."))
                            
(with-dyns [:args @["testcase.janet" "subcommand" "optional-param1" "optional-param2" "error-param" "-k" "100"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert-not res "subcommand permits only 2 optional parameters, but result is non-nil."))
                            
(with-dyns [:args @["testcase.janet" "strict" "required-param1" "error-param" "-k" "100"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert-not res "subcommand requires exactly 1 parameters, but result is non-nil."))
                            
(with-dyns [:args @["testcase.janet" "-k" "100" "strict-sc" "required-param1"]]
  (def res (suppress-stdout (argparse/argparse ;argparse-params)))
  (assert res "required argument was not parsed correctly with subcommand after flag."))
                            
(end-suite)
