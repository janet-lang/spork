(declare-project
  :name "spork"
  :description "Official contrib library of various Janet utility modules."
  :author "Calvin Rose"
  :license "MIT"
  :url "https://github.com/janet-lang/spork"
  :repo "git+https://github.com/janet-lang/spork")

(declare-source
  :source @["spork"])

# Scripts

(declare-binscript
  :main "bin/janet-format"
  :hardcode-syspath true
  :is-janet true)

(declare-binscript
  :main "bin/janet-netrepl"
  :hardcode-syspath true
  :is-janet true)

# Natives

(declare-native
  :name "spork/json"
  :source @["src/json.c"])

(declare-native
  :name "spork/rawterm"
  :source @["src/rawterm.c"])

(declare-native
  :name "spork/utf8"
  :source @["src/utf8.c"])

(declare-native
 :name "spork/tarray"
 :headers @["src/tarray.h"]
 :source @["src/tarray.c"])

(declare-headers
 :headers ["src/tarray.h"])

