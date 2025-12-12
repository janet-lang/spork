(declare-project
  :name "spork"
  :description "Official contrib library of various Janet utility modules."
  :author "Calvin Rose"
  :license "MIT"
  :dependencies []
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

(declare-binscript
  :main "bin/janet-pm"
  :hardcode-syspath :dynamic # allow for JANET_PATH=new_module_tree
  :is-janet true)

# Manual pages

(declare-manpage "man/janet-pm.1")

# Natives

(declare-native
  :name "spork/json"
  :source @["src/json.c"])

(declare-native
  :name "spork/rawterm"
  :source @["src/rawterm.c"])

(declare-native
  :name "spork/crc"
  :source @["src/crc.c"])

(declare-native
  :name "spork/utf8"
  :source @["src/utf8.c"])

(declare-native
 :name "spork/tarray"
 :headers @["src/tarray.h"]
 :source @["src/tarray.c"])

(declare-headers
 :headers ["src/tarray.h"])

(declare-native
  :name "spork/zip"
  :source @["src/zip.c" "deps/miniz/miniz.c"]
  :defines @{"_LARGEFILE64_SOURCE" true}
  :headers @["deps/miniz/miniz.h"])

(declare-native
  :name "spork/cmath"
  :source @["src/cmath.c"])

(declare-native
  :name "spork/base64"
  :source @["src/base64.c"])

(declare-native
  :name "spork/gfx2d"
  :source @["src/gfx2d.janet" "src/stb.janet"]
  :cflags @[;default-cflags "-Ideps/stb"])
