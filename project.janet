(declare-project
  :name "spork"
  :description "Official contrib library of various Janet utility modules."
  :author "Calvin Rose"
  :license "MIT"
  :dependencies []
  :version "1.2.0"
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
  :hardcode-syspath :dynamic
  :is-janet true)

(declare-binscript
  :main "bin/janet-pm"
  :hardcode-syspath :dynamic # allow for JANET_PATH=new_module_tree
  :is-janet true)

# Manual pages

(declare-manpage "man/janet-pm.1")

# Natives

(def win-libs
  (if (= (os/which) :mingw)
    ["-ljanet" "-lws2_32" "-lmswsock" "-lpsapi" "-ladvapi32"]
    []))

(declare-native
  :name "spork/json"
  :ldflags (distinct @[;default-ldflags ;win-libs])
  :source @["src/json.c"])

(declare-native
  :name "spork/rawterm"
  :ldflags (distinct @[;default-ldflags ;win-libs])
  :source @["src/rawterm.c"])

(declare-native
  :name "spork/crc"
  :ldflags (distinct @[;default-ldflags ;win-libs])
  :source @["src/crc.c"])

(declare-native
  :name "spork/utf8"
  :ldflags (distinct @[;default-ldflags ;win-libs])
  :source @["src/utf8.c"])

(declare-native
 :name "spork/tarray"
 :ldflags (distinct @[;default-ldflags ;win-libs])
 :headers @["src/tarray.h"]
 :source @["src/tarray.c"])

(declare-headers
 :headers ["src/tarray.h"])

(declare-native
  :name "spork/zip"
  :source @["src/zip.c" "deps/miniz/miniz.c"]
  :ldflags (distinct @[;default-ldflags ;win-libs])
  :defines @{"_LARGEFILE64_SOURCE" true}
  :headers @["deps/miniz/miniz.h"])

(declare-native
  :name "spork/cmath"
  :ldflags (distinct @[;default-ldflags ;win-libs])
  :source @["src/cmath.c"])

(declare-native
  :name "spork/base64"
  :ldflags (distinct @[;default-ldflags ;win-libs])
  :source @["src/base64.c"])

(declare-native
  :name "spork/gfx2d"
  :source @["spork/gfx2d-codegen.janet" "src/stb.janet"]
  :deps @["deps/default_font.h" "deps/tall_font.h" "deps/olive_font.h"]
  :ldflags (distinct @[;default-ldflags "-lm" ;win-libs])
  :cflags @[;default-cflags "-Ideps/stb" "-Ideps" "-Wall"])
