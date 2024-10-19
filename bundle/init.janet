(import ./test)
(import ./build :as b)
(import ./clean :as c)

(defn clean [&]
  (c/main))

(defn check [&]
  (test/main))

(defn build [&]
  (b/main))

(defn install [m &]
  (bundle/add-file m "src/tarray.h" "tarray.h")
  (bundle/add m "spork")
  (compwhen (dyn 'bundle/add-bin)
    (bundle/add-bin m "bin/janet-format")
    (bundle/add-bin m "bin/janet-netrepl"))
  (each file (os/dir "build/spork")
    (def f (string "build/spork/" file))
    (when (= (os/stat f :mode) :file)
      (bundle/add-file m f (string "spork/" file)))))
