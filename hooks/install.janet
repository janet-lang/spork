(bundle/add-directory "spork")
(bundle/add-file "src/tarray.h" "tarray.h")
(each file (os/dir "spork")
  (bundle/add-file (string "spork/" file)))

(each file (os/dir "build")
  (def f (string "build/" file))
  (when (= (os/stat f :mode) :file)
    (bundle/add-file f (string "spork/" file))))
