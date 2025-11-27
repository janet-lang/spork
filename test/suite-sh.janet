(use ../spork/test)
(import ../spork/sh)
(import ../spork/path)

(start-suite)

(def base-path "test")
(def assets-dir ["assets" "17"])

(do
  (assert (deep= (os/dir (path/join base-path ;assets-dir))
                 @["test.file"])
          "test files are wrong, assets/17 should only contain test.file")
  (sh/copy-file (path/join base-path "assets/17/test.file")
                (path/join base-path "assets/17/test2.file"))
  (def new_file (slurp (path/join base-path "assets/17/test2.file")))
  (assert (deep= (sort (sh/list-all-files (path/join base-path "assets")))
                 (map |(path/join base-path $0)
                      @["assets/17/test.file" "assets/17/test2.file"]))
          "sh/list-all-files didn't list the correct files")
  (sh/rm (path/join base-path "assets/17/test2.file"))
  (assert (deep= (os/dir (path/join base-path "assets/17"))
                 @["test.file"])
          "file test2.file was not removed by sh/rm")
  (assert (deep= (slurp (path/join base-path "assets/17/test.file"))
                 new_file)
          "file copied with sh/copy-file is not the same"))

(do
  (sh/create-dirs (path/join base-path "assets/17/some/more/directories/to/test"))
  (assert (= ((os/stat (path/join base-path "assets/17/some/more/directories/to/test")) :mode)
             :directory)
          "sh/create-dirs failed")
  (sh/rm (path/join base-path "assets/17/some"))
  (assert (= (os/stat (path/join base-path "assets/17/some/more/directories/to/test"))
             nil)
          "sh/rm didn't work correctly"))

(assert (deep=
          (sh/split ` "c d \" f" ' y z'  a b a\ b --cflags `)
          @["c d \" f" " y z" "a" "b" "a b" "--cflags"]))

(def new-dir ["assets" "18"])
(sh/create-dirs (path/join base-path ;new-dir))
(defer (do
         (os/rm (path/join base-path ;new-dir "test.file"))
         (os/rmdir (path/join base-path ;new-dir)))
  (sh/copy (path/join base-path ;assets-dir "test.file")
           (path/join base-path ;new-dir))
  (assert (= :file (os/stat (path/join base-path ;new-dir "test.file") :mode))
          "directory with file should be copied"))

(end-suite)
