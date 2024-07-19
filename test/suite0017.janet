(use ../spork/test)
(import ../spork/sh)
(import ../spork/path)

(start-suite 17)

(def base-path "test")

(do
  (assert (deep= (os/dir (path/join base-path "assets/17"))
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

(do
  (def win32-env
    {"TMP" (or (os/getenv "TMP") :nil)
     "TEMP" (or (os/getenv "TEMP") :nil)
     "USERPROFILE" (or (os/getenv "USERPROFILE") :nil)
     "WINDIR" (or (os/getenv "WINDIR") :nil)})
  (defn reset []
    (eachk name win32-env (os/setenv name nil)))
  (defn restore []
    (eachk name win32-env
      (def old-val (get win32-env name))
      (os/setenv name (if (= :nil old-val) nil old-val))))
  (defer (restore)
    (reset)
    (os/setenv "TMP" `C:\TEMP`)
    (assert (= `C:\TEMP` (sh/windows-temp-root))
            "TMP env var value for temp dir for windows")
    (reset)
    (os/setenv "TEMP" `C:\TEMP2`)
    (assert (= `C:\TEMP2` (sh/windows-temp-root))
            "TEMP env var value for temp dir for windows")
    (reset)
    (os/setenv "USERPROFILE" `C:\TEMPU`)
    (assert (= `C:\TEMPU` (sh/windows-temp-root))
            "USERPROFILE env var value for temp dir for windows")
    (reset)
    (os/setenv "WINDIR" `C:\WINDOWS`)
    (assert (= `C:\WINDOWS` (sh/windows-temp-root))
            "WINDIR env var value for temp dir for windows")))

(do
  (def posix-env
    {"TMPDIR" (or (os/getenv "TMPDIR") :nil)
     "TMP" (or (os/getenv "TMP") :nil)
     "TEMP" (or (os/getenv "TEMP") :nil)
     "TEMPDIR" (or (os/getenv "TEMPDIR") :nil)})
  (defn reset []
    (eachk name posix-env (os/setenv name nil)))
  (defn restore []
    (eachk name posix-env
      (def old-val (get posix-env name))
      (os/setenv name (if (= :nil old-val) nil old-val))))
  (defer (restore)
    (reset)
    (os/setenv "TMPDIR" `/tmp`)
    (assert (= `/tmp` (sh/posix-temp-root))
            "TMPDIR env var value for temp dir for posix")
    (reset)
    (os/setenv "TMP" `/var/tmp`)
    (assert (= `/var/tmp` (sh/posix-temp-root))
            "TMP env var value for temp dir for posix")
    (reset)
    (os/setenv "TEMP" `/private/tmp`)
    (assert (= `/private/tmp` (sh/posix-temp-root))
            "TEMP env var value for temp dir for posix")
    (reset)
    (os/setenv "TEMPDIR" `/tmp`)
    (assert (= `/tmp` (sh/posix-temp-root))
            "TEMPDIR env var value for temp dir for posix")))

(do
  (def tmp-root (sh/temp-root))
  (def unlikely-prefix "ReaLLyNotTooLikelYIHope")
  (def n 3) # could vary this each time...
  (def n-slashes (string/repeat "/" n))
  (def a-path
    (sh/make-temp-dir (string unlikely-prefix "-" n-slashes)))
  (assert (= :directory (os/stat a-path :mode))
          (string/format "temp dir created at: %s" a-path))
  (assert (path/abspath? a-path)
          "temp-dir path is an absolute path")
  (assert (string/has-prefix? tmp-root a-path)
          "temp-dir path starts with temp-root path")
  (assert (has-value? (os/dir tmp-root) (path/basename a-path))
          "temp dir created under temp-root")
  (os/rmdir a-path)
  (assert (peg/match ~(sequence (thru ,unlikely-prefix)
                                "-"
                                (repeat ,n :h))
                     a-path)
          "temp dir name matches template pattern"))

(do
  (def tmp-root (sh/temp-root))
  (def unlikely-prefix "ReaLLyNotTooLikelYIHope-")
  # pre-create 16 subdirs that would match a template
  (loop [i :range [0 16]]
    (def subdir-path
      (path/join tmp-root
                 (string unlikely-prefix (string/format "%x" i))))
    (os/mkdir subdir-path))
  # all subdirs that would match the template exist already
  (assert-error "temp dir creation can fail"
                (sh/make-temp-dir (string unlikely-prefix "/")))
  # remove the test subdirs
  (loop [i :range [0 16]]
    (def subdir-path
      (path/join tmp-root
                 (string unlikely-prefix (string/format "%x" i))))
    (os/rmdir subdir-path)))

(end-suite)
