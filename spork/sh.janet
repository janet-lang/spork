###
### Shell utilties for Janet.
### sh.janet
###

(import ./path)

(defn devnull
  "get the /dev/null equivalent of the current platform as an open file"
  []
  (os/open (if (= :windows (os/which)) "NUL" "/dev/null") :rw))

(defn exec
  "Execute command specified by args returning its exit code"
  [& args]
  (os/execute args :p))

(defn exec-fail
  "Execute command specified by args, fails when command exits with non-zero exit code"
  [& args]
  (os/execute args :px))

(defn exec-slurp
  ```
  It executes args with `os/spawn` and throws an error if the process returns with non-zero exit code. If the process
  exits with zero exit code, this function trims standard output of the process and returns it. Before the function
  finishes, the spawned process is closed for resource control.
  ```
  [& args]
  # Close the process pipes. If the process pipes are not closed, janet can run out of file descriptors.
  (with [proc (os/spawn args :xp {:out :pipe})]
    (let [[out] (ev/gather
                  (ev/read (proc :out) :all)
                  (os/proc-wait proc))]
      (if out (string/trimr out) ""))))

(defn exec-slurp-all
  ```
  It executes args with `os/spawn` and returns a struct which has the following keys.

  * `:out` - trimmed standard output of the process
  * `:err` - trimmed standard error of the process
  * `:status` - the exit code of the process

  Before the function finishes, the spawned process is closed for resource control.
  ```
  [& args]
  # Close the process pipes. If the process pipes are not closed, janet can run out of file descriptors.
  (with [proc (os/spawn args :p {:out :pipe :err :pipe})]
    (let [[out err status]
          (ev/gather
            (ev/read (proc :out) :all)
            (ev/read (proc :err) :all)
            (os/proc-wait proc))]
      {:out (if out (string/trimr out) "")
       :err (if err (string/trimr err) "")
       :status status})))

(defn rm
  "Remove a directory and all sub directories recursively."
  [path]
  (case (os/lstat path :mode)
    :directory (do
                 (each subpath (os/dir path)
                   (rm (path/join path subpath)))
                 (os/rmdir path))
    nil nil # do nothing if file does not exist
    # Default, try to remove
    (os/rm path)))

(defn rm-readonly
  "Like sh/rm, but will also remove readonly files and folders on windows"
  [path]
  (def os (os/which))
  (when (or (= os :windows) (= os :mingw))
    (def root
      (if (= :file (os/stat path :mode))
        path
        (path/join path "*.*")))
    (exec "attrib" "-R" "-H" root "/S" "/D"))
  (rm path))

(defn exists?
  "Check if the given file or directory exists. (Follows symlinks)"
  [path]
  (not= nil (os/stat path)))

(defn scan-directory
  "Scan a directory recursively, applying the given function on all files and
  directories in a depth-first manner. This function has no effect if the
  directory does not exist."
  [dir func]
  (each name (try (os/dir dir) ([_] @[]))
    (def fullpath (path/join dir name))
    (case (os/stat fullpath :mode)
      :file (func fullpath)
      :directory (do
                   (scan-directory fullpath func)
                   (func fullpath)))))

(defn list-all-files
  "List the files in the given directory recursively. Return the paths to all
  files found, relative to the current working directory if the given path is a
  relative path, or as an absolute path otherwise."
  [dir &opt into]
  (default into @[])
  (each name (try (os/dir dir) ([_] @[]))
    (def fullpath (path/join dir name))
    (case (os/stat fullpath :mode)
      :file (array/push into fullpath)
      :directory (list-all-files fullpath into)))
  into)

(defn create-dirs
  "Create all directories in path specified as string including itself."
  [dir-path]
  (def dirs @[])
  (each part (path/parts dir-path)
    (array/push dirs part)
    (let [path (path/join ;dirs)]
      (protect (os/mkdir path)))))

(defn create-dirs-to
  "Create all directories in path specified as string not including the final path segment."
  [dir-path]
  (def dirs @[])
  (each part (slice (path/parts dir-path) 0 -2)
    (array/push dirs part)
    (let [path (path/join ;dirs)]
      (protect (os/mkdir path)))))

(defn make-new-file
  "Create and open a file, creating all the directories leading to the file if
  they do not exist, and return it. By default, open as a writable file (mode is `:w`)."
  [file-path &opt mode]
  (default mode :w)
  (let [parent-path (path/dirname file-path)]
    (when (and (not (exists? file-path))
               (not (exists? parent-path)))
      (create-dirs parent-path)))
  (file/open file-path mode))

(defn copy-file
  "Copy a file from source to destination. Creates all directories in the path
  to the destination file if they do not exist."
  [src-path dst-path]
  (def buf-size 4096)
  (def buf (buffer/new buf-size))
  (with [src (file/open src-path :rb)]
    (with [dst (make-new-file dst-path :wb)]
      (while (def bytes (file/read src buf-size buf))
        (file/write dst bytes)
        (buffer/clear buf)))))

(defn copy
  `Copy a file or directory recursively from one location to another.
  Expects input to be unix style paths`
  [src dest]
  (if (= :windows (os/which))
    (let [end (last (path/posix/parts src))
          isdir (= (os/stat src :mode) :directory)]
      (os/shell (string "C:\\Windows\\System32\\xcopy.exe"
                        " "
                        (path/win32/join ;(path/posix/parts src))
                        (path/win32/join ;(if isdir [;(path/posix/parts dest) end] (path/posix/parts dest)))
                        "/y /s /e /i > nul")))
    (os/execute ["cp" "-rf" src dest] :px)))

(def- shlex-grammar (peg/compile ~{:ws (set " \t\r\n")
                                   :escape (* "\\" (capture 1))
                                   :dq-string (accumulate (* "\"" (any (+ :escape (if-not "\"" (capture 1)))) "\""))
                                   :sq-string (accumulate (* "'" (any (if-not "'" (capture 1))) "'"))
                                   :token-char (+ :escape (* (not :ws) (capture 1)))
                                   :token (accumulate (some :token-char))
                                   :value (* (any (+ :ws)) (+ :dq-string :sq-string :token) (any :ws))
                                   :main (any :value)}))

(defn split
  "Split a string into 'sh like' tokens, returns
   nil if unable to parse the string."
  [s]
  (peg/match shlex-grammar s))

(defn- shell-quote
  [arg]
  (string "'" (string/replace-all "'" `'\''` arg) "'"))

(defn escape
  "Output a string with all arguments correctly quoted"
  [& args]
  (string/join (map shell-quote args) " "))

(defn which
  "Search for the full path to a program, like the `which` command on unix or the `where` command on Windows."
  [name &opt paths]
  (def o (os/which))
  (def win (or (= o :windows) (= o :mingw)))
  (default paths
    (if win
      (string/split ";" (os/getenv "Path"))
      (string/split ":" (os/getenv "PATH"))))
  (def pathexts (if win (string/split ";" (os/getenv "PATHEXT" "")) @[]))
  (array/insert pathexts 0 "")
  (prompt :result
    (each p paths
      (when (= (os/stat p :mode) :directory)
        (def fp (path/join p name))
        (each ext pathexts
          (def fp2 (string fp ext))
          (when (= (os/stat fp2 :mode) :file)
            (return :result fp2)))))))

(defn self-exe
  "Get path to the janet executable"
  []
  (def janet (dyn *executable* "janet"))
  (when (path/abspath? janet) (break janet))
  (case (os/which)
    :linux (os/readlink "/proc/self/exe")
    # default
    (which janet)))
