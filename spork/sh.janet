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
         (if-not (os/lstat path)
             (os/mkdir path)))))

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
                        "/y "
                        "/s "
                        "/e "
                        "/i ")))
    (os/execute ["cp" "-rf" src dest] :p)))

(def- shlex-grammar (peg/compile ~{
  :ws (set " \t\r\n")
  :escape (* "\\" (capture 1))
  :dq-string (accumulate (* "\"" (any (+ :escape (if-not "\"" (capture 1)))) "\""))
  :sq-string (accumulate (* "'" (any (if-not "'" (capture 1))) "'"))
  :token-char (+ :escape (* (not :ws) (capture 1)))
  :token (accumulate (some :token-char))
  :value (* (any (+ :ws)) (+ :dq-string :sq-string :token) (any :ws))
  :main (any :value)
}))

(defn split
  "Split a string into 'sh like' tokens, returns
   nil if unable to parse the string."
  [s]
  (peg/match shlex-grammar s))

(defn- shell-quote
  [arg]
  (def buf (buffer/new (* (length arg) 2)))
  (buffer/push-string buf "'")
  (each c arg
    (if (= c (chr "'"))
      (buffer/push-string buf "'\\''")
      (buffer/push-byte buf c)))
  (buffer/push-string buf "'")
  (string buf))

(defn escape
  "Output a string with all arguments correctly quoted"
  [& args]
  (string/join (map shell-quote args) " "))

(defn lines
  ```
  It executes a list of command arguments described by args, starts an asynchronous task that retrieves each line from
  the command's standard output as a buffer, and returns a struct with the following keys.

  * `:chan` - The channel that gives each line from the task.
  * `:fiber` - A fiber that yields each line from the channel.
  * `:cancel` - An object-oriented function that accepts the struct and an optional argument which is signal passed to
    `os/proc-kill`. The default signal is `:term`. It kills the process and ends the task. You can call this function
    multiple times without errors.
  * `:exit-code` - An object-oriented function that accepts the struct and returns the process exit code after waiting
    for the task to end. After this function returns the exit code, it returns `nil`.

  Each line is terminated by a newline character, `\n`. After end of process, the channel gives the last line from the
  process. After the channel gives the last line, the process exit code is retrieved, and the process is closed before
  the task ends. Make sure to either take all lines from the channel or call `:cancel`. If you don't, the process
  remains frozen in the background or becomes a zombie process.

  If `stderr` is false or nil, /dev/null is opened and fed the command's standard error and closed when the task
  finishes. If `stderr` is a `core/file` value or a `core/stream` value, `stderr` is fed the command's standard error,
  and it is not closed by this function. If `stderr` is any other value, the returned struct gains a key, `:stderr`.
  `:stderr` is an object-oriented function that accepts the struct and returns standard error of the command as a
  buffer after waiting for the task to finish. After it is called for the first time, it returns `nil`.
  ```
  [args &named stderr]
  (def /dev/null (unless stderr
                   (devnull)))
  (def proc (os/spawn args :p {:out :pipe :err (cond
                                                 # If `stderr` is false or nil
                                                 (not (nil? /dev/null))
                                                 /dev/null
                                                 # If `stderr` is core/file or core/stream
                                                 (let [stderr-type (type stderr)]
                                                   (or (= stderr-type :core/file)
                                                       (= stderr-type :core/stream)))
                                                 stderr
                                                 # If `stderr` is any other value
                                                 :pipe)}))
  (def {:out out :err err} proc)
  (def exit-code (ev/chan 1))
  (def stderr-ch (when err
                   (ev/chan 1)))
  (when stderr-ch
    (ev/spawn
      (ev/give stderr-ch (ev/read err :all))))
  # lines channel must have capacity of 0. Otherwise, it can be closed before `ev/take` can take the last line.
  (def lines (ev/chan))
  (defn fetch-lines
    [chunk]
    # if-let breaks tail call optimization
    (def idx (string/find "\n" chunk))
    (if idx
      (do
        # Give the first line
        (ev/give lines (buffer/slice chunk 0 idx))
        # Eliminate the first line from chunk without creating a new buffer
        (def idx+1 (inc idx))
        (buffer/blit chunk chunk 0 idx+1)
        (fetch-lines (buffer/popn chunk idx+1)))
      (if (ev/read out 1024 chunk)
        (fetch-lines chunk)
        (when (not (empty? chunk))
          (ev/give lines chunk)))))
  (ev/spawn
    (defer (do
             (:close lines)
             (when /dev/null
               (:close /dev/null))
             (ev/give exit-code (:close proc)))
      (try
        (when-let [chunk (ev/read out 1024)]
          (fetch-lines chunk))
        # :cancel causes an error.
        ([_]))))
  {:chan lines
   :fiber (fiber/new (fn recurse []
                       # if-let breaks tail call optimization
                       (def line (ev/take lines))
                       (when line
                         (yield line)
                         (recurse)))
                     :yi)
   :cancel (fn [self &opt signal]
             (default signal :term)
             (try
               (os/proc-kill proc false signal)
               # after the process exit code is retrieved, `os/proc-kill` throws an error.
               ([_]))
             (:close lines)
             nil)
   :exit-code (fn [self] (when-let [ec (ev/take exit-code)]
                           (ev/chan-close exit-code)
                           ec))
   :stderr (when stderr-ch
             (fn [self] (when-let [buf (ev/take stderr-ch)]
                          (ev/chan-close stderr-ch)
                          buf)))})
