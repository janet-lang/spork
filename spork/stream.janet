(defn lines
  ```
  Returns a fiber that yields each line from a core/stream value. If separator is not specified, the default separator
  is `\n`. After the fiber yields the last line, it returns `nil`. If the fiber is resumed after the stream is closed or
  after the fiber returns `nil`, an error is thrown.
  ```
  [stream &named separator]
  (default separator "\n")
  (defn yield-lines
    [chunk]
    (when-let [idx (string/find separator chunk)]
      # Yield the first line
      (yield (buffer/slice chunk 0 idx))
      # Eliminate the first line from chunk without creating a new buffer
      (def idx+1 (inc idx))
      (buffer/blit chunk chunk 0 idx+1)
      (yield-lines (buffer/popn chunk idx+1))))
  (defn fetch-lines
    [chunk]
    (if (ev/read stream 1024 chunk)
      (do
        (yield-lines chunk)
        (fetch-lines chunk))
      (do
        (yield-lines chunk)
        (when (not (empty? chunk))
          (yield chunk)))))
  (coro (fetch-lines @"")))

(defn make-stdin
  "Return a readable stream on /dev/stdin. It doesn't work on windows."
  []
  (os/open "/dev/stdin" :r))

(defn make-stdout
  "Return a writable stream on /dev/stdout. It doesn't work on windows."
  []
  (os/open "/dev/stdout" :w))

(defn make-stderr
  "Return a writable stream on /dev/stderr. It doesn't work on windows."
  []
  (os/open "/dev/stderr" :w))
