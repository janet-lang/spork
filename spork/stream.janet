(defn lines
  ```
  Returns a fiber that yields each line from a core/stream value. If separator is not specified, the default separator
  is `\n`. If the stream is closed before the fiber yields all lines, an error is thrown from the stream.
  ```
  [stream &opt separator]
  (default separator "\n")
  (defn yield-lines
    [chunk]
    # if-let breaks tail call optimization. when-let depends on if-let.
    # https://github.com/janet-lang/janet/issues/1401
    (def idx (string/find "\n" chunk))
    (when idx
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

(defn lines-channel
  ```
  Returns a channel that gives each line from a core/stream value. An asynchronous task feeds lines to the channel. If
  separator is not specified, the default separator is `\n`. To make sure that the task is finished, drain all lines
  from the channel, or close the stream and the channel. Otherwise, the task remains frozen in the background. The
  channel gives `nil` after one of those conditions applies.

  * end of stream is reached.
  * stream or channel is closed.

  This function ignores errors from closed streams and closed channels.
  ```
  [stream &opt separator]
  (def fiber (lines stream separator))
  (def ch (ev/chan))
  (defn give-lines
    []
    # if-let breaks tail call optimization. when-let depends on if-let.
    # https://github.com/janet-lang/janet/issues/1401
    (def line (resume fiber))
    (when line
      (do
        (ev/give ch line)
        (give-lines))))
  (ev/spawn
    (try
      (defer (:close ch)
        (give-lines))
      # Ignore errors caused by closed streams and closed channels.
      ([_])))
  ch)
