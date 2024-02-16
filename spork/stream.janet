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

(defn lines-channel
  ```
  Returns a channel that gives each line from a core/stream value. If separator is not specified, the default separator
  is `\n`. `supervisor` argument is passed to `ev/go` which launches two tasks that feed lines to the channel. To finish
  the tasks, drain all lines from the channel, or close the channel. Otherwise, the tasks remain frozen. When the tasks
  finish, the channel is closed. A stream error finishes the tasks with an error. Writing to the channel finishes the
  tasks with an error or freezes the fiber that tries to write to the channel.
  ```
  [stream &named separator supervisor]
  (def ch (ev/chan))
  (def stream-ch (ev/chan))
  (def stream-task (ev/go |(try
                             (defer (:close stream-ch)
                               (each line (lines stream :separator separator)
                                 (ev/give stream-ch line)))
                             ([err f]
                               (unless (= err :cancel)
                                 (propagate err f))))
                          nil supervisor))
  (defn give-lines []
    (match (ev/select ch stream-ch)
      [:take c line]
      (if (= c stream-ch)
        (do
          (ev/give ch line)
          (give-lines))
        (error "Writing to the returned channel is prohibited."))
      [:close c]
      # If stream-ch is closed, give-lines exits quietly.
      (when (= c ch)
        (ev/cancel stream-task :cancel))))
  (ev/go |(defer (:close ch)
            (give-lines))
         nil supervisor)
  ch)
