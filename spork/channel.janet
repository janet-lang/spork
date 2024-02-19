(defn from-each
  ```
  Returns a channel that gives each item from an iterable data type. `each` macro is used to iterate over all iterable
  types. `supervisor` argument is passed to `ev/go` which launches two tasks that feed items to the channel. To finish
  the tasks, drain all items from the channel, or close the channel. Otherwise, the tasks remain frozen. When the tasks
  finish, the channel is closed. An error caused during iteration finishes the tasks with an error. Writing to the
  channel finishes the tasks with an error or freezes the fiber that tries to write to the channel.
  ```
  [iterable &named supervisor]
  (def ch (ev/chan))
  (def iterable-ch (ev/chan))
  (def iterable-task (ev/go |(try
                               (defer (:close iterable-ch)
                                 (each item iterable
                                   (ev/give iterable-ch item)))
                               ([err f]
                                 (unless (= err :cancel)
                                   (propagate err f))))
                            nil supervisor))
  (defn give-items []
    (match (ev/select ch iterable-ch)
      [:take c item]
      (if (= c iterable-ch)
        (do
          (ev/give ch item)
          (give-items))
        (do
          (ev/cancel iterable-task :cancel)
          (error "Writing to the returned channel is prohibited.")))
      [:close c]
      # If iterable-ch is closed, give-items exits quietly.
      (when (= c ch)
        (ev/cancel iterable-task :cancel))))
  (ev/go |(defer (:close ch)
            (give-items))
         nil supervisor)
  ch)
