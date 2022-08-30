(import spork/tasker)

(def t (tasker/new-tasker))

(ev/spawn
  (tasker/run-executors t))

(tasker/queue-task t ["echo" "hello," "world"])
(tasker/queue-task t ["echo" "hello," "world"])
(tasker/queue-task t ["echo" "hello," "world"])
