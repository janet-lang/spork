###
### A simple task executor library/server.
### tasker.janet
###

(import ./path)
(import ./ev-utils)

(def task-meta-name "Name of the task metadata file" "task.jdn")
(def out-file-name "out.log")
(def err-file-name "err.log")

(def- id-bytes 10)
(defn- make-id
  "Create a task id"
  []
  (def bytes (string/bytes (os/cryptorand id-bytes)))
  (string/format
    (comptime (string "task-" (string/repeat "%.2X" id-bytes)))
    ;bytes))

(defn- ts [] (os/time))
(defn- task-dir [tasker] (get tasker :task-dir "."))

(defn- rm
  "Remove a directory and all sub directories."
  [path]
  (case (os/lstat path :mode)
    :directory (do
      (each subpath (os/dir path)
        (rm (path/join path subpath)))
      (os/rmdir path))
    nil nil # do nothing if file does not exist
    # Default, try to remove
    (os/rm path)))

(defn- log-prefix
  "What to put in front of log messages."
  []
  (def now (os/date))
  (string/format "%.2d:%.2d:%.2d " (get now :hours) (get now :minutes) (get now :seconds)))

(defmacro- log
  "Write a log message"
  [& args]
  ~(,eprint (,log-prefix) ,;args))

(defn- task-to-disk
  "Write task data to disk."
  [payload]
  (def mf (get payload :meta-file))
  (log "syncing task " (get payload :task-id) " to disk")
  (string/format "%j" payload) # prevent non-printable structues from being serialized
  (spit mf (string/format "%p" payload))
  payload)

(defn- run-task
  "Run a single task inside an executor"
  [tasker payload]
  (when (= :canceled (get payload :status)) (break))
  (put payload :status :running)
  (def env (os/environ))
  (def task-id (get payload :task-id))
  (def timeout (get payload :timeout))
  (put env "TASK_ID" task-id)
  (put env "TASK_DIRECTORY" (get payload :dir))
  (put env "TASK_METADATA" (get payload :meta-file))
  (def dir (get payload :dir))
  (def out-stream (os/open (path/join dir out-file-name) :wce))
  (def err-stream (os/open (path/join dir err-file-name) :wce))
  (put env :err err-stream)
  (put env :out out-stream)
  (put payload :time-started (ts))
  (def proc (os/spawn (get payload :argv) :ep env))
  (put (tasker :task-proc-map) task-id proc)
  (put (tasker :task-queued-map) task-id payload)
  (put payload :pid (get proc :pid))
  (task-to-disk payload)
  (try
    (do
      (when timeout (ev/deadline timeout))
      (os/proc-wait proc)
      (def return-code (get proc :return-code))
      (put payload :return-code return-code)
      (put payload :status :done))
    ([err]
     (put payload :status :timeout)
     (os/proc-kill proc)))
  (put (tasker :task-proc-map) task-id nil)
  (put (tasker :task-queued-map) task-id nil)
  (put payload :time-finished (ts))
  (put payload :pid nil)
  (:close out-stream)
  (:close err-stream)
  (task-to-disk payload)
  nil)

###
### API
###

(defn new-tasker
  "Create queues and various settings to run tasks. Create a new tasker table."
  [&opt task-directory queues queue-size]
  (default task-directory "./tasks")
  (default queue-size 10)
  (default queues [:default])
  (os/mkdir task-directory)
  (def all-queues @{})
  (def ret
    @{:queues all-queues
      :task-dir task-directory
      :task-proc-map @{}
      :task-queued-map @{}})
  (each q queues
    (def channels (seq [_ :range [0 10]] (ev/chan queue-size)))
    (for pri 0 10 (put all-queues [q pri] (get channels pri))))
  ret)

(defn queue-task
  "Add a task specification to a queue. Supply an argv string array that will be
  used to invoke s a subprocess. The optional `note` parameter is just a textual note
  for task trackingv. The `priority` parameter should be an integer between 0 and 9 inclusive, default
  is 4. Lower priority jobs in the same queue will be executed by higher priority."
  [tasker argv &opt note priority qname timeout expiration]
  (default priority 4)
  (default note "")
  (default expiration (* 3600 24 30)) # 30 days
  (assert (and (int? priority) (>= priority 0) (<= priority 9)) "invalid priority")
  (default qname :default)
  (def q ((tasker :queues) [qname priority]))
  (assert q (string "queue " qname " not found"))
  (def id (make-id))
  (def dir (path/abspath (path/join (task-dir tasker) id)))
  (def meta-file (path/abspath (path/join dir task-meta-name)))
  (def timestamp (ts))
  (def payload
    @{:task-id id 
      :argv argv
      :priority priority
      :time-queued timestamp
      :queue-name qname
      :delete-after (+ timestamp expiration)
      :priority priority
      :timeout timeout
      :note note
      :dir dir
      :meta-file meta-file
      :status :pending})
  (log (string/format "creating task directory %s" dir))
  (os/mkdir dir)
  (task-to-disk payload)
  (ev/give q payload)
  id)

(defn run-executors
  "Start a number of executors to run tasks. Tasks can be added to a queue
  by calling queue-task."
  [tasker &opt workers-per-queue pre-task post-task]
  (default workers-per-queue 1)
  (def nurse (ev-utils/nursery))
  (def qnames (distinct (map first (keys (tasker :queues)))))
  (each qname qnames
    (def channels (seq [[[q p] c] :pairs (tasker :queues) :when (= q qname)] c))
    (for n 0 workers-per-queue
      (ev-utils/spawn-nursery nurse
        (log "starting executor " n " for queue " qname)
        (while (next channels)
          (def [msg chan x] (ev/select ;channels))
          (case msg
            :close
            (array/remove channels (index-of chan channels))
            :take
            (do
              (try
                (do
                  (def task-id (get x :task-id))
                  (log "starting task " task-id)
                  (when pre-task
                    (pre-task x)
                    (task-to-disk x))
                  (run-task tasker x)
                  (when post-task
                    (post-task x)
                    (task-to-disk x))
                  (log "finished task " task-id " normally"))
              ([err f]
               (log (string/format "error running job: %V" err))
               (debug/stacktrace f))))))
        (log "executor " n " for queue " qname " completed"))))
  (ev-utils/join-nursery nurse))

(defn close-queues
  "Prevent any tasks from being added to queues. When an executor finishes it's
  current job, if there are any, it will terminate. When all executors complete, the
  call to `run-executors` will complete."
  [tasker]
  (each chan (tasker :queues)
    (ev/chan-close chan)))

(defn task-status
  "Look up the status of a given task by id."
  [tasker task-id]
  (parse (slurp (path/join (task-dir tasker) task-id task-meta-name))))

(defn all-tasks
  "Get an array of all task ids for which there is still data on disk. If
  `detailed` is truthy, return full task metadata instead of ids."
  [tasker &opt detailed]
  (seq [dir :in (os/dir (task-dir tasker)) :when (string/has-prefix? "task-" dir)]
    (if detailed (task-status tasker dir) dir)))

(defn cancel-task
  "Cancel a queued or running task."
  [tasker task-id]
  (def proc (get (tasker :task-proc-map) task-id))
  (def payload (get (tasker :task-queued-map) task-id))
  (when proc (os/proc-kill proc))
  (when payload
    (put payload :status :canceled)
    (put payload :time-finished (ts))
    (task-to-disk payload)))

(defn run-cleanup
  "Delete old expired jobs saved on disk"
  [tasker]
  (def td (task-dir tasker))
  (log "removing expired task records in " td)
  (each dir (os/dir td)
    (when (string/has-prefix? "task-" dir)
      (def meta-path (path/join td dir task-meta-name))
      (try
        (do
          (def contents (-> meta-path slurp parse))
          (when (> (ts) (get contents :delete-after))
            (rm (path/join td dir))))
        ([err]
         (log "failed to clean up task " dir " : " (describe err))))))
  (log "removed expired task records in " td))
