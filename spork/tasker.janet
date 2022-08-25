###
### A simple task executor library/server.
### tasker.janet
###

(import ./path)
(import ./ev-utils)
(import ./schema)
(import ./sh)

(def task-meta-name "Name of the task metadata file" "task.jdn")
(def out-file-name "out.log")
(def err-file-name "err.log")

(def min-priority "Minimum allowed priority (lower priority tasks will execute first)" 0)
(def max-priority "Maximum allowed priority (lower priority tasks will execute first)" 9)
(def default-priority 4)
(def default-expiration (* 30 24 3600))

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

(def- task-record-validator
  "Check if records loaded from disk are valid"
  (schema/validator
    (props
      :task-id (peg (* "task-" :w+))
      :argv (and (or :tuple :array) (values :string))
      :priority (pred int?)
      :time-queued (pred int?)
      :queue-name :keyword
      :delete-after (pred int?)
      :timeout (or :nil (pred int?))
      :input (any)
      :note :string
      :dir :string
      :meta-file :string
      :status (enum :pending :running :canceled :done :timeout))))

(defmacro- log
  "Write a log message"
  [& args]
  ~(,eprint ,;args))

(defn- task-to-disk
  "Write task data to disk."
  [payload]
  (task-record-validator payload)
  (def mf (get payload :meta-file))
  (log "syncing task " (get payload :task-id) " to disk")
  (string/format "%j" payload) # prevent non-printable structues from being serialized
  (spit mf (string/format "%p" payload))
  payload)

(defn- task-to-queue
  "Move a pending task to the correct queue in memory"
  [tasker task]
  (assert (= (get task :status) :pending))
  (def priority (get task :priority))
  (def qname (get task :queue-name))
  (def q ((tasker :queues) [qname priority]))
  (assert q (string "queue " qname " not found"))
  (ev/give q task)
  tasker)

(defn- load-tasks-from-disk
  "Load old tasks already on disk into queue if they did not complete"
  [tasker]
  (def task-directory (task-dir tasker))
  (each dir (os/dir task-directory)
    (when (string/has-prefix? "task-" dir)
      (def meta-path (path/join task-directory dir task-meta-name))
      (try
        (do
          (def contents (-> meta-path slurp parse task-record-validator))
          (case (get contents :status)
            :pending (do
                       (log "found pending task " dir " on disk, adding to queue")
                       (task-to-queue tasker contents))
            :running (do
                       (log "found interruped task " dir " on disk")
                       (put contents :status :canceled)
                       (task-to-disk contents))))
        ([err]
         (log "failed to read suspected task " dir " : " (describe err)))))))


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
      :nurse (ev-utils/nursery)
      :task-proc-map @{}
      :task-queued-map @{}})
  (each q queues
    (def channels (seq [_ :range [min-priority (inc max-priority)]] (ev/chan queue-size)))
    (for pri min-priority (inc max-priority) (put all-queues [q pri] (get channels pri))))
  (load-tasks-from-disk ret)
  ret)

(defn queue-task
  "Add a task specification to a queue. Supply an argv string array that will be
  used to invoke s a subprocess. The optional `note` parameter is just a textual note
  for task trackingv. The `priority` parameter should be an integer between 0 and 9 inclusive, default
  is 4. Lower priority jobs in the same queue will be executed by higher priority. Use input to pass in generic, 
  unstructured input to a task."
  [tasker argv &opt note priority qname timeout expiration input]
  (default priority default-priority)
  (default note "")
  (default expiration default-expiration)
  (assert (and (int? priority) (>= priority min-priority) (<= priority max-priority)) "invalid priority")
  (default qname :default)
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
      :timeout timeout
      :note note
      :input input
      :dir dir
      :meta-file meta-file
      :status :pending})
  (log (string/format "creating task directory %s" dir))
  (os/mkdir dir)
  (task-to-disk payload)
  (task-to-queue tasker payload)
  id)

(defn spawn-executors
  "Start a number of executors to run tasks. Tasks can be added to a queue
  by calling queue-task. A single tasker object can make multiple calls to spawn-executors."
  [tasker &opt qnames workers-per-queue pre-task post-task]
  (default workers-per-queue 1)
  (def nurse (tasker :nurse))
  (default qnames (distinct (map first (keys (tasker :queues)))))
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
                    (pre-task tasker x)
                    (task-to-disk x))
                  (run-task tasker x)
                  (when post-task
                    (post-task tasker x)
                    (task-to-disk x))
                  (log "finished task " task-id " normally"))
              ([err f]
               (log (string/format "error running job: %V" err))
               (debug/stacktrace f))))))
        (log "executor " n " for queue " qname " completed"))))
  tasker)

(defn run-executors
  "Start a number of executors to run tasks as with `tasker/spawn-executors`, and then
  wait for all executors to complete."
  [tasker &opt workers-per-queue pre-task post-task]
  (spawn-executors tasker nil workers-per-queue pre-task post-task)
  (ev-utils/join-nursery (tasker :nurse)))

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
  (var num-removed 0)
  (log "removing expired task records in " td)
  (each dir (os/dir td)
    (when (string/has-prefix? "task-" dir)
      (def meta-path (path/join td dir task-meta-name))
      (try
        (do
          (def contents (-> meta-path slurp parse task-record-validator))
          (when (> (ts) (get contents :delete-after))
            (sh/rm (path/join td dir))
            (++ num-removed)))
        ([err]
         (log "failed to clean up task " dir " : " (describe err))))))
  (log "removed " num-removed " expired task records in " td))
