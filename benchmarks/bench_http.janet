# benchmarks/bench_http.janet
#
# Empirical benchmark harness for Spork HTTP client streaming performance.
# Measures wall-clock elapsed time, throughput (MB/s), and peak resident memory (VmHWM).
#
# Usage:
#   janet benchmarks/bench_http.janet [options]
#
# Options:
#   -b, --bench VALUE     Benchmark suite: all, pre-streaming, pre-perf1, perf, streaming
#   -r, --runs VALUE      Number of runs to average (default: 3)
#   -s, --size VALUE      Payload size in MiB (default: 1024)
#   -l, --label VALUE     Report label (defaults to current git short hash)
#   -d, --dest VALUE      File destination for download (default: platform null device)

(import spork/argparse :as argparse)
(import ../spork/http :as http)

(def- req-fn (get-in (curenv) [(quote http/request) :value]))
(def- dl-fn (get-in (curenv) [(quote http/download) :value]))
(def- open-fn (get-in (curenv) [(quote http/open-stream) :value]))


(defn- detect-git-label
  "Detect current git short hash for report labeling."
  []
  (try
    (let [p (os/spawn ["git" "rev-parse" "--short" "HEAD"] :p {:out :pipe})
          out (string/trim (:read (p :out) :all))]
      (:wait p)
      (if (empty? out) "current" out))
    ([_] "current")))

(defn- get-hwm-kb
  "Get peak resident memory (high-water mark) in kilobytes.
  Falls back gracefully on non-Linux POSIX systems or returns nil if unsupported."
  []
  (try
    # 1. Linux /proc/self/status
    (if-let [f (file/open "/proc/self/status" :r)]
      (defer (file/close f)
        (var hwm 0)
        (while (def line (file/read f :line))
          (when (string/has-prefix? "VmHWM:" line)
            (def parts (filter |(not (empty? $)) (string/split " " line)))
            (set hwm (scan-number (get parts 1 0)))
            (break)))
        (if (pos? hwm) hwm nil))
      # 2. POSIX ps fallback (FreeBSD, macOS, etc.)
      (if (find |(= (os/which) $) [:freebsd :macos :openbsd :netbsd])
        (let [p (os/spawn ["ps" "-o" "rss=" "-p" (string (os/getpid))] :p {:out :pipe})
              out (string/trim (:read (p :out) :all))
              kb (scan-number out)]
          (:wait p)
          (if (and kb (pos? kb)) kb nil))
        nil))
    ([_] nil)))

(defn- run-bench
  "Run a benchmark thunk across num-runs, measuring wall-clock duration and peak RSS."
  [id name size-mb num-runs thunk]
  (if (nil? thunk)
    {:id id :name name :skipped true}
    (do
      (var total-elapsed 0)
      (var peak-rss 0)
      (repeat num-runs
        (gccollect)
        (def t0 (os/clock))
        (thunk)
        (def t1 (os/clock))
        (+= total-elapsed (- t1 t0))
        (when-let [hwm (get-hwm-kb)]
          (def rss (/ hwm 1024))
          (when (> rss peak-rss)
            (set peak-rss rss))))
      (def avg-elapsed (/ total-elapsed num-runs))
      {:id id
       :name name
       :runs num-runs
       :elapsed avg-elapsed
       :speed (/ size-mb avg-elapsed)
       :rss (if (pos? peak-rss) peak-rss nil)})))

(def- benchmark-defs
  {:file
   {:name "Stream to file (http/download)"
    :run (fn [url dest]
           (if dl-fn
             (fn []
               (dl-fn url dest))
             nil))}

   :sink
   {:name "Stream to sink (callback)"
    :run (fn [url _]
           (if dl-fn
             (fn []
               (var total 0)
               (dl-fn url (fn [chunk] (+= total (length chunk)))))
             nil))}

   :blocks
   {:name "Stream blocks (:blocks default)"
    :run (fn [url _]
           (if open-fn
             (fn []
               (with [s (open-fn url)]
                 (loop [b :in (:blocks s)]
                   nil)))
             nil))}

   :blocks-reuse
   {:name "Stream blocks (:blocks reuse-buf)"
    :run (fn [url _]
           (if open-fn
             (fn []
               (def scratch (buffer/new 65536))
               (with [s (open-fn url)]
                 (loop [b :in (:blocks s 65536 scratch)]
                   nil)))
             nil))}

   :read-all
   {:name "In-memory stream (:read s :all)"
    :run (fn [url _]
           (if open-fn
             (fn []
               (with [s (open-fn url)]
                 (:read s :all)))
             nil))}

   :in-memory
   {:name "In-memory buffer (http/request)"
    :run (fn [url _]
           (if req-fn
             (fn [] (req-fn "GET" url))
             nil))}})

(def- suites
  {"all" [:file :sink :blocks :blocks-reuse :read-all :in-memory]
   "pre-streaming" [:in-memory]
   "pre-perf1" [:file :sink :blocks :read-all :in-memory]
   "pre-perf" [:file :sink :blocks :read-all :in-memory]
   "perf" [:file :sink :blocks :blocks-reuse :read-all :in-memory]
   "perf1" [:file :sink :blocks :blocks-reuse :read-all :in-memory]
   "streaming" [:file :sink :blocks :blocks-reuse]
   "file" [:file]
   "sink" [:sink]
   "blocks" [:blocks]
   "blocks-reuse" [:blocks-reuse]
   "read-all" [:read-all]
   "in-memory" [:in-memory]})

(defn- make-mock-server
  "Create an HTTP mock server streaming total-bytes over an ephemeral port."
  [total-bytes block]
  (def block-size (length block))
  (def num-blocks (div total-bytes block-size))
  (def header (string "HTTP/1.1 200 OK\r\nContent-Length: " total-bytes "\r\nConnection: close\r\n\r\n"))
  (net/server "127.0.0.1" "0" (fn [conn]
                                (defer (:close conn)
                                  (try
                                    (do
                                      (:read conn 1024 @"")
                                      (:write conn header)
                                      (repeat num-blocks (:write conn block)))
                                    ([_] nil))))))

(defn- print-report
  "Format and print structured benchmark results table and summary line."
  [label size-mb num-runs results]
  (def sep "+------------------------------------------+------------+---------------+-------------+")

  (print "")
  (print (string/format "Spork HTTP Benchmark: %s (%d MiB payload, %d-run average)" label size-mb num-runs))
  (print sep)
  (printf "| %-40s | %10s | %13s | %11s |" "Workload" "Time (avg)" "Throughput" "Peak RSS")
  (print sep)
  (each r results
    (if (r :skipped)
      (printf "| %-40s | %-41s |" (r :name) "[SKIPPED: not in this revision]")
      (let [rss-str (if-let [rss (r :rss)] (string/format "%8.1f MB" rss) "        N/A")]
        (printf "| %-40s | %7.3f s  | %7.1f MB/s | %11s |"
                (r :name) (r :elapsed) (r :speed) rss-str))))
  (print sep)
  (print "")

  # Generate formatted commit summary line
  (def by-id (table ;(mapcat (fn [r] [(r :id) r]) (filter |(not ($ :skipped)) results))))
  (def mem (or (in by-id :in-memory) (in by-id :read-all)))
  (def rss-suffix
    (fn [r]
      (if-let [rss (r :rss)]
        (string/format " | Peak RSS: %.1f MB" rss)
        "")))
  (if (and mem (nil? (in by-id :file)))
    (print (string/format "Commit Summary Line:\n  spork @ %s (pre-streaming):  %.2fs | %.0f MB/s%s (in-memory buffer)\n"
                          label (mem :elapsed) (mem :speed) (rss-suffix mem)))
    (when (and (in by-id :file) (in by-id :sink))
      (def f (in by-id :file))
      (def s (in by-id :sink))
      (if (and mem (f :rss) (mem :rss))
        (print (string/format "Commit Summary Line:\n  spork @ %s:  %.2fs mem (%.0f MB/s) | %.2fs file (%.0f MB/s) | %.2fs sink (%.0f MB/s) | Peak RSS: %.1f MB (stream) / %.1f MB (mem)\n"
                              label (mem :elapsed) (mem :speed) (f :elapsed) (f :speed) (s :elapsed) (s :speed) (f :rss) (mem :rss)))
        (print (string/format "Commit Summary Line:\n  spork @ %s:  %.2fs file (%.0f MB/s) | %.2fs sink (%.0f MB/s)%s\n"
                              label (f :elapsed) (f :speed) (s :elapsed) (s :speed) (rss-suffix f)))))))

(defn main [&]
  (def default-dest (if (= (os/which) :windows) "NUL" "/dev/null"))
  (def parsed
    (argparse/argparse
      "Empirical benchmark harness for Spork HTTP client streaming performance."
      "bench" {:kind :option
               :short "b"
               :default "all"
               :help "Benchmark suite: all, pre-streaming, pre-perf1, perf, streaming"}
      "runs" {:kind :option
              :short "r"
              :default "3"
              :help "Number of benchmark iterations to average (default: 3)"
              :map scan-number}
      "size" {:kind :option
              :short "s"
              :default "1024"
              :help "Payload size in MiB (default: 1024)"
              :map scan-number}
      "label" {:kind :option
               :short "l"
               :help "Report label (defaults to current git short hash)"}
      "dest" {:kind :option
              :short "d"
              :default default-dest
              :help (string "Destination for file download (default: " default-dest ")")}))

  (unless parsed
    (os/exit 1))

  (def bench-opt (in parsed "bench"))
  (def size-mb (in parsed "size"))
  (def num-runs (in parsed "runs"))
  (def label (or (in parsed "label") (detect-git-label)))
  (def dest (in parsed "dest"))
  (def suite-keys (in suites bench-opt))

  (unless suite-keys
    (eprint (string/format "unknown benchmark or suite: %s" bench-opt))
    (eprint (string/format "available suites: %s" (string/join (sort (keys suites)) ", ")))
    (os/exit 1))

  (def block-size 65536) # 64 KiB
  (def total-bytes (* size-mb 1024 1024))
  (def block (string/repeat "0123456789abcdef" 4096))

  (def srv (make-mock-server total-bytes block))
  (defer (:close srv)
    (def [_ port] (net/localname srv))
    (def url (string "http://127.0.0.1:" port "/data"))

    (def results
      (map (fn [key]
             (def entry (in benchmark-defs key))
             (def thunk ((entry :run) url dest))
             (run-bench key (entry :name) size-mb num-runs thunk))
           suite-keys))

    (print-report label size-mb num-runs results)))
