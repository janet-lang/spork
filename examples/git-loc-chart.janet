###
### Show Lines of code over time in a git repository
###

(use spork/gfx2d)
(use spork/charts)
(use spork/sh-dsl)

# Get lines of code for each commit
(def lines (string/split "\n" ($<_ git log master --date-order --topo-order --reverse --oneline)))
(def commits (map |(string/trim (first (string/split " " $))) lines))
(def commit-to-linecount @{})
(eachp [i commit] commits
  (print "Checking out " commit)
  ($ git checkout ,commit)
  # Count the total number of lines in all text files from git ls-files
  (print "Counting lines for " commit)
  #(def exclusions '[--exclude *.exe --exclude /bin/*])
  (def exclusions [])
  (def raw-out ($<_ git ls-files ;exclusions | xargs grep -I -l -- ^ >err /dev/null | xargs wc -l))
  (def raw-count-line (string/trim (last (string/split "\n" raw-out))))
  (def lines-of-code (scan-number (first (string/split " " raw-count-line))))
  (print "Found " lines-of-code " for commit " commit)
  (printf "%.2f%% complete - %d of %d" (* 100 (/ i (length commits))) i (length commits))
  (put commit-to-linecount commit lines-of-code))

# Make data-frame for chart
(def data-frame
  @{:line-count (seq [c :in commits] (get commit-to-linecount c))
   :x (range (length commits))})

(os/mkdir "tmp")
(spit "tmp/commit-data.jdn" (string/format "%j" data-frame))

(dark-mode)
(line-chart
  :title "Lines of Code vs. Commit History on master"
  :width (* 2 1920) # 4K!
  :height (* 2 1080)
  :data data-frame
  :x-column :x
  :y-column [:line-count]
  :format-x (fn [index] (get commits index))
  :x-label "Commit"
  :y-label "Lines of Code"
  :grid :stipple
  :y-min 0
  #:y-max 70000
  :x-labels-vertical true
  :save-as "tmp/loc.png")

(print "Wrote chart to tmp/loc.png")

# Show immediately. if it fails, oh well
($? feh tmp/loc.png)
