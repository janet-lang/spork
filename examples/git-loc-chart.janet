###
### Show Lines of code over time in a git repository
###

(use spork/gfx2d)
(use spork/charts)
(use spork/sh-dsl)

# Get lines of code for each commit
(def lines (reverse (string/split "\n" ($<_ git log master --oneline))))
(def commits (map |(string/trim (first (string/split " " $))) lines))
(def commit-to-linecount @{})
(each commit commits
  ($ git checkout ,commit)
  (def lines-of-code
    (scan-number
      (first
        (string/split
          " "
          (string/trim (last (string/split "\n" ($<_ git ls-files | xargs wc -l))))))))
  (put commit-to-linecount commit lines-of-code))

# Make data-frame for chart
(def data-frame
  @{:line-count (seq [c :in commits] (get commit-to-linecount c))
   :x (range (length commits))})
(spit "commit-data.jdn" (string/format "%j" data-frame))

(dark-mode)
(line-chart
  :title "Lines of Code vs. Commit History on master"
  :width (* 2 1920)
  :height (* 2 1080)
  :data data-frame
  :x-column :x
  :y-columns [:line-count]
  :format-x (fn [index] (get commits index))
  :x-label "Commit"
  :y-label "Lines of Code"
  :grid :stipple
  :y-min 0
  #:y-max 70000
  :x-labels-vertical true
  :save-as "loc.png")

(print "Wrote chart to loc.png")

# Show immediately
($ feh loc.png)
