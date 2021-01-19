###
### misc.janet
###
### One-off functions that don't need their own module.
###

(defn dedent
  "Remove indentation after concatenating the arguments. Works by removing
  leading whitespace, and then removing that same pattern of whitepsace after
  new lines."
  [& xs]
  (def x (string ;xs))
  (def first-letter (find-index (fn [c] (and (not= c (chr "\n"))
                                             (not= c (chr " "))
                                             (not= c (chr "\t"))
                                             (not= c (chr "\r")))) x))
  (if (not first-letter) (break ""))
  (def leading-whitespace (string/slice x 0 first-letter))
  (def indent (last (string/split "\n" leading-whitespace)))
  (if (and indent (not= indent ""))
    (let [y (string/replace-all (string "\n" indent) "\n" (string/replace indent "" x))]
      # Remove trailing newline to mimic long string newline omission.
      (if (= (chr "\n") (last y))
        (slice y 0 -2)
        y))
    x))

(defmacro set*
  ```Parallel `set` function.  Takes a list of targets and
  expressions, evaluates all the expressions, and then
  assigns them to the targets.  Each target can be a variable
  or a 2-tuple, just like in the normal `set` special form.```
  [tgts exprs]
  (when (not= (length tgts) (length exprs)) (error "Expected tgts and exprs to have same length"))
  (def code @['do])
  (def syms @[])
  (loop [e :in exprs]
    (def sym (gensym))
    (array/push syms sym)
    (array/push code (tuple 'def sym e)))
  (loop [[i t] :pairs tgts]
    (array/push code (tuple 'set t (in syms i))))
  (tuple ;code))
