#
# Run test scripts, and error if any fail.
# Pass through any args
#
(use /spork/declare-cc)
(dofile "project.janet" :env (jpm-shim-env))
(check)
