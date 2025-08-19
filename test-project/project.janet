(declare-project
  :name "test-project"
  :description "Janet project to test pre/post steps"
  :version "0.0.1"
  :dependencies [])

(declare-source
  :source @["hello"])

(declare-binscript
  :main "bin/bin-no-main"
  :hardcode-syspath true
  :is-janet true)

(declare-binscript
  :main "bin/bin-with-main"
  :hardcode-syspath true
  :is-janet true)

(task "pre-build" ["pre-build-test"])
(task "post-build" ["post-build-test"])

(task "pre-check" ["pre-check-test"])
(task "post-check" ["post-check-test"])

(task "pre-install" ["pre-install-test"])
(task "post-install" ["post-install-test"])

(task "pre-clean" ["pre-clean-test"])
(task "post-clean" ["post-clean-test"])

(task "pre-build-test" []
      (printf "****** pre-build"))

(task "post-build-test" []
      (printf "****** post-build"))

(task "pre-check-test" []
      (printf "****** pre-check"))

(task "post-check-test" []
      (printf "****** post-check"))

(task "pre-install-test" []
      (printf "****** pre-install"))

(task "post-install-test" []
      (printf "****** post-install"))

(task "pre-clean-test" []
      (printf "****** pre-clean"))

(task "post-clean-test" []
      (printf "****** post-clean"))
