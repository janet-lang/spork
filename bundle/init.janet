(use /spork/declare-cc)
(use /spork/build-rules)
(def e (curenv))

(dofile "project.janet" :env e)

(defn install [manifest &]
  (setdyn :verbose true)
  (setdyn *install-manifest* manifest)
  (build-run e "install"))

(defn build [&]
  (setdyn *install-manifest* @{})
  (setdyn :verbose true)
  (build-run e "build"))

(defn check [&]
  (setdyn *install-manifest* @{})
  (build-run e "test"))

(defn clean [&]
  (setdyn *install-manifest* @{})
  (build-run e "clean"))
