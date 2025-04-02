(declare-project
  :name "testmod")

(def n1
  (declare-native
    :name "testmod"
    :source @["testmod.c"]))

(def n2
  (declare-native
    :name "testmod2"
    :source @["testmod2.c"]))

(def n3
  (declare-native
    :name "testmod3"
    :source @["testmod3.cpp"]))

(def n4
  (declare-native
    :name "test-mod-4"
    :source @["testmod4.c"]))

(def n5
  (declare-native
    :name "testmod5"
    :source @["testmod5.cc"]))

(declare-executable
  :name "testexec"
  :entry "testexec.janet"
  :deps [(n1 :native)
         (n2 :native)
         (n3 :native)
         (n4 :native)
         (n5 :native)
         (n1 :static)
         (n2 :static)
         (n3 :static)
         (n4 :static)
         (n5 :static)])

(declare-executable
  :name "testexec-static"
  :static true
  :entry "testexec.janet"
  :deps [(n1 :native)
         (n2 :native)
         (n3 :native)
         (n4 :native)
         (n5 :native)
         (n1 :static)
         (n2 :static)
         (n3 :static)
         (n4 :static)
         (n5 :static)])
