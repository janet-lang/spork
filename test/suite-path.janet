(use ../spork/test)
(import ../spork/path)

(start-suite)

(defn aeq
  "assert equal"
  [x y &opt deep]
  (assert ((if deep deep= =) x y)
          (string "expected " x " to equal " y)))

(aeq (path/posix/abspath? "/home") true)
(aeq (path/posix/abspath? "home") false)
(aeq (path/posix/abspath? "./home") false)
(aeq (path/posix/abspath? "../home") false)
(aeq (path/posix/abspath? "") false)
(aeq (path/posix/abspath? "//") true)

(aeq (path/abspath "home") (path/join (os/cwd) "home"))
(aeq (path/posix/join "1" "2" "3") "1/2/3")
(aeq (path/posix/join "1" ".." "2" ".." "3") "3")
(aeq (path/posix/join "/home/" "for") "/home/for")

(aeq (path/posix/normalize "/abc/../") "/")
(aeq (path/posix/normalize "/abc/abc") "/abc/abc")
(aeq (path/posix/normalize "/abc/abc/..") "/abc")
(aeq (path/posix/normalize "/abc/abc/../") "/abc/")
(aeq (path/posix/normalize "/abc/abc/../.") "/abc")
(aeq (path/posix/normalize "//////abc/abc/../.") "/abc")
(aeq (path/posix/normalize "//////.") "/")
(aeq (path/posix/normalize "//////") "/")

(aeq (path/win32/normalize `C:\abc\..\`) `C:\`)
(aeq (path/win32/normalize `C:\abc\abc`) `C:\abc\abc`)
(aeq (path/win32/normalize `C:\abc\abc\..`) `C:\abc`)
(aeq (path/win32/normalize `C:\abc\abc\..\`) `C:\abc\`)
(aeq (path/win32/normalize `C:\abc\abc\..\.`) `C:\abc`)
(aeq (path/win32/normalize `\abc\abc`) `\abc\abc`)
(aeq (path/win32/normalize `\\wsl$\Debian\home\abc`) `\\wsl$\Debian\home\abc`)
(aeq (path/win32/normalize `\\server\share\path\file`) `\\server\share\path\file`)
(aeq (path/win32/normalize `\\?\Volume{4c1b02c1-d990-11dc-99ae-806e6f6e6963}\`) `\\?\Volume{4c1b02c1-d990-11dc-99ae-806e6f6e6963}\`)
(aeq (path/win32/normalize `\\.\COM56`) `\\.\COM56`)
(aeq (path/win32/normalize `\\.\abc\abc\..`) `\\.\abc`)
(aeq (path/win32/normalize `\\.\abc\abc\..\.`) `\\.\abc`)

(aeq (path/posix/dirname "abc/def") "abc/")
(aeq (path/posix/dirname "abc/") "abc/")
(aeq (path/posix/basename "abc") "abc")
(aeq (path/posix/dirname "abc") "./")

(aeq (path/posix/ext "project.janet") ".janet")
(aeq (path/posix/ext "/home/pork/work/project.janet") ".janet")

(aeq (path/posix/parts "/home/pork/.local/share")
     @["" "home" "pork" ".local" "share"] true)

(aeq (path/posix/parts ".local/share")
     @[".local" "share"] true)

(with-dyns [:path-cwd "D:\\Users\\sumbuddy"]
  (aeq (path/win32/abspath "C:\\home\\pork") "C:\\home\\pork")
  (aeq (path/win32/abspath "q:\\home\\pork") "q:\\home\\pork")
  (aeq (path/win32/abspath "..\\home\\pork") "D:\\Users\\home\\pork"))

(assert (= (path/posix/relpath "dir1" "dir2") "../dir2"))

(assert (= (path/win32/relpath "dir1" "dir2") "..\\dir2"))

(assert (= (path/relpath "dir1" "dir2") (path/join ".." "dir2")))

(assert (= (path/posix/relpath "a/bit/deeper/with/some/nested/dir" "a/bit/deeper/with/other/nested/dir") "../../../other/nested/dir"))

(assert (= (path/posix/relpath "a/nested/directory/with/a/few/children" "a/nested/directory/with/different/children") "../../../different/children"))

(end-suite)
