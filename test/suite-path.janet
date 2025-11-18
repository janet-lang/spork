(use ../spork/test)
(import ../spork/path)

(start-suite)

(defn aeq
  "assert equal"
  [x y &opt deep]
  (assert ((if deep deep= =) x y)
          (string "expected " x " to equal " y)))

# abspath?

(aeq (path/posix/abspath? "/home") true)
(aeq (path/posix/abspath? "home") false)
(aeq (path/posix/abspath? "./home") false)
(aeq (path/posix/abspath? "../home") false)
(aeq (path/posix/abspath? "") false)
(aeq (path/posix/abspath? "//") true)

(aeq (path/win32/abspath? "/home") false)
(aeq (path/win32/abspath? "home") false)
(aeq (path/win32/abspath? "c:/home") true)
(aeq (path/win32/abspath? "../home") false)
(aeq (path/win32/abspath? "") false)
(aeq (path/win32/abspath? `\\wsl$\Debian\`) true)
(aeq (path/win32/abspath? `\\wsl$\Debian\foo\bar`) true)

# join

(aeq (path/abspath "home") (path/join (os/cwd) "home"))
(aeq (path/posix/join "1" "2" "3") "1/2/3")
(aeq (path/posix/join "1" ".." "2" ".." "3") "3")
(aeq (path/posix/join "/home/" "for") "/home/for")

(aeq (path/win32/join "1" "2" "3") `1\2\3`)
(aeq (path/win32/join "1" ".." "2" ".." "3") "3")
(aeq (path/win32/join `c:` "foo" "bar") `c:\foo\bar`)
(aeq (path/win32/join `c:\` "foo" "bar") `c:\foo\bar`)
(aeq (path/win32/join `\\wsl$\Debian\` "foo" "bar") `\\wsl$\Debian\foo\bar`)
(aeq (path/win32/join `\\wsl$\\Debian` "foo" "bar") `\\wsl$\Debian\foo\bar`)


# normalize

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
(aeq (path/win32/normalize "abc/abc") `abc\abc`)
(aeq (path/win32/normalize `\\wsl$\Debian\home\abc`) `\\wsl$\Debian\home\abc`)
(aeq (path/win32/normalize `\\server\share\path\file`) `\\server\share\path\file`)
(aeq (path/win32/normalize `\\?\Volume{4c1b02c1-d990-11dc-99ae-806e6f6e6963}\`) `\\?\Volume{4c1b02c1-d990-11dc-99ae-806e6f6e6963}\`)
(aeq (path/win32/normalize `\\.\COM56`) `\\.\COM56`)
(aeq (path/win32/normalize `\\.\abc\abc\..`) `\\.\abc`)
(aeq (path/win32/normalize `\\.\abc\abc\..\.`) `\\.\abc`)

# basename

(aeq (path/posix/basename "abc/def") "def")
(aeq (path/posix/basename "abc") "abc")
(aeq (path/posix/basename "abc/") "")
(aeq (path/posix/basename "/home/abc/foo.txt") "foo.txt")
(aeq (path/posix/basename "/") "")

(aeq (path/win32/basename "abc/def") "def")
(aeq (path/win32/basename `abc\def`) `def`)
(aeq (path/win32/basename "abc") "abc")
(aeq (path/win32/basename `c:\foo\bar\baz`) "baz")
(aeq (path/win32/basename `D:\foo.txt`) "foo.txt")
(aeq (path/win32/basename `c:\`) "")
(aeq (path/win32/basename `D:`) "")

(aeq (path/win32/basename `\\wsl$\Debian\home\abc`) `abc`)
(aeq (path/win32/basename `\\wsl$\Debian\home abc`) `home abc`)
(aeq (path/win32/basename `\\wsl$\Debian\`) "")
(aeq (path/win32/basename `\\wsl$\Debian`) "")
(aeq (path/win32/basename `\\.\home\xyz`) `xyz`)
(aeq (path/win32/basename `\\.\homeabc`) "")
(aeq (path/win32/basename `\\.\`) "")
(aeq (path/win32/basename `\\?\`) "")
(aeq (path/win32/basename `\\.\c:\temp\test-file.txt`) "test-file.txt")
(aeq (path/win32/basename `\\?\c:\temp\test-file.txt`) "test-file.txt")
(aeq (path/win32/basename `\\.\c:\temp`) "temp")
(aeq (path/win32/basename `\\?\c:\temp`) "temp")
(aeq (path/win32/basename `\\.\c:\`) "")
(aeq (path/win32/basename `\\?\D:\`) "")
(aeq (path/win32/basename `\\.\c:`) "")
(aeq (path/win32/basename `\\?\D:`) "")

(aeq (path/win32/basename `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\Foo.txt`) "Foo.txt")
(aeq (path/win32/basename `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`) "")
(aeq (path/win32/basename `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\`) "")
(aeq (path/win32/basename `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}`) "")

(aeq (path/win32/basename `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\Foo.txt`) "Foo.txt")
(aeq (path/win32/basename `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`) "")
(aeq (path/win32/basename `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\`) "")
(aeq (path/win32/basename `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}`) "")

(aeq (path/win32/basename `\\.\UNC\Localhost\c$\foo.txt`) `foo.txt`)
(aeq (path/win32/basename `\\.\UNC\Localhost\c$\`) "")
(aeq (path/win32/basename `\\.\UNC\Localhost\c$`) "")
(aeq (path/win32/basename `\\?\UNC\Localhost\c$\foo.txt`) `foo.txt`)
(aeq (path/win32/basename `\\?\UNC\Localhost\c$\`) "")
(aeq (path/win32/basename `\\?\UNC\Localhost\c$`) "")

# dirname

(aeq (path/posix/dirname "abc/def") "abc/")
(aeq (path/posix/dirname "abc/") "abc/")
(aeq (path/posix/dirname "abc") "./")
(aeq (path/posix/dirname "/") "/")
(aeq (path/posix/dirname ".") "./")
(aeq (path/posix/dirname "..") "./")

(aeq (path/win32/dirname `abc\def`) `abc\`)
(aeq (path/win32/dirname "abc/def") "abc/")
(aeq (path/win32/dirname `abc\`) `abc\`)
(aeq (path/win32/dirname "abc/") "abc/")
(aeq (path/win32/dirname "abc") `.\`)
(aeq (path/win32/dirname `c:\`) `c:\`)
(aeq (path/win32/dirname `c:/`) `c:/`)
(aeq (path/win32/dirname `c:`) `c:`)
(aeq (path/win32/dirname ".") `.\`)
(aeq (path/win32/dirname "..") `.\`)
(aeq (path/win32/dirname `\\wsl$\Debian\foo`) `\\wsl$\Debian\`)
(aeq (path/win32/dirname `\\wsl$\Debian\`) `\\wsl$\Debian\`)
(aeq (path/win32/dirname `\\wsl$\Debian`) `\\wsl$\Debian`)
(aeq (path/win32/dirname `\\.\abc\def`) `\\.\abc\`)
(aeq (path/win32/dirname `\\.\abc`) `\\.\abc`)

(aeq (path/win32/dirname `\\.\c:\temp\test-file.txt`) `\\.\c:\temp\`)
(aeq (path/win32/dirname `\\?\c:\temp\test-file.txt`) `\\?\c:\temp\`)
(aeq (path/win32/dirname `\\.\c:\temp\`) `\\.\c:\temp\`)
(aeq (path/win32/dirname `\\?\c:\temp\`) `\\?\c:\temp\`)
(aeq (path/win32/dirname `\\.\c:\temp`) `\\.\c:\`)
(aeq (path/win32/dirname `\\?\c:\temp`) `\\?\c:\`)
(aeq (path/win32/dirname `\\.\X:\`) `\\.\X:\`)
(aeq (path/win32/dirname `\\?\Y:\`) `\\?\Y:\`)
(aeq (path/win32/dirname `\\.\x:`) `\\.\x:`)
(aeq (path/win32/dirname `\\?\y:`) `\\?\y:`)

(aeq (path/win32/dirname `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\Foo.txt`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`)
(aeq (path/win32/dirname `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`)
(aeq (path/win32/dirname `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\`)
(aeq (path/win32/dirname `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}`)

(aeq (path/win32/dirname `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\Foo.txt`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`)
(aeq (path/win32/dirname `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`)
(aeq (path/win32/dirname `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\`)
(aeq (path/win32/dirname `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}`)

(aeq (path/win32/dirname `\\.\UNC\Localhost\c$\foo.txt`) `\\.\UNC\Localhost\c$\`)
(aeq (path/win32/dirname `\\.\UNC\Localhost\c$\`) `\\.\UNC\Localhost\c$\`)
(aeq (path/win32/dirname `\\.\UNC\Localhost\c$`) `\\.\UNC\Localhost\c$`)
(aeq (path/win32/dirname `\\?\UNC\Localhost\c$\foo.txt`) `\\?\UNC\Localhost\c$\`)
(aeq (path/win32/dirname `\\?\UNC\Localhost\c$\`) `\\?\UNC\Localhost\c$\`)
(aeq (path/win32/dirname `\\?\UNC\Localhost\c$`) `\\?\UNC\Localhost\c$`)

# parent

(aeq (path/posix/parent "/abc/def") "/abc")
(aeq (path/posix/parent "/abc/def/") "/abc/def")
(aeq (path/posix/parent "abc/def") "abc")
(aeq (path/posix/parent (path/posix/parent "/abc/def/xyz")) "/abc")
(aeq (path/posix/parent "abc/") "abc")
(aeq (path/posix/parent "abc") "")
(aeq (path/posix/parent "/") "/")
(aeq (path/posix/parent "./") ".")
(aeq (path/posix/parent ".") "")
(aeq (path/posix/parent "..") "")

(aeq (path/win32/parent `abc\def`) `abc`)
(aeq (path/win32/parent "abc/def") "abc")
(aeq (path/win32/parent "abc/def/ghi") "abc/def")
(aeq (path/win32/parent (path/win32/parent `\abc\def\xyz`)) `\abc`)
(aeq (path/win32/parent (path/win32/parent "/abc/def/xyz")) "/abc")
(aeq (path/win32/parent `abc\`) `abc`)
(aeq (path/posix/parent "abc/") "abc")
(aeq (path/win32/parent "abc") "")
(aeq (path/win32/parent `c:\`) `c:\`)
(aeq (path/win32/parent "C:/") "C:/")
(aeq (path/win32/parent `d:`) `d:`)
(aeq (path/win32/parent `c:\foo`) `c:\`)
(aeq (path/win32/parent ".") "")
(aeq (path/win32/parent "..") "")
(aeq (path/win32/parent `\\wsl$\Debian\foo`) `\\wsl$\Debian\`)
(aeq (path/win32/parent `\\wsl$\Debian\foo\`) `\\wsl$\Debian\foo`)
(aeq (path/win32/parent `\\wsl$\Debian\`) `\\wsl$\Debian\`)
(aeq (path/win32/parent `\\wsl$\Debian`) `\\wsl$\Debian`)
(aeq (path/win32/parent `\\.\xyz\def`) `\\.\xyz\`)
(aeq (path/win32/parent `\\.\abc`) `\\.\abc`)

(aeq (path/win32/parent `\\.\X:\`) `\\.\X:\`)
(aeq (path/win32/parent `\\?\Y:\`) `\\?\Y:\`)
(aeq (path/win32/parent `\\.\x:`) `\\.\x:`)
(aeq (path/win32/parent `\\?\y:`) `\\?\y:`)

(aeq (path/win32/parent `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\Foo.txt`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test`)
(aeq (path/win32/parent `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test`)
(aeq (path/win32/parent `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}\`)
(aeq (path/win32/parent `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}`) `\\.\Volume{b75e2c83-0000-0000-0000-602f00000000}`)

(aeq (path/win32/parent `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\Foo.txt`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test`)
(aeq (path/win32/parent `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test\`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\Test`)
(aeq (path/win32/parent `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}\`)
(aeq (path/win32/parent `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}`) `\\?\Volume{b75e2c83-0000-0000-0000-602f00000000}`)

(aeq (path/win32/parent `\\.\UNC\Localhost\c$\foo.txt`) `\\.\UNC\Localhost\c$\`)
(aeq (path/win32/parent `\\.\UNC\Localhost\c$\`) `\\.\UNC\Localhost\c$\`)
(aeq (path/win32/parent `\\.\UNC\Localhost\c$`) `\\.\UNC\Localhost\c$`)
(aeq (path/win32/parent `\\?\UNC\Localhost\c$\foo.txt`) `\\?\UNC\Localhost\c$\`)
(aeq (path/win32/parent `\\?\UNC\Localhost\c$`) `\\?\UNC\Localhost\c$`)

# ext

(aeq (path/posix/ext "project.janet") ".janet")
(aeq (path/posix/ext "/home/pork/work/project.janet") ".janet")

# parts

(aeq (path/posix/parts "/home/pork/.local/share")
     @["" "home" "pork" ".local" "share"] true)

(aeq (path/posix/parts ".local/share")
     @[".local" "share"] true)

(aeq (path/win32/parts `.local\share`)
     @[".local" "share"] true)
(aeq (path/win32/parts ".local\\share")
     @[".local" "share"] true)
(aeq (path/win32/parts ".local/share")
     @[".local" "share"] true)

(aeq (path/win32/parts "c:/home/pork/.local/share")
     @["c:/" "home" "pork" ".local" "share"] true)

(aeq (path/win32/parts `c:\home\pork\.local/share`)
     @["c:\\" "home" "pork" ".local" "share"] true)

(aeq (path/win32/parts `\\wsl$\Debian\share`)
     @["\\\\wsl$\\Debian\\" "share"] true)

(with-dyns [:path-cwd "D:\\Users\\sumbuddy"]
  (aeq (path/win32/abspath "C:\\home\\pork") "C:\\home\\pork")
  (aeq (path/win32/abspath "q:\\home\\pork") "q:\\home\\pork")
  (aeq (path/win32/abspath "..\\home\\pork") "D:\\Users\\home\\pork"))


# split then join should return original path. String matches only
# if input is already normalized.

(def- posix-orig "/home/foo/bar/baz")
(aeq (reduce2 path/posix/join (path/posix/parts posix-orig)) posix-orig)

(def- posix-orig "/home/foo/bar/baz/")
(aeq (reduce2 path/posix/join (path/posix/parts posix-orig)) posix-orig)

(def- win32-orig `c:\home\pork\.local\share`)
(aeq (reduce2 path/win32/join (path/win32/parts win32-orig)) win32-orig)

(def- win32-orig `c:\home\pork\.local\share\`)
(aeq (reduce2 path/win32/join (path/win32/parts win32-orig)) win32-orig)

# relpath

(assert (= (path/posix/relpath "dir1" "dir2") "../dir2"))

(assert (= (path/win32/relpath "dir1" "dir2") "..\\dir2"))

(assert (= (path/relpath "dir1" "dir2") (path/join ".." "dir2")))

(assert (= (path/posix/relpath "a/bit/deeper/with/some/nested/dir" "a/bit/deeper/with/other/nested/dir") "../../../other/nested/dir"))

(assert (= (path/posix/relpath "a/nested/directory/with/a/few/children" "a/nested/directory/with/different/children") "../../../different/children"))

(end-suite)
