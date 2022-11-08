(use ../spork/test)
(import spork/zip)

(start-suite)

(os/mkdir "tmp")
(def file-contents (string/repeat "abc123" 1000))
(def w (zip/write-file "tmp/out.zip"))
(zip/add-bytes w "file.txt" file-contents)
(zip/writer-finalize w)

(def r (zip/read-file "tmp/out.zip"))
(def bytes (zip/extract r "file.txt"))
(assert (= file-contents (string bytes)) "compress -> decompress round trip")

(end-suite)
