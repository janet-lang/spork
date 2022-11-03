(use ../spork/test)
(import spork/crc)

(start-suite)

# 8 bit CRCs
(def crc8 (crc/make-variant 8 0x07))
(assert (= 0x00 (crc8 "")))
(assert (= 0xA1 (crc8 "abcd")))

# 16 bit CRCs
(def crc16/ccitt-false (crc/make-variant 16 0x1021 0xFFFF))
(def crc16/arc (crc/make-variant 16 0x8005 0x0000 true))
(assert (= 0x2CF6 (crc16/ccitt-false "abcd")))
(assert (= 0xFFFF (crc16/ccitt-false "")))
(assert (= 0xE9D9 (crc16/arc "abcdefg")))

# 32 bit CRCs
(def crc32 (crc/make-variant 32 0x04C11DB7 0xFFFFFFFF true 0xFFFFFFFF))
(def crc32-named (crc/named-variant :crc32))
(def crc32/bzip2 (crc/make-variant 32 0x04C11DB7 0xFFFFFFFF false 0xFFFFFFFF))
(assert (= 0 (crc32 "") (crc32-named "")))
(assert (= 0xED82CD11 (crc32 "abcd")))

(end-suite)
