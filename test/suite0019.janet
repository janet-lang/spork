(use ../spork/test)
(import spork/_getline)

(start-suite 19)

# TODO: This tests the reduced set of codepoints based on the original set in
# Bestline:
# https://github.com/jart/bestline/blob/4a09bf4355c15c96526/bestline.c#L274-L287
# Some ranges have carveouts in them to account for unallocated characters or
# unusual ones (U+302A and such) in blocks that the Bestline conditional treats
# as monolithic for simplification's sake.
(each [lo hi]
      @[[0x1100 0x115F]
        [0x2329 0x2329]
        [0x232A 0x232A]
        [0x2E80 0x2E99] # U+2E9A unallocated
        [0x2E9B 0x2EF3] # U+2EF4..U+2EFF unallocated
        [0x2F00 0x2FD5] # U+2FD6..U+2FEF unallocated
        [0x2FF0 0x2FFB] # U+2FFC..U+2FFF unallocated
        [0x3000 0x3029] # U+302A..U+302D zero-width?
        [0x302E 0x303E] # U+303F single-width; U+3040 unallocated
        [0x3041 0x3096] # U+3097..U+3098 unallocated; U+3099..U+309A zero-width
        [0x309B 0x30FF] # U+3100..U+3104 unallocated
        [0x3105 0x312F] # U+3130 unallocated
        [0x3131 0x318E] # U+318F unallocated
        [0x3190 0x31E3] # U+31E4..U+31EF unallocated
        [0x31F0 0x321E] # U+321F unallocated
        [0x3220 0x3247] # U+3248..U+324F single-width
        [0x3250 0x4DBF] # U+4DC0..U+4DFF single-width
        [0x4E00 0xA48C] # U+A48D..U+A48F unallocated
        [0xA490 0xA4C6] # U+A4C7..U+A4CF unallocated
        [0xAC00 0xD7A3]
        [0xF900 0xFAFF]
        [0xFE10 0xFE19]
        [0xFE30 0xFE52] # U+FE53 unallocated
        [0xFE54 0xFE66] # U+FE67 unallocated
        [0xFE68 0xFE6B] # U+FE6C..U+FE6F unallocated; U+FF00 unallocated
        [0xFF01 0xFF60]
        [0xFFE0 0xFFE6]
        [0x20000 0x2FFFD]
        [0x30000 0x3FFFD]]
  (for ch lo (inc hi)
    (assert (= 2 (_getline/rune-monowidth ch))
            (string/format "rune-monowidth: %X (expected 2, got %d)"
              ch (_getline/rune-monowidth ch)))))

(end-suite)
