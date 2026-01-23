(use ../spork/cjanet)

(@ define STB_IMAGE_IMPLEMENTATION)
(@ define STB_IMAGE_WRITE_IMPLEMENTATION)
(@ define STB_IMAGE_RESIZE_IMPLEMENTATION)
# (@ define STB_TRUETYPE_IMPLEMENTATION)
(@ define STBIW_WINDOWS_UTF8)

(@ define STBI_MALLOC janet_malloc)
(@ define STBI_REALLOC janet_realloc)
(@ define STBI_FREE janet_free)

(include <janet.h>)
(include "stb_image.h")
(include "stb_image_write.h")
(include "stb_image_resize2.h")
# (include "stb_truetype.h")
