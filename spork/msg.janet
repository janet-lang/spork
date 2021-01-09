###
### msg.janet
###
### A simple message protocol for sequential messages
### over a stream (full duplex channel of strings).
###

# Message Protocol
#
#          | b0 | b1 | b2 | b3 | ... n bytes |
#  n = b0 + b1 * 0x100 + b2 * 0x10000 + b3 * 0x1000000
#
# Messages are a four byte little endian unsigned message prefix
# denoting length followed by a payload of length bytes.
# An interrupted or incomplete message should be converted to nil.

(defmacro- nilerr
  "Coerce errors to nil."
  [& body]
  (apply try ~(do ,;body) [~([_] nil)]))

(defn make-recv
  "Get a function that, when invoked, gets the next message from a readable stream.
  Provide an optional unpack function that will parse the received buffer."
  [stream &opt unpack]
  (def buf @"")
  (default unpack string)
  (fn receiver []
    (buffer/clear buf)
    (if-not (nilerr (:chunk stream 4 buf)) (break))
    (def [b0 b1 b2 b3] buf)
    (def len (+ b0 (* b1 0x100) (* b2 0x10000) (* b3 0x1000000)))
    (buffer/clear buf)
    (if-not (nilerr (:chunk stream len buf)) (break))
    (unpack (string buf))))

(defn make-send
  "Create a function that when called with a msgs sends that msg.
  Provide an optional pack function that will convert a message to a string."
  [stream &opt pack]
  (def buf @"")
  (default pack string)
  (fn sender [msg]
    (def x (pack msg))
    (buffer/clear buf)
    (buffer/push-word buf (length x))
    (buffer/push-string buf x)
    (:write stream buf)
    nil))

(defn make-proto
  "Create both a send an recv function from a stream, as with
  `make-send` and `make-recv`."
  [stream &opt pack unpack]
  [(make-send stream pack) (make-recv stream unpack)])
