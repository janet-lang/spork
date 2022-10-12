###
### mqtt.janet
###
### MQTT protocol parser and broker implementation
###

###
### Parsing
###

### Packet types
### 0 - CONNECT
### 1 - CONNACK
### 2 - PUBLISH
### 3 - PUBACK
### 4 - PUBREC
### 5 - PUBREL
### 6 - PUBCOMP
### 7 - SUBSCRIBE
### 8 - SUBACK
### 9 - UNSUBSCRIBE
### 10 - UNSUBACK
### 11 - PINGREQ
### 12 - PINGRESP
### 13 - DOSCONNECT
### 14 - AUTH

(defn- combine-uvar
  [b1 &opt b2 b3 b4]
  (cond
    b4 (+ b4 (* 0x80 b3) (* 0x400 b2) (* 0x2000 b1))
    b3 (+ b3 (* 0x80 b2) (* 0x400 b1))
    b2 (+ b2 (* 0x80 b1))
    b1))

(def reason-codes
  {0x00 :success
   0x01 :granted-qos-1
   0x02 :granted-qos-2
   0x04 :disconnect-with-will-message
   0x10 :no-matching-subscribers
   0x11 :no-subscription-existed
   0x18 :continue-authentication
   0x19 :re-authenticate
   0x80 :unspecified-error
   0x81 :malformed-packet
   0x82 :protocol-error
   0x83 :implementation-specific-error
   0x84 :unsupported-protocol-version
   0x85 :client-identifier-not-valid
   0x87 :not-authorized
   0x88 :server-unavailable
   0x89 :server-busy
   0x8A :banned
   0x8B :server-shutting-down
   0x8C :bad-authentication-method
   0x8D :keep-alive-timeout
   0x8E :session-taken-over
   0x8F :topic-filter-invalid
   0x90 :topic-name-invalid
   0x91 :packet-identifier-in-use
   0x92 :packet-identifier-not-found
   0x93 :receive-maximum-exceeded
   0x94 :topic-alias-invalid
   0x95 :packet-too-large
   0x96 :message-rate-too-high
   0x97 :quota-exceeded
   0x98 :administrative-action
   0x99 :payload-format-invalid
   0x9A :retain-not-supported
   0x9B :qos-not-supported
   0x9C :use-another-server
   0x9D :server-moved
   0x9E :shared-subscriptions-not-supported
   0x9F :connection-rate-exceeded
   0xA0 :maximum-connect-time
   0xA1 :subscription-identifiers-not-supported
   0xA2 :wildcard-subscriptions-not-supported})

(def- grammar
  "MQTT grammar table for parsing packets into Janet structures"
  ~{:u32 (uint 4)
    :u16 (uint 2)
    :u8 (uint 1)
    :uvar {:lead (if (range "\x80\xFF") (uint 1))
           :term '(uint 1)
           :main (/ (* (between 0 3 :lead) :term) ,combine-uvar)}
    :uvar-skip (thru (range "\x00\x7F"))
    :data (* 2 '(lenprefix (> -2 :u16) 1))
    :string :data # TODO - validate utf8, but possible overkill

    # properties - capture keyword key and value
    :prop-payload-format-indicator (* "\x01" (constant :payload-format-indiciator) :u8)
    :prop-message-expiry-interval (* "\x02" (constant :message-expiry-interval) :u32)
    :prop-content-type (* "\x03" (constant :content-type) :string)
    :prop-response-topic (* "\x08" (constant :response-topic) :string)
    :prop-correlation-data (* "\x09" (constant :correlation-data) :data)
    :prop-subscription-identifier (* "\x0B" (constant :subscription-identifier) :uvar)
    :prop-session-expiry-interval (* "\x11" (constant :session-expiry-interval) :u32)
    :prop-assigned-client-identifier (* "\x12" (constant :assigned-client-identifier) :string)
    :prop-server-keep-alive (* "\x13" (constant :server-keep-alive) :u16)
    :prop-authentication-method (* "\x15" (constant :authentication-method) :string)
    :prop-authentication-data (* "\x16" (constant :authentication-data) :data)
    :prop-request-problem-information (* "\x17" (constant :reqeust-problem-information) :u8)
    :prop-will-delay-interval (* "\x18" (constant :will-delay-interval) :u32)
    :prop-request-response-information (* "\x19" (constant :reqeust-response-information) :u8)
    :prop-response-information (* "\x1A" (constant :response-information) :string)
    :prop-server-reference (* "\x1C" (constant :server-reference) :string)
    :prop-reason-string (* "\x1F" (constant :reason-string) :string)
    :prop-receive-maximum (* "\x21" (constant :receive-maximum) :u16)
    :prop-topic-alias-maximum (* "\x22" (constant :topic-alias-maximum) :u16)
    :prop-topic-alias (* "\x23" (constant :topic-alias) :u16)
    :prop-maximum-qos (* "\x24" (constant :maximum-qos) :u8)
    :prop-retain-available (* "\x25" (constant :retain-available) :u8)
    :prop-user-property (* "\x26" (/ :string ,keyword) :string)
    :prop-maximum-packet-size (* "\x27" (constant :maximum-packet-size) :u32)
    :prop-wildcard-subscription-available (* "\x28" (constant :wildcard-subscription-available) :u8)
    :prop-subscription-identifier-available (* "\x29" (constant :subscription-identifier-available) :u8)
    :prop-shared-subscription-available (* "\x2A" (constant :shared-subscription-available) :u8)

    # CONNECT
    :pkt-connect
    (/ (* "\x10" :uvar-skip (constant :type) (constant :connect)
          (constant :protocol) :string # protocol name
          (constant :protocol-verstion) :u8 # protocol-level
          (constant :connect-flags) (uint 1 :connect-flags) # connect flags
          (constant :keep-alive) :u16 # keep alive
          :uvar-skip # properties length in bytes
          (any (+ :prop-session-expiry-interval
                  :prop-authentication-method
                  :prop-authentication-data
                  :prop-request-problem-information
                  :prop-request-response-information
                  :prop-receive-maximum
                  :prop-topic-alias-maximum
                  :prop-user-property
                  :prop-maximum-packet-size))
          # payload
          (constant :client-identifier) :string # client idenitifer
          (if (cmt (backref :connect-flags) ,|(not= 0 (band $ 0x04))) # will flag
            (* :uvar-skip (constant :will-properties) (/ (any (+ :prop-will-delay-interval
                                                                 :prop-payload-format-indicator
                                                                 :prop-message-expiry-interval
                                                                 :prop-content-type
                                                                 :prop-response-topic
                                                                 :prop-correlation-data
                                                                 :prop-user-property)) ,table)
               (constant :will-topic) :string
               (constant :will-payload) :data))
          (if (cmt (backref :connect-flags) ,|(not= 0 (band $ 0x80))) # username flag
            (* (constant :username) :string))
          (if (cmt (backref :connect-flags) ,|(not= 0 (band $ 0x40))) # password flag
            (* (constant :password) :data)))
       ,table)

    # CONNACK
    :pkt-connack
    (/ (* "\x20" :uvar-skip (constant :type) (constant :connack)
          (constant :connect-acknowledge-flags) :u8
          (constant :connect-reason-code) :u8
          :uvar-skip
          (any (+ :prop-session-expiry-interval
                  :prop-receive-maximum
                  :prop-maximum-qos
                  :prop-retain-available
                  :prop-maximum-packet-size
                  :prop-assigned-client-identifier
                  :prop-topic-alias-maximum
                  :prop-reason-string
                  :prop-user-property
                  :prop-wildcard-subscription-available
                  :prop-subscription-identifier-available
                  :prop-shared-subscription-available
                  :prop-server-keep-alive
                  :prop-response-information
                  :prop-server-reference
                  :prop-authentication-method
                  :prop-authentication-data)))
       ,table)

    # PUBLISH
    :pkt-publish
    (/ (* (> (range "\x30\x3F"))
          (constant :publish-flags) (/ '1 |(% $ 16) :publish-flags)
          (constant :dup) (/ (backref :publish-flags) ,|(not= 0 (% $ 8)))
          (constant :qos) (/ (backref :publish-flags) ,|(% (* 0.5 $) 4) :qos)
          (constant :retain) (/ (backref :publish-flags) ,odd?)
          :uvar-skip
          (constant :type) (constant :publish)
          (constant :topic-name) :string
          (if (cmt (backref :qos) ,pos?) (* (constant :packet-identifier) :u16))
          :uvar-skip
          (any (+ :prop-payload-format-indicator
                  :prop-message-expiry-interval
                  :prop-topic-alias
                  :prop-response-topic
                  :prop-correlation-data
                  :prop-user-property
                  :prop-subscription-identifier
                  :prop-content-type))
          (constant :payload)
          '(any 1))
       ,table)

    # PUBACK
    # PUBREC
    # PUBREL
    # PUBCOMP
    :pkt-pub-family
    (/ (*
         (+
           (* "\x40" (constant :type) (constant :puback))
           (* "\x50" (constant :type) (constant :pubrec))
           (* "\x62" (constant :type) (constant :pubrel))
           (* "\x70" (constant :type) (constant :pubcomp)))
         :uvar-skip
         (constant :packet-id) :u16
         (+
           -1
           (*
             (constant :reason-code) :u8
             :uvar-skip
             (any (+ :prop-reason-string
                     :prop-user-property)))))
       ,table)

    # SUBSCRIBE
    :pkt-subscribe
    (/ (* "\x82" (constant :type) (constant :subscribe)
          :uvar-skip
          (constant :packet-id) :u16
          :uvar-skip
          (any (+ :prop-subscription-identifier
                  :prop-user-property))
          (constant :topic-filters)
          (group
            (some # todo - parse out topic filter options
                  (group (* :string :u8)))))
       ,table)

    # SUBACK
    :pkt-suback
    (/ (* "\x90" (constant :type) (constant :suback)
          :uvar-skip
          (constant :packet-id) :u16
          :uvar-skip
          (any (+ :prop-reason-string
                  :prop-user-property))
          (constant :reason-codes)
          (group (some :u8)))
       ,table)

    # UNSUBSCRIBE
    :pkt-unsubscribe
    (/ (* "\xA2" (constant :type) (constant :unsubscribe)
          :uvar-skip
          (constant :packet-id) :u16
          :uvar-skip
          (any :prop-user-property)
          (constant :topic-filters)
          (group (some :string)))
       ,table)

    # UNSUBACK
    :pkt-unsuback
    (/ (* "\xB0" (constant :type) (constant :unsuback)
          :uvar-skip
          (constant :packet-id) :u16
          :uvar-skip
          (any (+ :prop-reason-string
                  :prop-user-property))
          (constant :reason-codes)
          (group (some :u8)))
       ,table)

    # PINGREQ
    :pkt-pingreq
    (/ (* "\xC0" (constant :type) (constant :pingreq)
          :uvar-skip)
       ,table)

    # PINGRESP
    :pkt-pingresp
    (/ (* "\xD0" (constant :type) (constant :pingresp)
          :uvar-skip)
       ,table)

    # DISCONNECT
    :pkt-disconnect
    (/ (* "\xE0" (constant :type) (constant :disconnect)
          :uvar-skip
          (constant :reason-code) :u8
          :uvar-skip
          (any (+ :prop-session-expiry-interval
                  :prop-reason-string
                  :prop-user-property
                  :prop-server-reference)))
       ,table)

    # AUTH
    :pkt-auth
    (/ (* "\xF0" (constant :type) (constant :auth)
          :uvar-skip
          (constant :reason-code) :u8
          :uvar-skip
          (any (+ :prop-authentication-method
                  :prop-authentication-data
                  :prop-reason-string
                  :prop-user-property)))
       ,table)

    :main (+ :pkt-connect
             :pkt-connack
             :pkt-publish
             :pkt-pub-family
             :pkt-subscribe
             :pkt-suback
             :pkt-unsubscribe
             :pkt-unsuback
             :pkt-pingreq
             :pkt-pingresp
             :pkt-disconnect
             :pkt-auth)})

(def- packet-peg (peg/compile grammar))
