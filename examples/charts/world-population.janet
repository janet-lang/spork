###
### Make a packed-area chart of the population of countries. Each country gets
### a rectange whose area is proportional to it's population. This makes a good alternative
### to a bar chart, especially when there are many categories.
###

(import spork/gfx2d)
(import spork/charts)

# https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population
(def data
  @{"Abkhazia" 244236
    "Afghanistan" 43844000
    "Albania" 2363314
    "Algeria" 47400000
    "American Samoa (US)" 49710
    "Andorra" 89365
    "Angola" 36604681
    "Anguilla (UK)" 16010
    "Antigua and Barbuda" 103603
    "Argentina" 46387098
    "Armenia" 3076200
    "Aruba (Netherlands)" 109435
    "Australia" 27724744
    "Austria" 9219113
    "Azerbaijan" 10259701
    "Bahamas" 398165
    "Bahrain" 1594654
    "Bangladesh" 169828911
    "Barbados" 267800
    "Belarus" 9109280
    "Belgium" 11930567
    "Belize" 417634
    "Benin" 13224860
    "Bermuda (UK)" 64055
    "Bhutan" 784043
    "Bolivia" 11365333
    "Bosnia and Herzegovina" 3412000
    "Botswana" 2359609
    "Brazil" 213421037
    "British Virgin Islands (UK)" 39471
    "Brunei" 455500
    "Bulgaria" 6437360
    "Burkina Faso" 24070553
    "Burundi" 12332788
    "Cambodia" 17577760
    "Cameroon" 29442327
    "Canada" 41472081
    "Cape Verde" 491233
    "Cayman Islands (UK)" 88833
    "Central African Republic" 6470307
    "Chad" 19340757
    "Chile" 20206953
    "China" 1404890000
    "Christmas Island (Australia)" 1692
    "Cocos (Keeling) Islands (Australia)" 593
    "Colombia" 53057212
    "Comoros" 944388
    "Congo" 6142180
    "Cook Islands (New Zealand)" 15040
    "Costa Rica" 5191824
    "Croatia" 3866233
    "Cuba" 9748007
    "Cura\xC3\xA7ao (Netherlands)" 156115
    "Cyprus" 983000
    "Czechia" 10915839
    "Democratic Republic of the Congo" 112832000
    "Denmark" 6029607
    "Djibouti" 1066809
    "Dominica" 71946
    "Dominican Republic" 10771504
    "Ecuador" 18103660
    "Egypt" 107868296
    "El Salvador" 6029976
    "Equatorial Guinea" 1668768
    "Eritrea" 3607000
    "Estonia" 1362954
    "Eswatini" 1235549
    "Ethiopia" 111652998
    "Falkland Islands (UK)" 3662
    "Faroe Islands (Denmark)" 55177
    "Fiji" 902623
    "Finland" 5656779
    "France" 69081996
    "French Polynesia (France)" 279500
    "Gabon" 2469296
    "Gambia" 2422712
    "Georgia" 3704500
    "Germany" 83497147
    "Ghana" 33742380
    "Gibraltar (UK)" 38196
    "Greece" 10372335
    "Greenland (Denmark)" 56740
    "Grenada" 109021
    "Guam (US)" 153836
    "Guatemala" 18312373
    "Guernsey (UK)" 64781
    "Guinea" 17521167
    "Guinea-Bissau" 1852784
    "Guyana" 956044
    "Haiti" 11867032
    "Honduras" 10186738
    "Hong Kong (China)" 7510800
    "Hungary" 9489000
    "Iceland" 394530
    "India" 1417492000
    "Indonesia" 288315089
    "Iran" 86563000
    "Iraq" 46118793
    "Ireland" 5458600
    "Isle of Man (UK)" 84975
    "Israel" 10196700
    "Italy" 58942828
    "Ivory Coast" 31719275
    "Jamaica" 2774538
    "Japan" 122850000
    "Jersey (UK)" 103267
    "Jordan" 11937000
    "Kazakhstan" 20532240
    "Kenya" 53330978
    "Kiribati" 120740
    "Kosovo" 1585566
    "Kuwait" 4881254
    "Kyrgyzstan" 7404300
    "Laos" 7647000
    "Latvia" 1823500
    "Lebanon" 5490000
    "Lesotho" 2116427
    "Liberia" 5248621
    "Libya" 7459000
    "Liechtenstein" 41237
    "Lithuania" 2885752
    "Luxembourg" 681973
    "Macau (China)" 688900
    "Madagascar" 31727042
    "Malawi" 20734262
    "Malaysia" 34334400
    "Maldives" 515132
    "Mali" 22395489
    "Malta" 574250
    "Marshall Islands" 42418
    "Mauritania" 4927532
    "Mauritius" 1241856
    "Mexico" 131001723
    "Micronesia" 75817
    "Moldova" 2381300
    "Monaco" 38857
    "Mongolia" 3544835
    "Montenegro" 623327
    "Montserrat (UK)" 4386
    "Morocco" 36828330
    "Mozambique" 34090466
    "Myanmar" 51375327
    "Namibia" 3022401
    "Nauru" 11680
    "Nepal" 29911840
    "Netherlands" 18137265
    "New Caledonia (France)" 264596
    "New Zealand" 5342000
    "Nicaragua" 6874748
    "Niger" 27522750
    "Nigeria" 223800000
    "Niue (New Zealand)" 1681
    "Norfolk Island (Australia)" 2188
    "North Korea" 25950000
    "North Macedonia" 1822612
    "Northern Cyprus" 476214
    "Northern Mariana Islands (US)" 47329
    "Norway" 5627400
    "Oman" 5359557
    "Pakistan" 241499431
    "Palau" 16733
    "Palestine" 5483450
    "Panama" 4064780
    "Papua New Guinea" 10185363
    "Paraguay" 6109644
    "Peru" 34350244
    "Philippines" 114123600
    "Pitcairn Islands (UK)" 35
    "Poland" 37314000
    "Portugal" 10749635
    "Puerto Rico (US)" 3184195
    "Qatar" 3214609
    "Romania" 19036031
    "Russia" 146028325
    "Rwanda" 14104969
    "Saint Barth\xC3\xA9lemy (France)" 10562
    "Saint Helena_ Ascension and Tristan da Cunha (UK)" 5651
    "Saint Kitts and Nevis" 51320
    "Saint Lucia" 184100
    "Saint Martin (France)" 31496
    "Saint Pierre and Miquelon (France)" 5819
    "Saint Vincent and the Grenadines" 110872
    "Samoa" 205557
    "San Marino" 34172
    "Saudi Arabia" 35300280
    "Senegal" 18593258
    "Serbia" 6567783
    "Seychelles" 123097
    "Sierra Leone" 9077691
    "Singapore" 6110200
    "Sint Maarten (Netherlands)" 41349
    "Slovakia" 5409407
    "Slovenia" 2133852
    "Solomon Islands" 750325
    "Somalia" 19655000
    "South Africa" 63100945
    "South Korea" 51111158
    "South Ossetia" 56520
    "South Sudan" 15786898
    "Spain" 49570725
    "Sri Lanka" 21781800
    "Sudan" 51662000
    "Suriname" 616500
    "Sweden" 10604464
    "Switzerland" 9124288
    "Syria" 25620427
    "S\xC3\xA3o Tom\xC3\xA9 and Pr\xC3\xADncipe" 209607
    "Taiwan" 23280273
    "Tajikistan" 10499000
    "Tanzania" 68153004
    "Thailand" 65826149
    "Timor-Leste" 1391221
    "Togo" 8095498
    "Tokelau (New Zealand)" 2608
    "Tonga" 100179
    "Transnistria" 367776
    "Trinidad and Tobago" 1367764
    "Tunisia" 11972169
    "Turkey" 86092168
    "Turkmenistan" 7057841
    "Turks and Caicos Islands (UK)" 50828
    "Tuvalu" 10643
    "U.S. Virgin Islands (US)" 87146
    "Uganda" 45905417
    "Ukraine" 28700000
    "United Arab Emirates" 11294243
    "United Kingdom" 69487000
    "United States" 341784857
    "Uruguay" 3485931
    "Uzbekistan" 38236704
    "Vanuatu" 321409
    "Vatican City" 882
    "Venezuela" 28517000
    "Vietnam" 102300000
    "Wallis and Futuna (France)" 11620
    "Western Sahara (disputed)" 600904
    "Yemen" 32684503
    "Zambia" 19693423
    "Zimbabwe" 17073087})

(defn format-thousand-sep [x]
  (if (< x 1000) (break (string x)))
  (def chunk (% x 1000))
  (def y (div x 1000))
  (string (format-thousand-sep y) "," (string/format "%03d" chunk)))

(def extended-data @{})
(eachp [k v] data (put extended-data (string k "\n" (format-thousand-sep v)) v))
(def font (gfx2d/load-font "examples/fonts/Roboto-Regular.ttf" 8))
# use non-linear mapping - country populations are not uniformally distributed.
(def turbo (get charts/color-maps :viridis))
(defn mapping [t x & args] (turbo (math/pow t 0.10) x ;args)) # use non-linear mapping
(def c (charts/plot-packing-chart
         :width (* 1 3840) :height (* 1 2160)
         :data-map extended-data
         #:shuffle-bins true
         #:sort-bins false
         #:background-color gfx2d/black
         #:no-text-resize true
         :inner-padding 2 :padding 1 :color-map mapping
         :font font
         ))
(gfx2d/save "tmp/world-population.png" c)
(print "Wrote to tmp/world-population.png")
