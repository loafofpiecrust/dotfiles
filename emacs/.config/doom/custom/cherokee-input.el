(quail-define-package
 "cherokee" "UTF-8" "Cherokee" t
 "Input method for Cherokee using d/t consonants."
 nil t nil nil nil nil nil nil nil nil t)

(quail-define-rules
 ("a" "Ꭰ")
 ("e" "Ꭱ")
 ("i" "Ꭲ")
 ("o" "Ꭳ")
 ("u" "Ꭴ")
 ("v" "Ꭵ")
 ("ga" "Ꭶ")
 ("ka" "Ꭷ")
 ("ge" "Ꭸ")
 ("gi" "Ꭹ")
 ("go" "Ꭺ")
 ("gu" "Ꭻ")
 ("gv" "Ꭼ")
 ("ha" "Ꭽ")
 ("he" "Ꭾ")
 ("hi" "Ꭿ")
 ("ho" "Ꮀ")
 ("hu" "Ꮁ")
 ("hv" "Ꮂ")
 ("la" "Ꮃ")
 ("le" "Ꮄ")
 ("li" "Ꮅ")
 ("lo" "Ꮆ")
 ("lu" "Ꮇ")
 ("lv" "Ꮈ")
 ("ma" "Ꮉ")
 ("me" "Ꮊ")
 ("mi" "Ꮋ")
 ("mo" "Ꮌ")
 ("mu" "Ꮍ")
 ("na" "Ꮎ")
 ("hna" "Ꮏ")
 ("nah" "Ꮐ")
 ("ne" "Ꮑ")
 ("ni" "Ꮒ")
 ("no" "Ꮓ")
 ("nu" "Ꮔ")
 ("nv" "Ꮕ")
 ;; Both kw- and qu- forms are in use.
 ("kwa" "Ꮖ") ("qua" "Ꮖ")
 ("kwe" "Ꮗ") ("que" "Ꮗ")
 ("kwi" "Ꮘ") ("qui" "Ꮘ")
 ("kwo" "Ꮙ") ("quo" "Ꮙ")
 ("kwu" "Ꮚ") ("quu" "Ꮚ")
 ("kwv" "Ꮛ") ("quv" "Ꮛ")
 ("sa" "Ꮜ")
 ("s" "Ꮝ")
 ("se" "Ꮞ")
 ("si" "Ꮟ")
 ("so" "Ꮠ")
 ("su" "Ꮡ")
 ("sv" "Ꮢ")
 ("da" "Ꮣ")
 ("ta" "Ꮤ")
 ("de" "Ꮥ")
 ("te" "Ꮦ")
 ("di" "Ꮧ")
 ("ti" "Ꮨ")
 ("do" "Ꮩ")
 ("du" "Ꮪ")
 ("dv" "Ꮫ")
 ("dla" "Ꮬ")
 ("tla" "Ꮭ")
 ("tle" "Ꮮ")
 ("tli" "Ꮯ")
 ("tlo" "Ꮰ")
 ("tlu" "Ꮱ")
 ("tlv" "Ꮲ")
 ;; Provide both j- and ts- forms, as both are used.
 ("ja" "Ꮳ") ("tsa" "Ꮳ")
 ("je" "Ꮴ") ("tse" "Ꮴ")
 ("ji" "Ꮵ") ("tsi" "Ꮵ")
 ("jo" "Ꮶ") ("tso" "Ꮶ")
 ("ju" "Ꮷ") ("tsu" "Ꮷ")
 ("jv" "Ꮸ") ("tsv" "Ꮸ")
 ("wa" "Ꮹ")
 ("we" "Ꮺ")
 ("wi" "Ꮻ")
 ("wo" "Ꮼ")
 ("wu" "Ꮽ")
 ("wv" "Ꮾ")
 ("ya" "Ꮿ")
 ("ye" "Ᏸ")
 ("yi" "Ᏹ")
 ("yo" "Ᏺ")
 ("yu" "Ᏻ")
 ("yv" "Ᏼ"))

(provide 'cherokee-input)
