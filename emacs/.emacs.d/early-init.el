;; Disable tool-bar and menu-bar
(unless (eq system-type 'darwin)
  (menu-bar-mode -1))
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(show-paren-mode)
(save-place-mode)
(blink-cursor-mode)
(window-divider-mode)

;; Fonts need a bit of beefing on Mac
;; font test: `' o O 0 () [] {} *i
;; Because it's more complicated to use a font stack in emacs, we probably can't do the
;; ideal of using Hasklig as a backup for all glyphs not rendered by a primary font.
;; Maybe make my own programming font with monospaced IPA symbols.
;; Backup fonts are OK because we mainly want consistent widths within each character set.
;; Potential monospace font choices:
;; SF Mono       :: Quite readable but kinda boring now.
;; Cascadia Code :: Playful but still bold enough for me.
;; mononoki      :: Very similar to Cascadia, but thinner. Very round parens!
;; Hasklig       :: Readable, ligatures, IPA support (!!), no round parens, crashes on
;; Cherokee text.
;; Fira Code     :: Lovely, ligatures, Cherokee, semi-IPA, no italic!
(defvar default-font (if (eq system-type 'darwin) "Hasklig-12" "Fira Code-11")
  "Platform-dependent default font size")

(add-to-list 'default-frame-alist
             `(font . ,default-font))
