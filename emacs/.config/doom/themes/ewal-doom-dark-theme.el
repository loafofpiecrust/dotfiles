;;; ewal-doom-dark-theme.el --- Dread the color of darkness -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Taylor Snead

;; Author: Taylor Snead
;; URL: https://gitlab.com/jjzmajic/ewal
;;
;; Version: 0.1
;; Keywords: faces
;; Package-Requires: ((emacs "25") (ewal "0.1") (doom-themes "0.1"))

;; This program is free software: you can redistribute it and/or modify it under
;; the terms of the GNU General Public License as published by the Free Software
;; Foundation, either version 3 of the License, or (at your option) any later
;; version.

;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
;; FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
;; details.

;; You should have received a copy of the GNU General Public License along with
;; this program. If not, see <http://www.gnu.org/licenses/>.

;; This file is not part of Emacs.

;;; Commentary:

;; An `ewal'-based theme, created using `doom-dark' as its base.

;;; Code:
(require 'ewal-doom-themes)

(defgroup ewal-doom-dark-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom ewal-doom-dark-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'ewal-doom-dark-theme
  :type 'boolean)

(defcustom ewal-doom-dark-brighter-comments nil
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'ewal-doom-dark-theme
  :type 'boolean)

(defcustom ewal-doom-dark-comment-bg ewal-doom-dark-brighter-comments
  "If non-nil, comments will have a subtle, darker background."
  :group 'ewal-doom-dark-theme
  :type 'boolean)

(defcustom ewal-doom-dark-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'ewal-doom-dark-theme
  :type '(choice integer boolean))

(ewal-load-colors)

;; HACK: fixes bytecode overflow
(defvar ewal-doom-dark-hack
  (ewal-doom-themes-get-color 'background 0))

(def-doom-theme ewal-doom-dark
  "A dark theme inspired by Atom One Dark, cutomized with `ewal'."

  ;; name        default   256       16
  ((bg         (ewal-doom-themes-get-color 'background  0))
   (bg-alt     (ewal-doom-themes-get-color 'background -3))
   (base0      (ewal-doom-themes-get-color 'background -5))
   (base1      (ewal-doom-themes-get-color 'background -4))
   (base2      (ewal-doom-themes-get-color 'background -2))
   (base3      (ewal-doom-themes-get-color 'background -1))
   (base4      (ewal-doom-themes-get-color 'background +1))
   (base5      (ewal-doom-themes-get-color 'comment     0))
   (base6      (ewal-doom-themes-get-color 'background +4))
   (base7      (ewal-doom-themes-get-color 'background +5))
   (base8      (ewal-doom-themes-get-color 'brightwhite +1))
   (base9      (ewal-doom-themes-get-color 'background +3))
   (fg         (ewal-doom-themes-get-color 'brightwhite  0))
   (fg-alt     (ewal-doom-themes-get-color 'white 0))
   (wallpaper (ewal-doom-themes-get-color 'brightred -3))

   (grey       base4)
   (red        (ewal-doom-themes-get-color 'red      0))
   (orange     (ewal-doom-themes-get-color 'brightred       0))
   (green      (ewal-doom-themes-get-color 'green    0))
   (teal       (ewal-doom-themes-get-color 'brightgreen     0))
   (yellow     (ewal-doom-themes-get-color 'yellow   0))
   (bright-yellow     (ewal-doom-themes-get-color 'brightyellow   0))
   (blue       (ewal-doom-themes-get-color 'brightblue      0))
   (dark-blue  (ewal-doom-themes-get-color 'blue     0))
   (magenta    (ewal-doom-themes-get-color 'brightmagenta   0))
   (violet     (ewal-doom-themes-get-color 'magenta  0))
   (cyan       (ewal-doom-themes-get-color 'brightcyan      0))
   (dark-cyan  (ewal-doom-themes-get-color 'cyan     0))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        yellow)
   (comments       dark-cyan)
   (doc-comments   teal)
   (constants      violet)
   (functions      magenta)
   (keywords       green)
   (methods        cyan)
   (operators      fg)
   (type           yellow)
   (strings        red)
   (variables      fg)
   (numbers        orange)
   (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright ewal-doom-dark-brighter-modeline)
   (-modeline-pad
    (when ewal-doom-dark-padded-modeline
      (if (integerp ewal-doom-dark-padded-modeline) ewal-doom-dark-padded-modeline 1)))

   (modeline-fg     fg)
   ;; (modeline-fg-alt fg-alt)
   (modeline-fg-alt nil)

   (modeline-bg
    (if -modeline-bright
        (doom-darken blue 0.475)
      `(,(doom-darken (car bg-alt) 0.15) ,@(cdr base0))))
   (modeline-bg-l
    (if -modeline-bright
        (doom-darken blue 0.45)
      `(,(doom-darken (car bg-alt) 0.1) ,@(cdr base0))))
   (modeline-bg-inactive   `(,(doom-darken (car bg-alt) 0.1) ,@(cdr bg-alt)))
   (modeline-bg-inactive-l `(,(car bg-alt) ,@(cdr base1))))


  ;; --- extra faces ------------------------
  ((elscreen-tab-other-screen-face :background "#353a42" :foreground "#1e2022")

   ;; basics
   ((lazy-highlight &override) :foreground base0 :distant-foreground base1)
   ((lsp-face-highlight-textual &override) :foreground base0 :distant-foreground base1)
   ((lsp-face-highlight-read &override) :foreground base0 :distant-foreground base1)
   ((lsp-face-highlight-write &override) :foreground base0 :distant-foreground base1)
   ((lsp-ui-peek-highlight &override) :foreground fg)
   (cursor :background magenta)
   (mini-modeline-mode-line :background nil)
   (doom-modeline-bar :background orange :foreground fg :height 1.1)
   (doom-modeline-bar-inactive :height 1.1)
   (aw-mode-line-face :foreground red)
   (doom-modeline-battery-normal :inherit nil)

   (show-paren-match :background base9 :foreground bright-yellow :weight 'bold)
   ((hl-todo &override) :foreground orange)

   (highlight :foreground blue :underline nil ;; :underline `(:style line :color ,blue)
              )
   ((link &override) :underline `(:style line :color ,blue) :weight 'bold)

   ;; Turn all wavy underlines into straight ones for readability.

   ((cfw:face-grid &override) :foreground base6)
   ((cfw:face-toolbar-button-off &override) :foreground fg-alt)
   ((cfw:face-sunday &override) :foreground red)

   ;; Add padding to the minibuffer prompt, making it easier to read.
   ;; This is especially helpful for single-line prompts like passwords.
   ((minibuffer-prompt &override) :box `(:line-width 4 :color ,bg;; base6
                                         ))

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground comments :background region)
   ((line-number-current-line &override) :foreground fg)

   (font-lock-comment-face
    :foreground comments
    :background (if ewal-doom-dark-comment-bg (doom-lighten bg 0.05))
    :slant 'italic)
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments
    :slant 'italic)
   (font-lock-type-face
    :foreground type
    :slant 'italic)

   (mode-line
    ;; :height 30
    :background base6
    :foreground modeline-fg
    :weight 'bold
    ;; :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg))
    )
   (mode-line-inactive
    ;; :height 30
    :weight 'normal
    :foreground modeline-fg
    :inherit 'vertical-border
    ;; :background modeline-bg-inactive :foreground modeline-fg-alt
    ;; :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive))
    )
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))
   (doom-modeline-spc-face :inherit nil)

   ((header-line &override) :extend t :weight 'medium ;; :background base6
    :background base6
    ;; :box `(:line-width 7 :color ,bg)
    ;; :underline `(:style line :color ,yellow)
    )
   ;; (mode-line-buffer-id :inherit 'variable-pitch)
   ;; ((doom-modeline-buffer-path &override) :inherit 'variable-pitch)

   ((message-header-subject &override) :height 1.1)

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground green :weight 'bold)

   ;; ivy-mode
   (ivy-current-match :distant-foreground base0 :weight 'bold :inverse-video t)
   (selectrum-current-candidate :weight 'bold :inverse-video t)
   (ivy-posframe :background base0 :foreground fg)
   (ivy-minibuffer-match-face-1 :foreground selection :background base4)

   ;; ediff
   (ediff-fine-diff-A :background (doom-blend magenta bg 0.3) :weight 'bold)

   ;; evil-mode
   ;; (evil-search-highlight-persist-highlight-face :background violet)

   ;; evil-snipe
   (evil-snipe-first-match-face :foreground base0 :background green)
   (evil-snipe-matches-face     :foreground green :underline t)

   ;; flycheck
   (flycheck-error   :underline `(:style line :color ,red)   )
   (flycheck-warning :underline `(:style line :color ,yellow))
   (flycheck-info    :underline `(:style line :color ,green))
   (spell-fu-incorrect-face :underline `(:style line :color ,red))

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground magenta)
   (rainbow-delimiters-depth-2-face :foreground green)
   (rainbow-delimiters-depth-3-face :foreground cyan)
   (rainbow-delimiters-depth-4-face :foreground orange)
   (rainbow-delimiters-depth-5-face :foreground magenta)
   (rainbow-delimiters-depth-6-face :foreground green)
   (rainbow-delimiters-depth-7-face :foreground cyan)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))
   (markdown-blockquote-face :inherit 'italic :foreground dark-blue)
   (markdown-list-face :foreground magenta)
   (markdown-pre-face  :foreground cyan)
   (markdown-link-face :inherit 'bold :foreground blue)
   (markdown-bold-face :inherit 'bold)
   (markdown-italic-face :inherit 'italic)

   (tooltip              :background base0 :foreground fg)
   (company-tooltip-selection     :inverse-video t)
   (vertical-border :foreground base6 :background base6)
   ;; (internal-border :background green :foreground green)
   (internal-border :background wallpaper :foreground wallpaper)
   (whitespace-indentation :inherit 'default)
   (whitespace-big-indent :inherit 'default)
   (hl-line :background base9)

   ;; Gaps between windows
   (window-divider :background wallpaper :foreground wallpaper)
   ;; (window-divider-first-pixel :inherit 'vertical-border)
   ;; (window-divider-last-pixel :inherit 'vertical-border)

   ;; org-mode
   ((outline-1 &override) :foreground violet)
   ((outline-2 &override) :foreground green)
   ((outline-3 &override) :foreground orange)
   ((outline-4 &override) :foreground yellow)
   ((outline-5 &override) :foreground cyan)
   ((outline-6 &override) :foreground magenta)
   ((outline-7 &override) :foreground blue)
   (org-hide :foreground bg)
   (solaire-org-hide-face :foreground hidden)

   ;; mu4e
   ;; (mu4e-title-face :inherit 'outline-2)
   (mu4e-context-face :foreground green)
   (mu4e-header-highlight-face :weight 'bold :inherit 'hl-line)

   ((org-agenda-done &override) :strike-through t)
   ((org-scheduled-today &override) :foreground fg)
   (org-agenda-current-time :background base0 :inherit 'org-time-grid)

   ;; magit
   (magit-blame-culprit :foreground yellow)
   (magit-blame-header :foreground green)
   (magit-blame-sha1 :foreground yellow)
   (magit-blame-subject :foreground yellow)
   (magit-blame-time :foreground green)
   (magit-blame-name :foreground yellow)
   (magit-blame-heading :foreground green)
   (magit-blame-hash :foreground yellow)
   (magit-blame-summary :foreground yellow)
   (magit-blame-date :foreground green)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-other :foreground cyan)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground cyan)
   (magit-reflog-reset :foreground red)
   (magit-branch :foreground magenta :weight 'bold)
   (magit-branch-current :foreground blue :weight 'bold :box t)
   (magit-branch-local :foreground blue :weight 'bold)
   (magit-branch-remote :foreground orange :weight 'bold)
   (magit-diff-file-header :foreground yellow)
   (magit-diff-file-heading :weight 'normal)
   (magit-diff-file-heading-highlight :weight 'bold :background base0)
   (magit-diff-file-heading-selection :weight 'bold)
   (magit-diff-hunk-heading :foreground yellow :weight 'normal)
   (magit-diff-hunk-heading-highlight :weight 'bold :background base0 :inherit 'magit-diff-hunk-heading)
   (magit-diff-hunk-heading-selection :inherit 'selection :weight 'bold)
   (magit-diff-lines-heading :foreground bg :background orange :extend t)
   (magit-hash :foreground yellow)
   (magit-item-highlight :background grey)
   (magit-log-author :foreground yellow)
   (magit-log-head-label-head :background yellow :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background red :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background green :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background magenta :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background cyan :foreground bg-alt :weight 'bold)
   (magit-log-sha1 :foreground green)
   (magit-process-ng :foreground orange :weight 'bold)
   (magit-process-ok :foreground yellow :weight 'bold)
   (magit-header-line :inherit 'outline-1)
   (magit-section-heading :foreground red :inherit 'outline-1)
   (magit-section-highlight :background base0)
   (section-heading-selection :foreground red :weight 'bold)
   (magit-section-title :background bg-alt :foreground red :weight 'bold)
   (eldoc-highlight-function-argument :background bg-alt :weight 'bold)
   ;; (magit-cherry-equivalent :foreground magenta)
   ;; (magit-cherry-unmatched :foreground cyan)
   ;; (magit-reflog-checkout :foreground blue)
   ;; (magit-reflog-cherry-pick :foreground green)
   ;; (magit-bisect-bad :foreground red)
   ;; (magit-bisect-good :foreground green)
   ;; (magit-bisect-skip :foreground fg)
   ;; (magit-diff-conflict-heading :foreground fg)
   (magit-dimmed :foreground fg-alt)
   ((forge-topic-closed &override) :strike-through t :inherit 'magit-dimmed)
   ((forge-topic-merged &override) :strike-through t :inherit 'magit-dimmed)
   ;; (magithub-ci-no-status :foreground grey)
   ;; (magithub-issue-number :foreground fg)
   ;; (magithub-notification-reason :foreground fg)
   )

  ;; --- extra variables ---------------------
  ()
  )

(provide-theme 'ewal-doom-dark)

;;; ewal-doom-dark-theme.el ends here
