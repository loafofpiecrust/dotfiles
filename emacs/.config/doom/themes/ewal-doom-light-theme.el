;;; ewal-doom-light-theme.el -*- lexical-binding: t; no-byte-compile: t;-*-

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

;; An `ewal'-based theme, created using `doom-light' as its base.

;;; Code:
(require 'ewal)
(require 'ewal-doom-themes)

(defgroup ewal-doom-light-theme nil
  "Options for doom-themes"
  :group 'doom-themes)

(defcustom ewal-doom-light-brighter-modeline nil
  "If non-nil, more vivid colors will be used to style the mode-line."
  :group 'ewal-doom-light-theme
  :type 'boolean)

(defcustom ewal-doom-light-brighter-comments t
  "If non-nil, comments will be highlighted in more vivid colors."
  :group 'ewal-doom-light-theme
  :type 'boolean)

(defcustom ewal-doom-light-comment-bg ewal-doom-light-brighter-comments
  "If non-nil, comments will have a subtle, darker background."
  :group 'ewal-doom-light-theme
  :type 'boolean)

(defcustom ewal-doom-light-padded-modeline doom-themes-padded-modeline
  "If non-nil, adds a 4px padding to the mode-line.
Can be an integer to determine the exact padding."
  :group 'ewal-doom-light-theme
  :type '(choice integer boolean))

(ewal-load-colors)

;; HACK: fixes bytecode overflow
(defvar ewal-doom-light-hack
  (ewal-doom-themes-get-color 'background 0))

(def-doom-theme ewal-doom-light
  "A dark theme inspired by Atom One Dark, cutomized with `ewal'."

  ;; name        default   256       16
  ((bg         (ewal-doom-themes-get-color 'white +10))
   (bg-alt     (ewal-doom-themes-get-color 'white +5))
   (base0      (ewal-doom-themes-get-color 'foreground -5))
   (base1      (ewal-doom-themes-get-color 'foreground -4))
   (base2      (ewal-doom-themes-get-color 'foreground -2))
   (base3      (ewal-doom-themes-get-color 'foreground -1))
   (base4      (ewal-doom-themes-get-color 'foreground +1))
   (base5      (ewal-doom-themes-get-color 'comment     0))
   (base6      (ewal-doom-themes-get-color 'foreground +4))
   (base7      (ewal-doom-themes-get-color 'foreground +5))
   (base8      (ewal-doom-themes-get-color 'background +1))
   (fg         (ewal-doom-themes-get-color 'background  0))
   (fg-alt     (ewal-doom-themes-get-color 'background -1))

   (grey       (ewal-doom-themes-get-color 'background +3))
   (red        (ewal-doom-themes-get-color 'red      0))
   (orange     (ewal-doom-themes-get-color 'red       -2))
   (green      (ewal-doom-themes-get-color 'green    0))
   (teal       (ewal-doom-themes-get-color 'green     -3))
   (yellow     (ewal-doom-themes-get-color 'yellow   -2))
   (dark-yellow     (ewal-doom-themes-get-color 'yellow   -3))
   (blue       (ewal-doom-themes-get-color 'blue      +2))
   (dark-blue  (ewal-doom-themes-get-color 'blue     0))
   (magenta    (ewal-doom-themes-get-color 'magenta   0))
   (violet     (ewal-doom-themes-get-color 'magenta  -2))
   (cyan       (ewal-doom-themes-get-color 'cyan      -1))
   (dark-cyan  (ewal-doom-themes-get-color 'cyan     -2))

   ;; face categories -- required for all themes
   (highlight      blue)
   (vertical-bar   (doom-darken base1 0.1))
   (selection      dark-blue)
   (builtin        magenta)
   (comments       base5)
   (doc-comments   teal)
   (constants      orange)
   (functions      magenta)
   (keywords       dark-blue)
   (methods        cyan)
   (operators      dark-cyan)
   (type           dark-yellow)
   (strings        green)
   (variables      violet)
   (numbers        orange)
   (region         `(,(doom-darken (car bg-alt) 0.10) ,@(doom-darken (cdr base1) 0.30)))
   (error          red)
   (warning        yellow)
   (success        green)
   (vc-modified    orange)
   (vc-added       green)
   (vc-deleted     red)

   ;; custom categories
   (hidden     `(,(car bg) "black" "black"))
   (-modeline-bright ewal-doom-light-brighter-modeline)
   (-modeline-pad
    (when ewal-doom-light-padded-modeline
      (if (integerp ewal-doom-light-padded-modeline) ewal-doom-light-padded-modeline 4)))

   (modeline-fg     nil)
   (modeline-fg-alt base5)

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

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground base4)
   ((line-number-current-line &override) :foreground orange)

   (hl-fill-column-face :background bg-alt :foreground fg-alt)

   (font-lock-comment-face
    :inherit 'fixed-pitch-serif
    :slant 'italic
    :foreground comments
    :background (if ewal-doom-light-comment-bg (doom-lighten bg 0.05)))
   (font-lock-doc-face
    :inherit 'font-lock-comment-face
    :foreground doc-comments)

   (mode-line
    :background modeline-bg :foreground modeline-fg
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg)))
   (mode-line-inactive
    :background modeline-bg-inactive :foreground modeline-fg-alt
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive)))
   (mode-line-emphasis
    :foreground (if -modeline-bright base8 highlight))

   (solaire-mode-line-face
    :inherit 'mode-line
    :background modeline-bg-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-l)))
   (solaire-mode-line-inactive-face
    :inherit 'mode-line-inactive
    :background modeline-bg-inactive-l
    :box (if -modeline-pad `(:line-width ,-modeline-pad :color ,modeline-bg-inactive-l)))

   ;; Doom modeline
   ;; (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))
   ;; (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   ;; (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   ;; (doom-modeline-buffer-project-root :foreground green :weight 'bold)

;;;;; whitespace
   (whitespace-indentation :inherit 'default)
   (whitespace-big-indent :inherit 'default)

   ;; ivy-mode
   (ivy-current-match :background base4 :distant-foreground blue :weight 'normal)
   (ivy-posframe :background base2 :foreground fg)
   (internal-border :background fg-alt)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground orange)
   (css-property             :foreground green)
   (css-selector             :foreground blue)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground red)
   (markdown-code-face :background (doom-lighten base3 0.05))

   ;; org-mode
   (org-hide :foreground hidden)
   (solaire-org-hide-face :foreground hidden)

   ;; magit
   (magit-blame-culprit :foreground cyan)
   (magit-blame-header :foreground green)
   (magit-blame-sha1 :foreground cyan)
   (magit-blame-subject :foreground cyan)
   (magit-blame-time :foreground green)
   (magit-blame-name :foreground cyan)
   (magit-blame-heading :foreground green)
   (magit-blame-hash :foreground cyan)
   (magit-blame-summary :foreground cyan)
   (magit-blame-date :foreground green)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-reflog-amend :foreground magenta)
   (magit-reflog-other :foreground yellow)
   (magit-reflog-rebase :foreground magenta)
   (magit-reflog-remote :foreground yellow)
   (magit-reflog-reset :foreground red)
   (magit-branch :foreground magenta :weight 'bold)
   (magit-branch-current :foreground blue :weight 'bold :box t)
   (magit-branch-local :foreground blue :weight 'bold)
   (magit-branch-remote :foreground orange :weight 'bold)
   (magit-diff-file-header :foreground yellow)
   (magit-diff-file-heading :foreground blue :weight 'light)
   (magit-diff-file-heading-highlight :foreground blue :weight 'bold)
   (magit-diff-file-heading-selection :foreground blue :weight 'bold :background base1)
   (magit-diff-hunk-heading :foreground yellow :weight 'light)
   (magit-diff-hunk-heading-highlight :foreground yellow :weight 'bold)
   (magit-diff-hunk-heading-selection :inherit 'selection :weight 'bold)
   (magit-diff-added :foreground green :weight 'light)
   (magit-diff-removed :foreground red :weight 'light)
   (magit-diff-context :foreground fg :weight 'light)
   (magit-diff-added-highlight :foreground green :weight 'bold)
   (magit-diff-removed-highlight :foreground red :weight 'bold)
   (magit-diff-context-highlight :foreground fg :weight 'bold)
   (magit-diff-base :foreground fg :weight 'light)
   (magit-diff-base-highlight :foreground fg :weight 'bold)
   (magit-diff-lines-boundary :background fg :foreground base2)
   (magit-diff-lines-heading :background fg :foreground base2)
   (magit-hash :foreground cyan)
   (magit-item-highlight :background grey)
   (magit-log-author :foreground cyan)
   (magit-log-head-label-head :background cyan :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background red :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background green :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background magenta :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background yellow :foreground bg-alt :weight 'bold)
   (magit-log-sha1 :foreground green)
   (magit-process-ng :foreground orange :weight 'bold)
   (magit-process-ok :foreground cyan :weight 'bold)
   (magit-section-heading :foreground red)
   (magit-section-highlight :weight 'bold)
   (section-heading-selection :foreground red :weight 'bold)
   (magit-section-title :background bg-alt :foreground red :weight 'bold)
   (magit-cherry-equivalent :foreground magenta)
   (magit-cherry-unmatched :foreground orange)
   (magit-reflog-checkout :foreground blue)
   (magit-reflog-cherry-pick :foreground green)
   (magit-bisect-bad :foreground red)
   (magit-bisect-good :foreground green)
   (magit-bisect-skip :foreground fg)
   (magit-diff-conflict-heading :foreground fg)
   (magit-dimmed :foreground base8)
   (magithub-ci-no-status :foreground grey)
   (magithub-issue-number :foreground fg)
   (magithub-notification-reason :foreground fg)
   )



  ;; --- extra variables ---------------------
  ;; ((evil-insert-state-cursor `(box ,red))
  ;;  (evil-normal-state-cursor `(box ,green)))
  )

(provide-theme 'ewal-doom-light)

;;; ewal-doom-light-theme.el ends here
