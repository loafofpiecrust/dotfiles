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
(require 'ct)

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

(defun +doom--contrast (a b &optional ratio)
  (if (consp a)
      (cons (ct-tint-ratio (car-safe a) (or (car-safe b) b) (or ratio 5))
            (cdr a))
    (ct-tint-ratio (or (car-safe a) a) (or (car-safe b) b) (or ratio 5))))

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
   (highlight      (+doom--contrast blue bg))
   (vertical-bar   (doom-darken base1 0.1))
   (selection      (+doom--contrast dark-blue bg))
   (builtin        (+doom--contrast yellow bg))
   (comments       (+doom--contrast dark-cyan bg))
   (doc-comments   (+doom--contrast teal bg))
   (constants      (+doom--contrast violet bg))
   (functions      (+doom--contrast magenta bg))
   (keywords       (+doom--contrast green bg))
   (methods        (+doom--contrast cyan bg))
   (operators      (+doom--contrast fg bg))
   (type           (+doom--contrast yellow bg))
   (strings        (+doom--contrast red bg))
   (variables      (+doom--contrast fg bg))
   (numbers        (+doom--contrast orange bg))
   (region base6)
   ;; (region         `(,(doom-lighten (car bg-alt) 0.15) ,@(doom-lighten (cdr base1) 0.35)))
   (error          (+doom--contrast red bg))
   (warning        (+doom--contrast yellow bg))
   (success        (+doom--contrast green bg))
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
   (header-line-bg base6)

   (distant-base (+doom--contrast fg bg))

   (light-highlight (+doom--contrast fg orange))
   (matching-bg (+doom--contrast functions fg))
   (matching-fg (+doom--contrast bright-yellow matching-bg))

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
   ((lazy-highlight &override) :background orange :foreground light-highlight :distant-foreground distant-base :weight 'normal)
   (lsp-face-highlight-textual :background matching-bg :foreground matching-fg :distant-foreground distant-base)
   (lsp-face-highlight-read :background matching-bg :foreground matching-fg :distant-foreground distant-base)
   (lsp-face-highlight-write :background matching-bg :foreground matching-fg :distant-foreground distant-base)
   ((lsp-ui-peek-highlight &override) :foreground fg)
   (cursor :background fg)
   (mini-modeline-mode-line :background nil)
   (doom-modeline-bar :background numbers :foreground bg :height 1.1 :weight 'bold)
   (aw-mode-line-face :foreground bg :weight 'bold)
   (doom-modeline-bar-inactive :height 1.1)
   (doom-modeline-battery-normal :inherit nil)

   (show-paren-match :background matching-bg :foreground matching-fg :weight 'bold)
   ((hl-todo &override) :foreground numbers)

   (highlight :foreground highlight :underline nil ;; :underline `(:style line :color ,blue)
              )
   ((link &override) :underline `(:style line :color ,highlight) :weight 'bold)

   ;; Turn all wavy underlines into straight ones for readability.

   ((avy-lead-face &override) :weight 'normal)

   ((cfw:face-title &override) :height 1.8 :box `(:line-width 4 :color ,bg))
   ((cfw:face-grid &override) :foreground base6)
   ((cfw:face-toolbar-button-off &override) :foreground fg-alt)
   ((cfw:face-sunday &override) :foreground strings)

   ;; Add padding to the minibuffer prompt, making it easier to read.
   ;; This is especially helpful for single-line prompts like passwords.
   ;; ((minibuffer-prompt &override) :box `(:line-width 4 :color ,bg;; base6
   ;;                                       ))

   (evil-goggles-default-face :inherit 'region :background (doom-blend region bg 0.5))

   ((line-number &override) :foreground comments :background region)
   ((line-number-current-line &override) :foreground fg :background bg)

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
    :foreground highlight)
   (doom-modeline-spc-face :inherit nil)

   ((header-line &override) :extend t :weight 'medium :background base6)
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
   ;; (doom-modeline-bar :background (if -modeline-bright modeline-bg highlight))

   (doom-modeline-buffer-file :inherit 'mode-line-buffer-id :weight 'bold)
   (doom-modeline-buffer-path :inherit 'mode-line-emphasis :weight 'bold)
   (doom-modeline-buffer-project-root :foreground success :weight 'bold)
   (doom-modeline-buffer-modified :foreground strings)

   ;; Evil tags
   (doom-modeline-evil-insert-state :background highlight :foreground bg :weight 'bold)
   (doom-modeline-evil-normal-state :background functions :foreground bg :weight 'bold)
   (doom-modeline-evil-visual-state :background numbers :foreground bg :weight 'bold)
   (doom-modeline-evil-replace-state :background strings :foreground bg :weight 'bold)
   (doom-modeline-evil-operator-state :background strings :foreground bg :weight 'bold)

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
   (evil-snipe-first-match-face :foreground base0 :background success)
   (evil-snipe-matches-face     :foreground success :underline t)

   ;; flycheck
   (flycheck-error   :underline `(:style line :color ,error)   )
   (flycheck-warning :underline `(:style line :color ,warning))
   (flycheck-info    :underline `(:style line :color ,success))
   (spell-fu-incorrect-face :underline `(:style line :color ,error))

   ;; rainbow-delimiters
   (rainbow-delimiters-depth-1-face :foreground functions)
   (rainbow-delimiters-depth-2-face :foreground success)
   (rainbow-delimiters-depth-3-face :foreground methods)
   (rainbow-delimiters-depth-4-face :foreground numbers)
   (rainbow-delimiters-depth-5-face :foreground functions)
   (rainbow-delimiters-depth-6-face :foreground success)
   (rainbow-delimiters-depth-7-face :foreground methods)

   ;; --- major-mode faces -------------------
   ;; css-mode / scss-mode
   (css-proprietary-property :foreground numbers)
   (css-property             :foreground success)
   (css-selector             :foreground highlight)

   ;; markdown-mode
   (markdown-markup-face :foreground base5)
   (markdown-header-face :inherit 'bold :foreground strings)
   (markdown-code-face :background (doom-lighten base3 0.05))
   (markdown-blockquote-face :inherit 'italic :foreground selection)
   (markdown-list-face :foreground functions)
   (markdown-pre-face  :foreground methods)
   (markdown-link-face :inherit 'bold :foreground highlight)
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
   ((outline-1 &override) :foreground constants)
   ((outline-2 &override) :foreground keywords)
   ((outline-3 &override) :foreground numbers)
   ((outline-4 &override) :foreground type)
   ((outline-5 &override) :foreground methods)
   ((outline-6 &override) :foreground functions)
   ((outline-7 &override) :foreground highlight)
   (org-hide :foreground bg)
   (solaire-org-hide-face :foreground hidden)

   ;; mu4e
   ;; (mu4e-title-face :inherit 'outline-2)
   (mu4e-context-face :foreground keywords)
   (mu4e-header-highlight-face :weight 'bold :inherit 'hl-line)

   ((org-agenda-done &override) :strike-through t)
   ((org-scheduled-today &override) :foreground fg)
   (org-agenda-current-time :background base0 :inherit 'org-time-grid)

   ;; magit
   (magit-blame-culprit :foreground type)
   (magit-blame-header :foreground keywords)
   (magit-blame-sha1 :foreground type)
   (magit-blame-subject :foreground type)
   (magit-blame-time :foreground keywords)
   (magit-blame-name :foreground type)
   (magit-blame-heading :foreground keywords)
   (magit-blame-hash :foreground type)
   (magit-blame-summary :foreground type)
   (magit-blame-date :foreground keywords)
   (magit-log-date :foreground fg-alt)
   (magit-log-graph :foreground fg-alt)
   (magit-reflog-amend :foreground functions)
   (magit-reflog-other :foreground methods)
   (magit-reflog-rebase :foreground functions)
   (magit-reflog-remote :foreground methods)
   (magit-reflog-reset :foreground strings)
   (magit-branch :foreground functions :weight 'bold)
   (magit-branch-current :foreground highlight :weight 'bold :box t)
   (magit-branch-local :foreground highlight :weight 'bold)
   (magit-branch-remote :foreground numbers :weight 'bold)
   (magit-diff-file-header :foreground type)
   (magit-diff-file-heading :weight 'normal)
   (magit-diff-file-heading-highlight :weight 'bold :background base0)
   (magit-diff-file-heading-selection :weight 'bold)
   (magit-diff-hunk-heading :foreground type :weight 'normal)
   (magit-diff-hunk-heading-highlight :weight 'bold :background base0 :inherit 'magit-diff-hunk-heading)
   (magit-diff-hunk-heading-selection :inherit 'selection :weight 'bold)
   (magit-diff-lines-heading :foreground bg :background numbers :extend t)
   (magit-hash :foreground type)
   (magit-item-highlight :background grey)
   (magit-log-author :foreground type)
   (magit-log-head-label-head :background type :foreground bg-alt :weight 'bold)
   (magit-log-head-label-local :background strings :foreground bg-alt :weight 'bold)
   (magit-log-head-label-remote :background keywords :foreground bg-alt :weight 'bold)
   (magit-log-head-label-tags :background functions :foreground bg-alt :weight 'bold)
   (magit-log-head-label-wip :background methods :foreground bg-alt :weight 'bold)
   (magit-log-sha1 :foreground keywords)
   (magit-process-ng :foreground numbers :weight 'bold)
   (magit-process-ok :foreground type :weight 'bold)
   (magit-header-line :inherit 'outline-1)
   (magit-section-heading :foreground strings :inherit 'outline-1)
   (magit-section-highlight :background base0)
   (section-heading-selection :foreground strings :weight 'bold)
   (magit-section-title :background bg-alt :foreground strings :weight 'bold)
   (eldoc-highlight-function-argument :background bg-alt :weight 'bold)
   ;; (magit-cherry-equivalent :foreground functions)
   ;; (magit-cherry-unmatched :foreground cyan)
   ;; (magit-reflog-checkout :foreground highlight)
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
