;; -*- mode: lisp; -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some usage notes — beyond obvious known navigation keyboard shortcuts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Reloading config file:
;;   - `M-x eval-buffer` while in this file reloads config without restart
;;
;; Navigation:
;;   - `M-r` moves cursor to top, middle, & bottom of current window.
;;     - (https://emacs.stackexchange.com/a/36849)
;;   - `M-g g X` is equivalent to `M-x goto-line X`
;;
;; Multiple cursors:
;;   - `C-c e` to edit all lines in region
;;   - `C-c n` to select next occurrence of region
;;   - `C-c p` to select previous occurrence of region
;;   - `C-c a` to select all occurrences of region
;;
;; Other:
;;   - `C-h` is the help prefix key. `C-h k` is used to describe a keybinding.
;;   - `C-/` toggles comments on current line or selected lines. See below.
;;
;; TODO should probs check out https://www.masteringemacs.org/ for more.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up MELPA (https://www.emacswiki.org/emacs/MELPA)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package initialization
;;
;; Grabbed this section from https://stackoverflow.com/a/55058934/5802691
;;
;; This section should install desired packages when initializing emacs if they
;; haven't been installed. Add new packages to `my-packages` below. Note that
;; sometimes you may need to run `M-x package-refresh-contents` to update the
;; package list, as indicated in the comments below.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init the package facility
(require 'package)
(package-initialize)
;; (package-refresh-contents)
;; ^this line is commented since refreshing packages is time-consuming and
;; should be done on demand, i.e. M-x package-refresh-contents

;; Declare packages
;; [1] https://github.com/zerolfx/copilot.el
;; [2] https://github.com/magnars/multiple-cursors.el
(setq my-packages
      '(dash  ;; added per [1]
	diff-hl
	editorconfig  ;; added per [1]
	f  ;; added per [1]
	multiple-cursors  ;; [2]
	s  ;; added per [1]
	solarized-theme
	undo-tree
	xclip))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Install copilot.el. Following instructions from:
;;  - https://github.com/zerolfx/copilot.el
;;  - https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;;
;; Note: this following line assumes that `~/.emacs` is a symlink to this file,
;; & that `copilot.el/` is a sibling directory to this file.
(add-to-list 'load-path (expand-file-name "copilot.el" (file-name-directory (file-truename "~/.emacs"))))
(require 'copilot)

(require 'multiple-cursors)
(require 'undo-tree)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Use diff-hl-mode in all buffers; this shows VC (e.g. Git) uncommitted changes
;; on left-side of emacs windows
;; https://github.com/dgutov/diff-hl
(global-diff-hl-mode)
(diff-hl-margin-mode 1)
(diff-hl-flydiff-mode 1)
;; With ChatGPT help (!):
;;   - buffer-list-update-hook runs every time the buffer list is updated (e.g.
;;     when switching buffers), so diff-hl-update [1] will be run now too.
;;   - I also defined my own timer fxn (runs every 2 sec) to continuously update
;;     the diff-hl state based on potential changes to file's git status per
;;     commands done outside of emacs (e.g. if I `git add` & `git commit` a
;;     file, before this timer the diff on left side of file wouldn't update
;;     until buffer was refreshed). Technically this is does what `(add-hook *)`
;;     line does below, so `buffer-list-update-hook` is superfluous, but keeping
;;     around just in case I need to comment out timer fxn for performance
;;     reasons. Whole thing is wrapped in "start timer" fxn to ensure only one
;;     of these timers is running at once (e.g. while debugging &
;;     M-x eval-buffer'ing this file a bunch of times in a row, can spawn many
;;     timers, which can be trickier to debug).
;; [1] https://github.com/dgutov/diff-hl#integration
(add-hook 'buffer-list-update-hook 'diff-hl-update)
(defun continuous-diff-hl ()
  (funcall 'diff-hl-update))
(defun start-running-timer ()
  (when (boundp 'running-timer)
    (cancel-timer running-timer))
  (setq running-timer
	(run-at-time nil 2 'continuous-diff-hl)))  ;; run every 2 seconds
(start-running-timer)

;; Set solarized dark color theme
;; https://github.com/bbatsov/solarized-emacs
(load-theme 'solarized-dark t)

;; Enable xclip-mode by default
;; https://elpa.gnu.org/packages/xclip.html
(xclip-mode 1)

;;;; GH Copilot
;; Use copilot-mode in any programming mode (i.e. derived from prog-mode)
(add-hook 'prog-mode-hook 'copilot-mode)

;; tab key
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

;; disable copilot warning about indent offset:
;; https://github.com/copilot-emacs/copilot.el/blob/733bff26450255e092c10873580e9abfed8a81b8/copilot.el#L111C12-L111C49
(setq copilot-indent-offset-warning-disable t)

;; TODO consider going through [1] & setting up more copilot stuff
;; [1] https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

;;;; Multiple cursors
;; mostly setting default configuration in repo README
;; https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-c e") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)

;;;; Undo tree
;; https://www.emacswiki.org/emacs/UndoTree
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)

;; Save undo tree history files in a ~/.emacs.d instead of current directory.
(setq undo-tree-history-directory-alist
      '(("." . "~/.emacs.d/undo-tree-histories/")))

;; Custom code from ChatGPT. This makes it so that when I'm
;; in undo-tree-visualizer & I hit RET, it quits undo-tree-visualizer, kills the
;; undo-tree buffer, & restores the previous window configuration.
(defun my/undo-tree-visualizer-quit ()
  "Quit undo-tree-visualizer, kill undo-tree buffer, and restore the previous window configuration."
  (interactive)
  (let ((buffer-to-restore (other-buffer (current-buffer))))
    ;; Quit undo-tree-visualizer
    (undo-tree-visualizer-quit)
    ;; Kill the undo-tree buffer
    (kill-buffer "*undo-tree*")
    ;; Restore the other buffer in the current window
    (set-window-buffer (selected-window) buffer-to-restore)))

(with-eval-after-load 'undo-tree
  (define-key undo-tree-visualizer-mode-map (kbd "RET") 'my/undo-tree-visualizer-quit))


;;;;;;;;;;;;;;;;;
;; Other stuff ;;
;;;;;;;;;;;;;;;;;

;; Display vertical line at column 80
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Displaying-Boundaries.html
(global-display-fill-column-indicator-mode)
(setq-default display-fill-column-indicator-column 80)
;; note: this has to be HTML code of unicode character, see https://unicode-table.com/
(setq-default display-fill-column-indicator-character 9474)

;; Display current line & column number
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Displaying-the-current-line-or-column.html
(global-display-line-numbers-mode 1)
(setq column-number-mode t)

;; Have C-v & M-v move cursor to top & bottom of buffer
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Scrolling.html
(setq scroll-error-top-bottom 'true)

;; Have Emacs create no backup files.
;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
;;
;; TODO: should probably try to actually save stuff under something like
;; ~/.emacs/backups or something, but I haven't been able to get this all
;; working correctly. See above answer & https://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
;; for more.
(setq make-backup-files nil)

;; Auto-refresh all buffers when files have changed on disk
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Revert.html
;; https://stackoverflow.com/a/1481706
(setq auto-revert-interval 2)  ;; 2 seconds
(global-auto-revert-mode t)

;; Splitting window horizontal & vertically (similar bindings as in `.tmux`)
(global-set-key (kbd "C-x -") 'split-window-below)
(global-set-key (kbd "C-x |") 'split-window-right)

;; Turn off menu-bar-mode
(menu-bar-mode -1)

;; Set default python indent to 4 spaces
(setq python-indent-offset 4)
;; from https://stackoverflow.com/a/51966682, don't want this emacs warning
;; to show up.
(setq python-indent-guess-indent-offset-verbose nil)

;; Turn off startup message
;; from: https://emacs.stackexchange.com/a/437
(defun display-startup-echo-area-message () (message nil))

;; Keep search strings highlighted
;; https://stackoverflow.com/a/3780053
(setq lazy-highlight-cleanup nil)

;; Remap C-/ to comment-line, toggling current/selected line(s) to comment(s).
;;
;; Note that per [1], C-/ & C-_ are bound to `undo` by default, & C-/ actually
;; just sends C-_ in some terminals (C-h k C-/ to test this). I don't use undo
;; with these keybindings, so I'm fine updating it to comment toggle command,
;; which also is idiomatic in other editors like VSCode. However, because I use
;; undo-tree, this remapping needs to be done after undo-tree is loaded.
;;
;; [1] https://www.gnu.org/software/emacs/manual/html_node/emacs/Basic-Undo.html
(with-eval-after-load 'undo-tree
  (define-key undo-tree-map (kbd "C-_") #'comment-line)
  (define-key undo-tree-map (kbd "C-/") #'comment-line))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff that was automatically added
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(diff-hl f undo-tree s dash editorconfig solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Added after first time I ran M-x list-timers
(put 'list-timers 'disabled nil)
