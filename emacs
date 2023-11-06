;; -*- mode: lisp; -*-
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some usage notes (beyond obvious known navigation ones)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; `M-r` moves cursor to top, middle, & bottom of current window.
;; (https://emacs.stackexchange.com/a/36849)
;;
;; `M-g g X` is equivalent to `M-x goto-line X`
;;
;; `M-x eval-buffer` while in emacs config file reloads config without restart
;;
;; `C-h` is the help prefix key. e.g. `C-h k` is used to describe a keybinding.
;;
;; TODO Should probs check out https://www.masteringemacs.org/ for more.
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
;; tl;dr This should install desired packages when initializing emacs if they
;; haven't been installed. Add new packages to `my-packages` below.
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
	multiple-cursors  ;; [2]
	s  ;; added per [1]
	solarized-theme
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
;; Packages ;;
;;;;;;;;;;;;;;

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

;; TODO consider going through [1] & setting up more copilot stuff
;; [1] https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/

;;;; Multiple cursors
;; mostly setting default configuration in repo README
;; https://github.com/magnars/multiple-cursors.el
(global-set-key (kbd "C-c e") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff that was automatically added
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(s dash editorconfig solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Added after first time I ran M-x list-timers
(put 'list-timers 'disabled nil)
