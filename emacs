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
;; Should probs check out https://www.masteringemacs.org/ for more.
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
(setq my-packages
      '(diff-hl
	solarized-theme
	xclip))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

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
;;     reasons.
;; [1] https://github.com/dgutov/diff-hl#integration
(add-hook 'buffer-list-update-hook 'diff-hl-update)
;; (setq counter 0)  ;; debug
(defun run-diff-hl-continuously ()
  ;; (message "ran %s time(s)" counter)  ;; debug
  ;; (setq counter (+ counter 1))  ;; debug
  (funcall 'diff-hl-update))
(run-at-time nil 2 'run-diff-hl-continuously) ;; run every 2 seconds

;; Set solarized dark color theme
;; https://github.com/bbatsov/solarized-emacs
(load-theme 'solarized-dark t)

;; Enable xclip-mode by default
;; https://elpa.gnu.org/packages/xclip.html
(xclip-mode 1)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff that was automatically added
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(solarized-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Added after first time I ran M-x list-timers
(put 'list-timers 'disabled nil)
