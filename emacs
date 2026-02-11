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
	markdown-mode
	multiple-cursors  ;; [2]
	s  ;; added per [1]
	undo-tree
	xclip
	zenburn-theme
	))

;; Iterate on packages and install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;;;; The following code block was written with the help of ChatGPT. Essentially
;;;; I want to auto-remove packages that are not in my-packages list, but I also
;;;; want to keep packages that are dependencies of the packages in my-packages.
;;;; This way I can guarantee that simply updating my-packages will do
;;;; appropriate install & cleanup of packages.
;; Function to get dependencies for a given package
(defun my/get-dependencies (pkg)
  "Return a list of packages that PKG depends on."
  (when-let ((desc (cadr (assoc pkg package-alist))))
    (mapcar #'car (package-desc-reqs desc))))
;; Build an extended list including dependencies
(setq my-extended-packages (copy-sequence my-packages))
(dolist (pkg my-packages)
  (dolist (dep (my/get-dependencies pkg))
    (add-to-list 'my-extended-packages dep)))
;; Use the extended list in your auto-removal snippet:
(dolist (pkg package-alist)
  (let ((pkg-name (car pkg))
	(pkg-desc (cadr pkg)))
    (unless (member pkg-name my-extended-packages)
      (when pkg-desc
	(message "Auto-removing package: %s" pkg-name)
	(package-delete pkg-desc t)))))


;; Install copilot.el. Following instructions from:
;;  - https://github.com/zerolfx/copilot.el
;;  - https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
;;
;; Note: this following line assumes that `~/.emacs` is a symlink to this file,
;; & that `copilot.el/` is a sibling directory to this file.
(add-to-list 'load-path (expand-file-name "copilot.el" (file-name-directory (file-truename "~/.emacs"))))
(require 'copilot)
;; Safe copilot setup.
;; Wrote this with ChatGPT, essentially wanted to automate process of installing
;; any copilot dependencies.
(condition-case cp-err
    (progn
      ;; 1) Make sure Emacs can find your nvm npm & the eventual language server
      (let ((nvm-bin (expand-file-name "~/.nvm/versions/node/v20.19.0/bin")))
	(when (file-directory-p nvm-bin)
	  (setenv "PATH" (concat nvm-bin ":" (getenv "PATH")))
	  (add-to-list 'exec-path nvm-bin)))

      ;; 2) Tell Emacs where to load copilot.el from (only once)
      (add-to-list 'load-path
		   (expand-file-name "copilot.el"
				     (file-name-directory (file-truename "~/.emacs"))))
      (require 'copilot)

      ;; 3) Install the language server if it isn’t on PATH yet
      (unless (executable-find "copilot-language-server")
	(if-let ((npm (executable-find "npm")))
	    (progn
	      (message "Installing @github/copilot-language-server…")
	      (shell-command (concat npm " install -g @github/copilot-language-server"))
	      (message "copilot-language-server installed"))
	  (message "npm not found; Copilot server install skipped")))

      ;; 4) Configure Copilot
      ;; disable copilot warning about indent offset:
      ;; https://github.com/copilot-emacs/copilot.el/blob/733bff26450255e092c10873580e9abfed8a81b8/copilot.el#L111C12-L111C49
      (setq copilot-indent-offset-warning-disable t)
      ;; Use copilot-mode in any programming mode (i.e. derived from prog-mode)
      (add-hook 'prog-mode-hook #'copilot-mode)

      (define-key copilot-completion-map (kbd "<tab>") #'copilot-accept-completion)
      (define-key copilot-completion-map (kbd "TAB")     #'copilot-accept-completion)
      ;; TODO consider going through [1] & setting up more copilot stuff
      ;; [1] https://robert.kra.hn/posts/2023-02-22-copilot-emacs-setup/
      )
  (error
   (message "Copilot setup failed, continuing init…\nError: %S" cp-err)))

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

;; Set up theme
;; https://github.com/bbatsov/zenburn-emacs
(load-theme 'zenburn t)

;; Enable xclip-mode by default
;; https://elpa.gnu.org/packages/xclip.html
(xclip-mode 1)

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

;; Disable persistent undo history to avoid warnings when files change outside
;; emacs.
(setq undo-tree-auto-save-history nil)
;; Save undo tree history files in a ~/.emacs.d instead of current directory;
;; likely no longer needed since I disabled persistent undo history above.
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

;; Ensure that auto-save & backup directories exist.
(dolist (dir '("~/.emacs.d/auto-save/" "~/.emacs.d/backups/"))
  (unless (file-directory-p (expand-file-name dir))
    (make-directory (expand-file-name dir) t)))

;; backups: enable backups & store them in ~/.emacs.d/backups
;; file format: "filename~"
(setq backup-directory-alist `((".*" . "~/.emacs.d/backups")))
(setq make-backup-files t)

;; auto-save: redirect auto-save files out of project dirs
;; file format: "#filename#"
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save/" t)))
(setq auto-save-interval 20   ;; auto-save after 20 keystrokes
      auto-save-timeout 5)    ;; auto-save after 5 seconds idle

;; lockfiles: disable creation of lockfiles
;; format: ".#filename"
(setq create-lockfiles nil)

;; auto-save: remove stale auto-save files after saving a file
(defun my/cleanup-stale-auto-save ()
  "Delete an existing auto-save file if it’s older than the file's last modification."
  (when (and buffer-auto-save-file-name
             (file-exists-p buffer-auto-save-file-name))
    (let ((auto-save-time (nth 5 (file-attributes buffer-auto-save-file-name)))
          (file-time (nth 5 (file-attributes (buffer-file-name)))))
      (when (and file-time auto-save-time
                 (time-less-p auto-save-time file-time))
        (delete-file buffer-auto-save-file-name)
        (message "Removed stale auto-save file for %s" (buffer-file-name))))))
(add-hook 'after-save-hook 'my/cleanup-stale-auto-save)

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

;; Force *Buffer List* to open in current window & not split horizontally.
(add-to-list 'display-buffer-alist
             '("\\*Buffer List\\*" (display-buffer-same-window)))

;; Highlight added/removed lines in commit messages
(add-hook 'find-file-hook
  (lambda ()
    (when (string-match-p "COMMIT_EDITMSG\\'" (buffer-name))
      (font-lock-add-keywords nil
       '(("^\\(+.*\\)$" 1 'diff-added nil t)
         ("^\\(-.*\\)$" 1 'diff-removed nil t)))
      (font-lock-flush))))

;; Remap M-<backspace> to backward-delete-word instead of its default which
;; copies the deleted word to the kill ring / clipboard.
(defun backward-delete-word (arg)
  "Delete words backward without saving to the kill ring."
  (interactive "p")
    (delete-region (point) (progn (backward-word arg) (point))))
(global-set-key (kbd "M-DEL") #'backward-delete-word)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)

;; Enable visual line mode globally, which makes long lines wrap without "\"
;; at the end of the line.
(global-visual-line-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Stuff that was automatically added
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(diff-hl f markdown-mode multiple-cursors undo-tree xclip
	     zenburn-theme)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; Added after first time I ran M-x list-timers
(put 'list-timers 'disabled nil)
