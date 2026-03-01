;; -*- mode: lisp; indent-tabs-mode: nil; -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;                ███████╗███╗   ███╗ █████╗  ██████╗███████╗
;;                ██╔════╝████╗ ████║██╔══██╗██╔════╝██╔════╝
;;                █████╗  ██╔████╔██║███████║██║     ███████╗
;;                ██╔══╝  ██║╚██╔╝██║██╔══██║██║     ╚════██║
;;                ███████╗██║ ╚═╝ ██║██║  ██║╚██████╗███████║
;;                ╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚══════╝
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Usage notes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
;; Formatting this file:
;;   - Select all `C-x h`, then re-indent `M-x indent-region`
;;   - To convert tabs to spaces: `C-x h`, then `M-x untabify`
;;
;; C-h is the universal "tell me about..." help prefix:
;;   - `C-h v` describe a variable (e.g. `C-h v package-archives RET`)
;;   - `C-h f` describe a function (e.g. `C-h f untabify RET`)
;;   - `C-h k` describe a keybinding (e.g. `C-h k C-x h`)
;;   - `C-h o` describe any symbol (when unsure if it's a function or variable)
;;   - `C-h a` search for commands by keyword
;;   - `C-h ?` list all help commands
;;
;; Other:
;;   - `C-/` toggles comments on current line or selected lines. See below.
;;
;; See https://www.masteringemacs.org/ for more.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package initialization
;;
;; This section should install desired packages when initializing emacs if they
;; haven't been installed. Add new packages to `my-packages` below. Note that
;; sometimes you may need to run `M-x package-refresh-contents` to update the
;; package list, as indicated in the comments below.
;;
;; Inspired by https://stackoverflow.com/a/55058934/5802691
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Init the package facility & add MELPA
;; https://www.emacswiki.org/emacs/MELPA
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
;; (package-refresh-contents)
;; ^this line is commented since refreshing packages is time-consuming &
;; should be done on demand, i.e. M-x package-refresh-contents

;; Declare packages
(setq my-packages
      '(
        ;; https://github.com/copilot-emacs/copilot.el
        copilot
        ;; https://github.com/dgutov/diff-hl
        diff-hl
        ;; https://jblevins.org/projects/markdown-mode/
        markdown-mode
        ;; https://github.com/magnars/multiple-cursors.el
        multiple-cursors
        ;; https://www.emacswiki.org/emacs/UndoTree
        undo-tree
        ;; https://elpa.gnu.org/packages/xclip.html
        xclip
        ;; https://github.com/bbatsov/zenburn-emacs
        zenburn-theme
        ))

;; Iterate on packages & install missing ones
(dolist (pkg my-packages)
  (unless (package-installed-p pkg)
    (package-install pkg)))

;; Auto-remove installed packages not in my-packages or needed as transitive
;; dependencies. Removing a package from my-packages triggers cleanup on next
;; init.
(defun my/package-keep-list ()
  "Return my-packages plus all their transitive dependencies."
  (let ((keep '())
        (queue (append my-packages nil)))
    (while queue
      (let ((pkg (pop queue)))
        (unless (memq pkg keep)
          (push pkg keep)
          (when-let ((desc (cadr (assoc pkg package-alist))))
            (dolist (req (package-desc-reqs desc))
              (push (car req) queue))))))
    keep))

(let ((keep (my/package-keep-list)))
  (dolist (entry package-alist)
    (unless (memq (car entry) keep)
      (when-let ((desc (cadr entry)))
        (message "Auto-removing package: %s" (car entry))
        (package-delete desc t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; User configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Copilot setup.
;; https://github.com/copilot-emacs/copilot.el
(require 'copilot)
(setq copilot-indent-offset-warning-disable t)
(add-hook 'prog-mode-hook #'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") #'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB")   #'copilot-accept-completion)

;; Use diff-hl-mode in all buffers; this shows VC (e.g. Git) uncommitted changes
;; on left-side of emacs windows
;; https://github.com/dgutov/diff-hl
(global-diff-hl-mode)
(diff-hl-margin-mode 1)
(diff-hl-flydiff-mode 1)
;; Periodically refresh diff-hl to pick up changes made outside Emacs
;; (e.g. git add, git commit from a terminal).
(when (boundp 'my/diff-hl-timer)
  (cancel-timer my/diff-hl-timer))
(setq my/diff-hl-timer (run-at-time nil 2 #'diff-hl-update))

;; Set up theme
;; https://github.com/bbatsov/zenburn-emacs
(load-theme 'zenburn t)

;; Enable xclip-mode by default
;; https://elpa.gnu.org/packages/xclip.html
(xclip-mode 1)

;; Multiple cursors
;; Mostly setting default configuration in repo README.
;; https://github.com/magnars/multiple-cursors.el
(require 'multiple-cursors)
(global-set-key (kbd "C-c e") 'mc/edit-lines)
(global-set-key (kbd "C-c n") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c a") 'mc/mark-all-like-this)

;; Undo tree
;; https://www.emacswiki.org/emacs/UndoTree
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-visualizer-timestamps t)
;; Disable persistent undo history to avoid warnings when files change outside
;; emacs.
(setq undo-tree-auto-save-history nil)
;; Save undo tree history files in a ~/.emacs.d instead of current directory;
;; likely no longer needed since I disabled persistent undo history above.
(setq undo-tree-history-directory-alist
      '(("." . "~/.emacs.d/undo-tree-histories/")))
;; Custom function to quit undo-tree-visualizer. This makes it so that when I'm
;; in undo-tree-visualizer & I hit RET, it quits undo-tree-visualizer, kills the
;; undo-tree buffer, & restores the previous window configuration.
(defun my/undo-tree-visualizer-quit ()
  "Quit undo-tree-visualizer, kill undo-tree buffer, & restore the previous window configuration."
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
;; Stuff that's automatically added/edited. Probs shouldn't manually edit this,
;; other than explanatory comments.
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
