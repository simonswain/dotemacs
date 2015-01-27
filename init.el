(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings starter-kit-js sws-mode jade-mode less-css-mode php-mode markdown-mode clojure-mode clojure-test-mode nrepl expand-region emmet-mode)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(mouse-avoidance-mode 'none)
(delete-selection-mode 1)

(set-face-foreground 'minibuffer-prompt "white")

(add-to-list 'load-path "~/emacs.d/emmet-mode")

(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(load-theme ' tango-dark)
(set-background-color "black")

(auto-fill-mode -1)

;; Use only spaces (no tabs at all).
(setq-default indent-tabs-mode nil)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

(require 'clojure-mode)
(require 'nrepl)

(require 'sws-mode)
(require 'jade-mode)    
(add-to-list 'auto-mode-alist '("\\.styl$" . sws-mode))
(add-to-list 'auto-mode-alist '("\\.jade$" . jade-mode))


(global-font-lock-mode t) 
(show-paren-mode 1) 
(add-hook 'lisp-mode-hook '(lambda () 
                             (local-set-key (kbd "RET") 'newline-and-indent)))

;;(remove-hook 'text-mode-hook 'turn-on-auto-fill)

(setq 
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

(setq column-number-mode 1)
(setq flymake-gui-warnings-enabled nil)

(defun console-log ()
  (interactive)
  (insert "console.log();")
  (backward-char 2)
  (indent-for-tab-command))

(global-set-key (kbd "C-c l") 'console-log)

(defun rt-do-line-comments ()
  (setq comment-start "// ")
  (setq comment-end ""))
(add-hook 'c-mode-hook 'rt-do-line-comments)

(remove-hook 'prog-mode-hook 'esk-turn-on-hl-line-mode)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(set-default-font "-unknown-Ubuntu Mono-normal-normal-normal-*-15-*-*-*-m-0-iso10646-1")

(set-frame-position (selected-frame) 0 0) 
