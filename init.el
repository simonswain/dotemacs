(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

;; Add in your own as you wish:

(defvar my-packages '(starter-kit starter-kit-lisp starter-kit-bindings starter-kit-js sws-mode jade-mode less-css-mode php-mode markdown-mode expand-region emmet-mode auto-complete flymake-jshint flymake-cursor nodejs-repl flycheck exec-path-from-shell)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(custom-set-variables
 '(ediff-split-window-function (quote split-window-horizontally))
 )

(setenv "NODE_NO_READLINE" "1")

; http://blog.deadpansincerity.com/2011/05/setting-up-emacs-as-a-javascript-editing-environment-for-fun-and-profit/

(require 'auto-complete-config)
; Make sure we can find the dictionaries
(add-to-list 'ac-dictionary-directories "~/emacs/auto-complete/dict")
; Use dictionaries by default
(setq-default ac-sources (add-to-list 'ac-sources 'ac-source-dictionary))
(global-auto-complete-mode t)
; Start auto-completion after 2 characters of a word
(setq ac-auto-start 2)
; case sensitivity is important when finding matches
(setq ac-ignore-case nil)

;;(setq debug-on-error t)

(require 'flymake-jshint)
(require 'flymake-cursor)
(add-hook 'js-mode-hook 'flymake-mode)

(setq-default indent-tabs-mode nil)

(load "~/.emacs.d/web-mode.el")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
)
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; ;; npm install -g jsxhint
;; (flycheck-define-checker jsxhint-checker
;;   "A JSX syntax and style checker based on JSXHint."

;;   :command ("jsxhint" source)
;;   :error-patterns
;;   ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
;;   :modes (web-mode))
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (equal web-mode-content-type "jsx")
;;               ;; enable flycheck
;;               (flycheck-select-checker 'jsxhint-checker)
;;               (flycheck-mode))))


(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; Auto-start on any markup modes
(add-hook 'css-mode-hook  'emmet-mode) ;; enable Emmet's css abbreviation.

(mouse-avoidance-mode 'none)
(delete-selection-mode 1)
(set-face-foreground 'minibuffer-prompt "white")

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(load-theme ' tango-dark)
(set-background-color "black")

(global-whitespace-mode 1)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)

(auto-fill-mode -1)

;; Use only spaces (no tabs at all).
(setq-default indent-tabs-mode nil)

(global-set-key "\M-n" 'scroll-up-line)
(global-set-key "\M-p" 'scroll-down-line)

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

(global-hl-line-mode -1)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(set-default-font "-unknown-Ubuntu Mono-normal-normal-normal-*-18-*-*-*-m-0-iso10646-1")

(set-frame-position (selected-frame) 0 0)

(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)
