;;; package --- Init.el
(require 'package)
;;(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(better-defaults paredit idle-highlight-mode ido-ubiquitous magit smex flycheck auto-complete less-css-mode))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;;; opts
(mouse-avoidance-mode 'none)
(delete-selection-mode 1)
(idle-highlight-mode 1)
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)
(setq column-number-mode 1)
(setq confirm-kill-emacs 'y-or-n-p)
(auto-fill-mode -1)
(setq select-active-regions nil)      ;; don't copy on select
(setq-default indent-tabs-mode nil)   ;; Use only spaces (no tabs at all).
(setq tab-width 2)
(setq c-basic-indent 2)
(setq css-indent-offset 2)

(load-theme ' tango-dark)
(set-background-color "black")

(require 'ido)
(ido-mode t)

(setq
 ido-case-fold  t                    ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30      ; should be enough
 ido-max-work-file-list      50      ; remember many
 ido-use-filename-at-point nil       ; don't use filename at point (annoying)
 ido-use-url-at-point nil            ; don't use url at point (annoying)
 ido-confirm-unique-completion t)    ; wait for RET, even with unique completion

;;; keys
(fset 'yes-or-no-p 'y-or-n-p)
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-x g") 'magit-status)

(defun revert-buffer-no-confirm ()
  "Revert buffer without confirmation."
  (interactive)
  (revert-buffer t t))
(global-set-key (kbd "C-x r") 'revert-buffer-no-confirm)

(fset 'dup-line
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 67108896 down 134217847 25 up] 0 "%d")) arg)))
(global-set-key (kbd "C-c .") 'dup-line)

(defun console-log ()
  (interactive)
  (insert "console.log();")
  (backward-char 2)
  (indent-for-tab-command))
(global-set-key (kbd "C-c l") 'console-log)

(defun clean-and-format ()
  (interactive)
  (save-excursion
    (whitespace-cleanup)
    (indent-region (point-min) (point-max))
    ))
(global-set-key (kbd "C-x j") 'clean-and-format )

(fset 'es6functionify
   (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([19 102 117 110 134217826 134217828 134217760 19 123 left 61 62 32 tab] 0 "%d")) arg)))
(global-set-key (kbd "C-c 6") 'es6functionify)

(defun rt-do-line-comments ()
  (setq comment-start "// ")
  (setq comment-end ""))
(add-hook 'c-mode-hook 'rt-do-line-comments)

(global-set-key [f5] 'save-buffer)
(global-set-key [f6] 'flycheck-list-errors)
(global-set-key [f8] 'kill-this-buffer)
(global-set-key [f9] 'clean-and-format)

;; arrows to change frame
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))

(require 'flycheck)
(global-flycheck-mode 1)

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

(flycheck-define-checker javascript-semistandard
  "A Javascript code and style checker for the (Semi-)Standard Style."
  :command ("semistandard" "--stdin")
  :standard-input t
  :error-patterns
  ((error line-start "  <text>:" line ":" column ":" (message) line-end))
  :modes (js-mode js-jsx-mode js2-mode js2-jsx-mode js3-mode))

(eval-after-load 'js
  '(progn
     (define-key js-mode-map "{" 'paredit-open-curly) 
     (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
     (add-hook 'js-mode-hook 'my-paredit-nonlisp)
     (setq js-indent-level 2)
     (set 'js-switch-indent-offset 2)
     (global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
     (delete-selection-mode 1)
     (define-key js-mode-map (kbd ",") 'self-insert-command)
     ))

(add-hook 'js-mode-hook
          (lambda ()
            (flycheck-select-checker 'javascript-semistandard)
            (flycheck-mode)))

;; (add-hook 'js-mode-hook
;;           (lambda () (flycheck-mode t)))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

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

(set-face-foreground 'minibuffer-prompt "white")
