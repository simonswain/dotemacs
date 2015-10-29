(require 'package)
(add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(better-defaults paredit idle-highlight-mode ido-ubiquitous magit smex flycheck))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))


(require 'smex)
(global-set-key (kbd "M-x") 'smex)

;;; opts

(mouse-avoidance-mode 'none)
(delete-selection-mode 1)
(idle-highlight-mode 1)
(setq column-number-mode 1)

;; Use only spaces (no tabs at all).
(setq-default indent-tabs-mode nil)

;; 
(setq inhibit-splash-screen t)
(setq initial-scratch-message "")
(setq initial-major-mode 'text-mode)

;;; layout
(load-theme ' tango-dark)
(set-background-color "black")

;; ido
(require 'ido)
(ido-mode t)

(setq
 ido-case-fold  t                 ; be case-insensitive
 ido-enable-last-directory-history t ; remember last used dirs
 ido-max-work-directory-list 30   ; should be enough
 ido-max-work-file-list      50   ; remember many
 ido-use-filename-at-point nil    ; don't use filename at point (annoying)
 ido-use-url-at-point nil         ; don't use url at point (annoying)
 ido-confirm-unique-completion t) ; wait for RET, even with unique completion

;;; keys
(define-key global-map (kbd "C-+") 'text-scale-increase)
(define-key global-map (kbd "C--") 'text-scale-decrease)

(global-set-key (kbd "M-/") 'hippie-expand)

(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c w") 'whitespace-cleanup)
(global-set-key (kbd "C-x g") 'magit-status)

(defun console-log ()
  (interactive)
  (insert "console.log();")
  (backward-char 2)
  (indent-for-tab-command))
(global-set-key (kbd "C-c l") 'console-log)

;; arrows to change frame
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

(defun my-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

(require 'flycheck)

(eval-after-load 'js
  '(progn
     (define-key js-mode-map "{" 'paredit-open-curly)
     (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
     (add-hook 'js-mode-hook 'my-paredit-nonlisp)
     (setq js-indent-level 2)
     ;; fixes problem with pretty function font-lock
     (define-key js-mode-map (kbd ",") 'self-insert-command)
     ))

(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
