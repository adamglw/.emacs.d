;; Initialise package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialise use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; Load no-littering package
;; Puts backup files (file~) in /var/backup/
(use-package no-littering)

;; Put auto-save files (#file#) in /var/auto-save
;(setq auto-save-file-name-transforms
;      '((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Disable lock files (.#file) with this option
;; (setq create-lockfiles nil)

; need swiper package
; need counsel package

(use-package ivy
  :diminish ; Keep Ivy out of modeline
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
:config
(ivy-mode 1))

;; Basic UI config
(setq inhibit-startup-message t)

(scroll-bar-mode -1)    ; Disable visible scrollbar
(tool-bar-mode -1)      ; Disable the toolbar
(tooltip-mode -1)       ; Disable tooltips
(set-fringe-mode 1)     ; Minimal side fringes
(menu-bar-mode -1)      ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Set font and size
(set-face-attribute 'default nil :font "Hack Nerd Font" :height 105)

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Column and line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Rainbow delimiters
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; which-key to show available commands in minibuffer
(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))

;; ivy-rich: A more friendly inferface for ivy
(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

;; Doom modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom (doom-modeline-height 15))

;; Doom themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, itaics are universally disabled
  (load-theme 'doom-gruvbox t))
