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

;; Use no-littering package to automatically set common paths to user-emacs-directory
;; Puts backup files (file~) in /var/backup/
(use-package no-littering)

;; Keep customisation settings out of init.el
(setq custom-file "~/.emacs.d/etc/custom.el")
(if (file-exists-p custom-file)
(load custom-file))

;; Put auto-save files (#file#) in /var/auto-save
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

;; Disable lock files (.#file) with this option
;; (setq create-lockfiles nil)

; Ivy completion framework, comes with Counsel and Swiper
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

;; Make ESC quit prompts
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

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

;; Configure Counsel keybindings
(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :config
  (setq ivy-initial-inputs-alist nil)) ; Don't start searches with ^

;; Helpful is an alternative to build in emacs help
(use-package helpful
;  :ensure t ; not needed as already turned on by default above
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap desribe-key] . helpful-key))

;; Doom modeline
(use-package doom-modeline
  :init (doom-modeline-mode 1)
;  :custom (doom-modeline-height 20)
)

;; Doom themes
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, itaics are universally disabled
  (load-theme 'doom-zenburn t)
)

;; general.el for defining keybindings
(use-package general
  :config
  (general-create-definer aw/leader-keys
    :keymaps '(normal insert visual emacs)
    :prefix "SPC"
    :global-prefix "C-SPC")

  (aw/leader-keys
    "t"  '(:ignore t :which-key "toggles")
    "tt" '(counsel-load-theme :which-key "choose theme")))


(defun aw/evil-hook ()
  "Configure evil mode"
  ;; Use emacs state for the following modes
  (dolist (mode '(term-mode
                  shell-mode
                  eshell-mode
                  git-rebase-mode))
    (add-to-list 'evil-emacs-state-modes mode)))

;; Evil mode
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  :config
  (add-hook 'evil-mode-hook 'aw/evil-hook)
  (evil-mode 1)
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal))

;; Collection of evil keybindings for other parts of emacs
(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; Hydra to tie related commands into a family of short bindings
(use-package hydra)

(defhydra hydra-text-scale (:timeout 4)
  "scale text"
  (izll"j" text-scale-increase "in")
  ("k" text-scale-decrease "out")
  ("f" nil "finished" :exit t))

(aw/leader-keys
  "ts" '(hydra-text-scale/body :which-key "scale text"))

;; Projectile is a project interaction library to provide easy project management and navigation
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (when (file-directory-p "~/code")
    (setq projectile-project-search-path '("~/code")))
  (setq projectile-switch-project-action #'projectile-dired))

;; Counsel-projectile provides further ivy integration by taking advantage of ivy's support for
;; selecting from a list of actions and applying an action without leaving the completion session.
(use-package counsel-projectile
  :config (counsel-projectile-mode))

;; Magit is a Git Porcelain inside Emacs
(use-package magit
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Forge allows you to work with Git forges, such as Github, from the comfort of Magit
;; and the rest of Emacs.
;(use-package forge)

(defun aw/org-mode-setup ()
  (org-indent-mode 0)
  (variable-pitch-mode 1)
  (auto-fill-mode 0)
  (visual-line-mode 1))

(use-package org
  :hook (org-mode . aw/org-mode-setup)
  :config
  (setq org-ellipsis " ▼"
	org-hide-emphasis-markers t))

;(evil-define-key '(normal insert) evil-org-mode-map (kbd "M-") 'org-meta-return)

(font-lock-add-keywords 'org-mode
			'(("^ *\\([-]\\) "
			   (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(let* ((variable-tuple
	(cond ((x-list-fonts "Source Sans 3 VF") '(:font "Source Sans 3 VF"))
	      ((x-family-fonts "Sans") '(:family "Sans"))
	      (nil (warn "Cannot find a Sans font.  Install one!"))))
          (headline `(:inherit default :weight bold)))


  (custom-theme-set-faces
   'user
   `(org-level-8 ((t (,@headline ,@variable-tuple))))
   `(org-level-7 ((t (,@headline ,@variable-tuple))))
   `(org-level-6 ((t (,@headline ,@variable-tuple))))
   `(org-level-5 ((t (,@headline ,@variable-tuple))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.10))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.15))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.20))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.25))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.25 :underline nil))))))

(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Source Sans 3 VF" :height 120))))
 '(fixed-pitch ((t ( :family "Hack Nerd Font" :height 105)))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t)))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch))))))

(defun aw/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
	visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :defer t
  :hook (org-mode . aw/org-mode-visual-fill))

(use-package org-roam
  :hook
  (after-init . org-roam-mode)
  :custom
  (org-roam-directory "~/org-roam")
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam)
	       ("C-c n f" . org-roam-find-file))))
