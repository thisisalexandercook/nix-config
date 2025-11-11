;;misc
(setq inhibit-startup-screen t)
(global-visual-line-mode 1)
(global-display-line-numbers-mode 1)
(delete-selection-mode 1)
(setq select-enable-clipboard t)

;; Top Bar Cleanup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; Disable auto-save files (#file#)
(setq auto-save-default nil)

;; Disable backup files (file~)
(setq make-backup-files nil)

;; Disable lockfiles (.#file)
(setq create-lockfiles nil)

;; eval-buffer on save of init.el
(defun reload-init-file-on-save ()
  "Reload init.el automatically after saving."
  (when (string-equal (file-truename user-init-file)
                      (file-truename buffer-file-name))
    (eval-buffer)
    (message "✅ init.el reloaded successfully.")))
(add-hook 'after-save-hook #'reload-init-file-on-save)

;; package manager
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; theme
(use-package modus-themes
  :ensure nil
  :init
  (setq modus-themes-italic-constructs t
        modus-themes-bold-constructs   t
        modus-themes-region 'bg-only
        modus-themes-org-blocks 'gray-background)
  ;; Dark theme: modus-vivendi. Light theme: modus-operandi
  (load-theme 'modus-operandi t))

;; orderless
(use-package orderless
  :ensure t
  :custom
  (completion-styles '(orderless basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion)))))

;; vertico
(use-package vertico
  :ensure t
  :init
  (vertico-mode))

;; marginalia
(use-package marginalia
  :ensure t
  :init (marginalia-mode))

;; consult
(use-package consult
  :ensure t
  :bind
  ("C-s" . consult-line)
  ("C-c i" . consult-imenu))

;; magit
(use-package magit
  :ensure t)

;; eat
(use-package eat
  :ensure t
  :custom
  (eat-term-name "xterm-256color")
  :hook
  (eshell-load . eat-eshell-mode))

;; winner mode
(use-package winner
  :ensure nil
  :config
  (winner-mode 1))

;; nix mode
(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; proof general
(use-package proof-general
  :ensure t
  :config
  (setq coq-prog-name "rocq")
  (setq coq-compile-before-require t))

;; company-coq
(use-package company-coq
  :ensure t
  :hook (coq-mode . company-coq-mode))

;; which-key
(use-package which-key
  :ensure t
  :config (which-key-mode))

;; eshell-prompt-extras
(use-package eshell-prompt-extras
  :ensure t
  :after esh-mode
  :config
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-multiline-with-status))

;; yasnippet
(use-package yasnippet
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  (yas-global-mode 1))

;; denote
(use-package denote
  :ensure t
  :hook
  (
  (text-mode . denote-fontify-links-mode-maybe)
  (dired-mode . denote-dired-mode))
  :bind
  ( :map global-map
    ("C-c n n" . denote-open-or-create)
    ("C-c n d" . denote-sort-dired)
    ("C-c n l" . denote-link)
    ("C-c n L" . denote-add-links)
    ("C-c n b" . denote-backlinks)
    ("C-c n q c" . denote-query-contents-link) ; create link that triggers a grep
    ("C-c n q f" . denote-query-filenames-link) ; create link that triggers a dired
    ("C-c n r" . denote-rename-file)
    ("C-c n R" . denote-rename-file-using-front-matter)
    :map dired-mode-map
    ("C-c C-d C-i" . denote-dired-link-marked-notes)
    ("C-c C-d C-r" . denote-dired-rename-files)
    ("C-c C-d C-k" . denote-dired-rename-marked-files-with-keywords)
    ("C-c C-d C-R" . denote-dired-rename-marked-files-using-front-matter))
  :config
  (setq denote-directory (expand-file-name "~/notes/"))
  (setq denote-save-buffers nil)
  (setq denote-known-keywords '("emacs" "philosophy" "research" "type system" "recipe" "writing" "paper"))
  (setq denote-infer-keywords t)
  (setq denote-sort-keywords t)
  (setq denote-prompts '(title keywords))
  (setq denote-excluded-directories-regexp nil)
  (setq denote-excluded-keywords-regexp nil)
  (setq denote-rename-confirmations '(rewrite-front-matter modify-file-name))
  (setq denote-date-prompt-use-org-read-date t)
  (denote-rename-buffer-mode 1))

;; denote-journal
(use-package denote-journal
  :ensure t
  :bind
  ( :map global-map
    ("C-c n j j" .  denote-journal-new-or-existing-entry)
    ("C-c n j l" .  denote-journal-link-or-create-entry ))
  :hook (calendar-mode . denote-journal-calendar-mode)
  :config
  (setq denote-journal-directory
        (expand-file-name "journal" denote-directory))
  (setq denote-journal-keyword "journal")
  (setq denote-journal-title-format 'day-date-month-year)
  (defun my/denote-journal-insert-template ()
    "Append template to a brand-new Denote journal entry."
    (save-excursion
      (goto-char (point-max))
      (insert
       "* Personal :personal:\n\n"
       "* Work :work:\n\n"
       "* Time-block\n\n"
       "#+BEGIN: clocktable :scope file :maxlevel 2 :block today\n"
       "#+END:\n\n")))
  (add-hook 'denote-journal-hook #'my/denote-journal-insert-template))


;; yaml-mode
(use-package yaml-mode
  :ensure t
  :mode ("\\.yml\\'" "\\.yaml\\'"))

;; nov
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(company-coq consult denote-journal eat eshell-prompt-extras magit
		 marginalia nix-mode nov orderless proof-general
		 vertico yaml-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
