;;misc
(setq inhibit-startup-screen t)
(global-visual-line-mode 1)
(setq display-line-numbers-type 'relative)
(global-display-line-numbers-mode 1)
(delete-selection-mode 1)
(setq select-enable-clipboard t)

;; Top Bar Cleanup
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; load-path
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/project" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/java" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/org-util" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/ott" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/cfg" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "lisp/biblio" user-emacs-directory))

;; /lisp imports
(require 'training-calculations)

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
;;
;; fontaine
(use-package fontaine
  :ensure t
  :config
  (setq fontaine-presets
        '((regular
           :default-family "Aporetic Sans Mono"
           :default-height 130
           :line-spacing 1)
          (large
           :default-family "Aporetic Sans Mono"
           :default-height 200
           :line-spacing 1)))
  (fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))
  (fontaine-mode 1))

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

;; embark
(use-package embark
  :ensure t
  :bind
  (("C-." . embark-act)
   ("C-;" . embark-dwim)
   ("C-h B" . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; embark-consult
(use-package embark-consult
  :ensure t
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; magit
(use-package magit
  :ensure t
  :config
  (with-eval-after-load 'magit
    (transient-append-suffix 'magit-push "p"
      '("B" "both" (lambda () (interactive) (magit-run-git-async "pushall"))))))

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

;; windmove
(use-package windmove
  :ensure nil
  :config
  (windmove-default-keybindings))

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

;; flyspell
(use-package flyspell
  :ensure nil
  :hook ((text-mode . flyspell-mode)
         (prog-mode . flyspell-prog-mode))
  :config
  (setq ispell-program-name "aspell"))

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

;; nov
(use-package nov
  :ensure t
  :mode ("\\.epub\\'" . nov-mode))

;; groovy-mode
(use-package groovy-mode
  :ensure t
  :mode (("\\.groovy\\'" . groovy-mode)
         ("\\.gradle\\'" . groovy-mode)))

;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode ("\\.md\\'" . gfm-mode))
;;
;; envrc
(use-package envrc
  :hook (after-init . envrc-global-mode))

;; activities
(use-package activities
  :ensure t
  :bind
  (([remap switch-to-buffer] . activities-switch-buffer)
   ("C-x C-a C-n" . activities-new)
   ("C-x C-a C-d" . activities-define)
   ("C-x C-a C-a" . activities-resume)
   ("C-x C-a C-s" . activities-suspend)
   ("C-x C-a C-k" . activities-kill)
   ("C-x C-a RET" . activities-switch)
   ("C-x C-a b" . activities-switch-buffer)
   ("C-x C-a g" . activities-revert)
   ("C-x C-a l" . activities-list))
  :init
  (activities-mode)
  (activities-tabs-mode)
  (setq edebug-inhibit-emacs-lisp-mode-bindings t))

;; tree-sitter
(use-package treesit
  :ensure nil
  :custom
  (treesit-font-lock-level 4)
  :init
  (setq major-mode-remap-alist
        '((java-mode   . java-ts-mode)
          (python-mode . python-ts-mode)
          (sh-mode     . bash-ts-mode))))

;; yaml-ts-mode
(use-package yaml-ts-mode
  :ensure nil
  :mode "\\.ya?ml\\'")

;; json-ts-mode
(use-package json-ts-mode
  :ensure nil
  :mode "\\.json\\'")

;; eglot
(use-package eglot
  :ensure t
  :config
  (require 'java-eglot)
  (my/java-eglot-setup))

;; org
(use-package org
  :ensure nil
  :init
  (require 'org-capture)
  (require 'time-tracking)
  (require 'pull-block)
  (require 'org-id)
  :config
  (setq org-M-RET-may-split-line '((default . nil)))
  (setq org-insert-heading-respect-content t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)
  (setq org-directory "~/notes")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@)" "BLOCK(b)" "ACTIVE(a)" "|" "CANCEL(c!)" "DONE(d!)")))
  (setq org-todo-keyword-faces
	'(("TODO"    . (:foreground "red" :weight bold))
          ("WAIT"    . (:foreground "orange" :weight bold))
          ("BLOCK"   . (:foreground "blue" :weight bold))
          ("ACTIVE"  . (:foreground "green" :weight bold))
          ("CANCEL"  . (:foreground "grey" :weight bold))
          ("DONE"    . (:foreground "forest green" :weight bold))))
  (setq org-agenda-prefix-format
	'((agenda . "%t ")
          (todo   . " ")
          (tags   . " ")
          (search . " ")))
  (setq org-agenda-custom-commands
	'(("D" "Daily agenda"
           ((todo "ACTIVE"
                  ((org-agenda-overriding-header "CURRENT TIMEBLOCK")
		   (org-agenda-files (list (denote-journal-path-to-new-or-existing-entry)))
                   (org-agenda-remove-tags t)))
	    (todo "BLOCK"
                  ((org-agenda-overriding-header "🗓️  TODAY'S TIMEBLOCKS")
		   (org-agenda-files (list (denote-journal-path-to-new-or-existing-entry)))
                   (org-agenda-prefix-format "  %i %?-12t")))
	    (todo "BLOCK"
                  ((org-agenda-overriding-header "📂 TIMEBLOCK LIST")
                   (org-agenda-files '("20251203T141526--projects__projects.org"))
		   (org-agenda-prefix-format "  %i %?-12t")
                   (org-agenda-sorting-strategy '(priority-down category-keep))))
	    (tags-todo "work"
                       ((org-agenda-overriding-header "💼 WORK INBOX")
		       (org-agenda-files '("20251203T141103--todos__todos.org"))))
	    (tags-todo "personal"
                       ((org-agenda-overriding-header "🏠 PERSONAL INBOX")
		       (org-agenda-files '("20251203T141103--todos__todos.org"))))
            (todo "WAIT"
                  ((org-agenda-overriding-header "⏳ PENDING")))
	    (todo "ACTIVE"
                  ((org-agenda-overriding-header "⚠️  ZOMBIE CLOCKS")
                   (org-agenda-skip-function '(org-agenda-skip-entry-if 'timestamp 'today))))
            (agenda ""
                    ((org-agenda-block-separator nil)
		     (org-agenda-files (list (denote-journal-path-to-new-or-existing-entry)
					     (my/get-todo-file-path)))
                     (org-agenda-span 1)
                     (org-agenda-overriding-header "\nDaily Agenda")))))))

(setq org-capture-templates
      '(

        ("w" "Work Task" entry
         (file+headline "~/notes/20251203T141103--todos__todos.org" "Work")
         "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:"
         :empty-lines 1)

        ("p" "Personal Task" entry
         (file+headline "~/notes/20251203T141103--todos__todos.org" "Personal")
         "* TODO %? \n:PROPERTIES:\n:CREATED: %U\n:END:"
         :empty-lines 1)

	("b" "Block Entry" entry
         (file "~/notes/20251203T141526--projects__projects.org")
         "* BLOCK %? :projects:\n:PROPERTIES:\n:ID: %(org-id-new)\n:CREATED: %U\n:END:\n"
         :empty-lines 1)

	("e" "Event" entry
         (file+function my/get-journal-file-for-date my/find-event-target-location)
         "* %? :event:\n:PROPERTIES:\n:LOCATION: %^{Location}\n:END:\n<%(format-time-string \"%Y-%m-%d %a %H:%M\" my/captured-event-time)>"
         :empty-lines 1)

	("t" "Training Session" entry
         (file+olp+datetree my/get-training-file-path)
         (file "~/.emacs.d/lisp/org-util/training-template.org")
         :immediate-finish nil
         :empty-lines 1))))

;; custom file
(setq custom-file (locate-user-emacs-file "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; pdf-tools
(use-package pdf-tools
  :ensure nil
  :config
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-continuous nil)
  (add-hook 'pdf-view-mode-hook (lambda () (display-line-numbers-mode -1)))
  (define-key pdf-view-mode-map (kbd "C-s") 'pdf-occur))

;; tex
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil)
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-source-correlate-start-server t)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
  (setq reftex-plug-into-AUCTeX t)
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer))

;; ott
(autoload 'ott-mode "ott-mode" "Major mode for editing Ott files." t)
(add-to-list 'auto-mode-alist '("\\.ott\\'" . ott-mode))

;; cfg
(autoload 'cfg-mode "cfg-mode" "Major mode for editing CFG files." t)
(add-to-list 'auto-mode-alist '("\\.cfg\\'" . cfg-mode))

;; adaptive wrap
(use-package adaptive-wrap
  :ensure t
  :hook
  (prog-mode . adaptive-wrap-prefix-mode)
  (text-mode . adaptive-wrap-prefix-mode))

;; v-term
(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000))

;; project-gemini
(require 'gemini-integration)
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "G") #'my/project-gemini))

;; project-vterm
(require 'project-vterm)
(with-eval-after-load 'project
  (define-key project-prefix-map (kbd "v") #'my/project-vterm))

;; citar
(use-package citar
  :ensure t
  :custom
  (citar-bibliography '("~/codex/papers/main.bib"))
  (citar-library-paths '("~/codex/papers/"))
  :hook
  (LaTeX-mode . citar-capf-setup)
  (org-mode . citar-capf-setup)
  :bind
  (("C-c b" . citar-insert-citation)))

;; citar-embark
(use-package citar-embark
  :ensure t
  :after citar embark
  :no-require t
  :config (citar-embark-mode))

;; biblio
(use-package biblio
  :ensure t
  :config
  (require 'biblio-lookup)
  (setq biblio-synchronous-backends '(dblp crossref arxiv))
  (setq biblio-bibtex-use-autokey nil)
  (setq biblio-cleanup-bibtex-function #'my/biblio-cleanup)
  :bind
  (:map dired-mode-map
        ("b" . my/dired-biblio-lookup)))
