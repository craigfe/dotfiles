;; this function is called at the very end of Spacemacs initialization after layers configuration.
(defun dotspacemacs/user-config ()

  (flyspell-mode-off)

  ;; Org agenda files
  ;; (setq org-agenda-files '("~/sys/inbox.org"
  ;;                          "~/sys/projects.org"))


  ;; (setq org-capture-templates '(("t" "Todo [inbox]" entry
  ;;                                (file+headline "~/sys/inbox.org" "Tasks")
  ;;                                "* TODO %i%?")
  ;;                               ("T" "Tickler" entry
  ;;                        (file+headline "~/sys/tickler.org" "Tickler")
  ;;                                "* %i%? \n %U")))

  ;; (setq org-refile-targets '(("~/sys/projects.org" :maxlevel . 3)
  ;;                            ("~/sys/someday.org" :maxlevel . 1)))

  ;; Subfiles
  ;; (load "~/t/dotfiles/emacs/lib/email.el")

  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  (setq inhibit-startup-screen t
        initial-buffer-choice nil)

  ;; evil-mc: multiple cursors for evil mode
  (global-evil-mc-mode 1) ;; enable globally

  ;; in visual mode, use A/I to create cursors at the beginning/end of every visual selection
  (evil-define-key 'visual evil-mc-key-map
    "A" #'evil-mc-make-cursor-in-visual-selection-end
    "I" #'evil-mc-make-cursor-in-visual-selection-beg)

  ;; kill every buffer except the currently focussed one
  ;; https://www.emacswiki.org/emacs/KillingBuffers#toc2
  (defun kill-other-buffers ()
    "Kill all other buffers."
    (interactive)
    (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

  ;; (global-set-key (kbd "<tab>") 'hippie-expand)

  ;; (add-to-list 'yas-snippet-dirs '("~/r/yasnippet-dune"))


  ;; ---------------------------------------------------------------------------
  ;; Layout
  ;; ---------------------------------------------------------------------------

  (with-eval-after-load 'magit
    (setq magit-repository-directories
          '(;; Directory containing project root directories
            ("~/r/"      . 2)
            ("~/t/"      . 2)
            ;; Specific project root directory
            ("~/sys/" . 1))))

  ;; Piggy-back `magit''s repository tracking mechanism for `projectile-known-projects'.
  ;; See https://emacs.stackexchange.com/a/32635.

  (with-eval-after-load 'projectile
    (when (require 'magit nil t)
      (mapc #'projectile-add-known-project
            (mapcar #'file-name-as-directory (magit-list-repos)))
      ;; Optionally write to persistent `projectile-known-projects-file'
      (projectile-save-known-projects)))

  ;; ---------------------------------------------------------------------------
  ;; Languages
  ;; ---------------------------------------------------------------------------

  (mapc 'load (file-expand-wildcards "~/t/dotfiles/emacs/lib/languages/*.el"))

  ;; Capnp mode
  (load "~/t/dotfiles/emacs/lib/modes/capnp-mode.el")
  (require 'capnp-mode)
  (add-to-list 'auto-mode-alist '("\\.capnp\\'" . capnp-mode))

  ;; ---------------------------------------------------------------------------
  ;; Fonts
  ;; ---------------------------------------------------------------------------

  ;; (set-frame-font "Source Code Pro Semibold-10" t t)
  (set-frame-font "JetBrainsMono-15" t t)
  ;; (set-face-attribute 'mode-line nil :font "JetBrainsMono-16")

  ;; Use variable width font faces in current buffer
  (defun my-buffer-face-mode-variable ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face "Futura LT")
    (buffer-face-mode))

  ;; Set default font faces for Info and ERC modes
  ;; (add-hook 'LaTeX-mode-hook 'my-buffer-face-mode-variable) ;; includes .cls files
  (add-to-list 'auto-mode-alist
               '("\\.tex\\'" . (lambda ()
                                 (LaTeX-mode)
                                 (my-buffer-face-mode-variable))))

  ;; ---------------------------------------------------------------------------
  ;; Org mode
  ;; ---------------------------------------------------------------------------

  (setq org-directory "~/org")
  (setq org-cycle-separator-lines 1)

  (defun my/org-mode-hook ()
    "Stop the org-level headers from increasing in height relative to the other text."
    (dolist (face '(org-level-1
                    org-level-2
                    org-level-3
                    org-level-4
                    org-level-5))
      (set-face-attribute face nil :weight 'semi-bold :height 1.0)))

  (add-hook 'org-mode-hook 'my/org-mode-hook)

  (setq-default line-spacing 3)
  (setq-default fill-column 80)

  ;; Always follow symlinks to edit Git version-controlled files directly
  (setq vc-follow-symlinks t)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  ;; Key rebindings
  (bind-key "C--" 'zoom-frm-out)
  (bind-key "C-=" 'zoom-frm-in)

  (setq avy-timout-seconds 0.3)

  (setq-default evil-escape-key-sequence "qw")
  )



