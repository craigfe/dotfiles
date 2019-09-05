;; this function is called at the very end of Spacemacs initialization after layers configuration.
(defun dotspacemacs/user-config ()
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

  ;; (define-key yas-minor-mode-map (kbd "TAB") yas-expand)

  ;; Tuareg mode -- the OCaml major mode
  ;; https://github.com/ocaml/tuareg

  (add-hook 'tuareg-mode-hook
            (lambda ()
              (bind-keys*
               ;; Navigate between Merlin errors with M-{j,k}
               ("M-j" . merlin-error-next)
               ("M-k" . merlin-error-prev)

               ;; Navigate between compilation errors with M-{u,i}
               ("M-u" . next-error)
               ("M-i" . previous-error))

              ;; 'merlin-error-{next,prev} check errors and move at the same time
              (setq merlin-error-check-then-move nil)

              ;; Navigate between compilation errors with M-S-{j,k}

              ;; Projectile rebinds this to 'helm-make-projectile, which is not
              ;; what I want for OCaml.
              (local-set-key "SPC c c" 'compile)
              (set (make-local-variable 'compile-command) "dune build @check")
              (setq compilation-read-command nil) ;; don't require confirm

              ;; Rebind <tab> to run OCamlformat
              (define-key merlin-mode-map (kbd "<tab>") 'ocamlformat)

              ;; Run ocamlformat before saving
              (add-hook 'before-save-hook 'ocamlformat-before-save)

              ;; Fira Code ligatures
              ;; (load "~/r/dotfiles/emacs/ligatures.el")
              ))

  ;; bind ", {m/n}" to "merlin-error-{prev/next}" in tuareg mode
  (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "n" 'merlin-error-prev)
  (spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "m" 'merlin-error-next)

  ;; font configuration
  (set-frame-font "Source Code Pro Semibold-10" t t)

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


  (setq org-directory "~/org")
  (setq org-cycle-separator-lines 1)

  (setq-default TeX-engine 'xetex)
  ;; (add-to-list 'TeX-view-program-selection
  ;;              '(output-pdf "Zathura"))

  (setq-default line-spacing 5)
  (setq-default fill-column 100)

  ;; Always follow symlinks to edit Git version-controlled files directly
  (setq vc-follow-symlinks t)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  ;; Key rebindings
  (bind-key "C--" 'zoom-frm-out)
  (bind-key "C-=" 'zoom-frm-in)

  (setq avy-timout-seconds 0.3)

  (setq-default evil-escape-key-sequence "kj")

  ;; Disable fontification of section headings
  ;; (setq font-latex-fontify-script nil)
  ;; (setq font-latex-fontify-sectioning 'color)

  ;; Use monospace fonts / no indenting on lstlisting environments
  ;; (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  ;; (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))
)
