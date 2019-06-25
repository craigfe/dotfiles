;; This function is called at the very end of Spacemacs initialization after layers configuration.
(defun dotspacemacs/user-config ()

  (setq inhibit-startup-screen t
        initial-buffer-choice nil)

  ;; Use variable width font faces in current buffer
  (defun my-buffer-face-mode-variable ()
    "Set font to a variable width (proportional) fonts in current buffer"
    (interactive)
    (setq buffer-face-mode-face '(:family "Inter" :height 150))
    (buffer-face-mode))

  ;; Set default font faces for Info and ERC modes
  ;; (add-hook 'LaTeX-mode-hook 'my-buffer-face-mode-variable) ;; includes .cls files
  (add-to-list 'auto-mode-alist
               '("\\.tex\\'" . (lambda ()
                                 (LaTeX-mode)
                                 (my-buffer-face-mode-variable))))

  (setq-default TeX-engine 'xetex)
  ;; (add-to-list 'TeX-view-program-selection
  ;;              '(output-pdf "Zathura"))

  (setq-default line-spacing 5)
  (setq-default fill-column 100)

  ;; Always follow symlinks to edit Git version-controlled files directly
  (setq vc-follow-symlinks t)
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1)

  (load "~/repos/config/emacs/ligatures.el")
  (add-hook 'tuareg-mode-hook #'fira-code-mode)


  ;; Disable fontification of section headings
  ;; (setq font-latex-fontify-script nil)
  ;; (setq font-latex-fontify-sectioning 'color)

  ;; Use monospace fonts / no indenting on lstlisting environments
  ;; (add-to-list 'LaTeX-verbatim-environments "lstlisting")
  ;; (add-to-list 'LaTeX-indent-environment-list '("lstlisting" current-indentation))

  (setq-default evil-escape-key-sequence "kj")
  )
