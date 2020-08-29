;; ---------------------------------------------------------------------------
;; OCaml configuration
;; ---------------------------------------------------------------------------

;; Tuareg mode -- the OCaml major mode
;; https://github.com/ocaml/tuareg

(add-hook 'dune-mode-hook
          (lambda ()
            (set (make-local-variable 'compile-command) "dune build @check")
            ))

(defun my/tuareg-mode-hook ()
  (git-gutter+-mode)
  (evil-matchit-mode)

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
  ;; (local-set-key "<SPC> c c" 'compile)
  (set (make-local-variable 'compile-command) "dune build @check")
  (setq compilation-read-command nil) ;; don't require confirm

  ;; ocamlformat
  (require 'ocamlformat)

  ;; Rebind <tab> to run OCamlformat
  ;; (define-key merlin-mode-map (kbd "<tab>") 'ocamlformat)

  ;; Run ocamlformat before saving
  (add-hook 'before-save-hook 'ocamlformat-before-save)

  ;; Fira Code ligatures
  (load "~/t/dotfiles/emacs/lib/ligatures.el")
  )

(add-hook 'tuareg-mode-hook 'my/tuareg-mode-hook)
(add-hook 'tuareg-mode-hook #'lsp-ocaml-enable)

;; Add support for `foo_intf.ml' ↔ `foo.ml' in tuareg-find-alternate-file
(custom-set-variables
 '(tuareg-other-file-alist
   (quote
    (("\\.mli\\'" (".ml" ".mll" ".mly"))
     ("_intf.ml\\'" (".ml"))
     ("\\.ml\\'" (".mli" "_intf.ml"))
     ("\\.mll\\'" (".mli"))
     ("\\.mly\\'" (".mli"))
     ("\\.eliomi\\'" (".eliom"))
     ("\\.eliom\\'" (".eliomi"))))))

;; bind ", {m/n}" to "merlin-error-{prev/next}" in tuareg mode
(spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "n" 'merlin-error-prev)
(spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "m" 'merlin-error-next)
(spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "f" 'merlin-type-enclosing)
(spacemacs/set-leader-keys-for-major-mode 'tuareg-mode "a" 'tuareg-find-alternate-file)


