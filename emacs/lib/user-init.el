(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."

  (require 'opam-user-setup "~/r/dotfiles/emacs/lib/opam-user-setup.el")

  (setq exec-path-from-shell-check-startup-files nil)
  (setq evil-want-abbrev-expand-on-insert-exit nil)
  )
