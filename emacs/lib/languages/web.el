(defun my/web-mode-hook ()
  (prettier-js-mode))

(add-hook 'web-mode-hook 'my/web-mode-hook)

(defun my/css-mode-hook ()
  (prettier-js-mode))

(add-hook 'css-mode-hook 'my/css-mode-hook)
