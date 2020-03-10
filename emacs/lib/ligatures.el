  ;; ---------------------------------------------------------------------------
  ;; Ligature stuff
  ;; ---------------------------------------------------------------------------

(defun hasklig-mode--make-alist (list)
  "Generate prettify-symbols alist from LIST."
  (let ((idx -1))
    (mapcar
     (lambda (s)
       (setq idx (1+ idx))
       (let* ((code (+ #Xe100 idx))
              (width (string-width s))
              (prefix ())
              (suffix '(?\s (Br . Br)))
              (n 1))
         (while (< n width)
           (setq prefix (append prefix '(?\s (Br . Bl))))
           (setq n (1+ n)))
         (cons s (append prefix suffix (list (decode-char 'ucs code))))))
     list)))

;; Hasklig ligatures. See https://github.com/i-tu/Hasklig/blob/master/GlyphOrderAndAliasDB.
(defconst hasklig-mode--ligatures
  '("&&" "***" "*>" "\\\\" "||" "|>" "::"
    "==" "===" "==>" "=>" "=<<" "!!" ">>"
    ">>=" ">>>" ">>-" ">-" "->" "-<" "-<<"
    "<*" "<*>" "<|" "<|>" "<$>" "<>" "<-"
    "<<" "<<<" "<+>" ".." "..." "++" "+++"
    "/=" ":::" ">=>" "->>" "<=>" "<=<" "<->"))

(defvar hasklig-mode--old-prettify-alist)

(defun hasklig-mode--enable ()
  "Enable Hasklig ligatures in current buffer."
  (setq-local hasklig-mode--old-prettify-alist prettify-symbols-alist)
  (setq-local prettify-symbols-alist (append (hasklig-mode--make-alist hasklig-mode--ligatures) hasklig-mode--old-prettify-alist))
  (prettify-symbols-mode t))

(defun hasklig-mode--disable ()
  "Disable Hasklig ligatures in current buffer."
  (setq-local prettify-symbols-alist hasklig-mode--old-prettify-alist)
  (prettify-symbols-mode -1))

;;;###autoload
(define-minor-mode hasklig-mode
  "Hasklig Ligatures minor mode."
  :lighter " Hasklig"
  (setq-local prettify-symbols-unprettify-at-point 'right-edge)
  (if hasklig-mode
      (hasklig-mode--enable)
      (hasklig-mode--disable)))

(provide 'hasklig-mode)
;;; hasklig-mode.el ends here

(defun jetbrains-ligature-mode--make-alist (list)
   "Generate prettify-symbols alist from LIST."
   (let ((idx -1))
     (mapcar
      (lambda (s)
        (setq idx (1+ idx))
        (if s
            (let* ((code (+ #X10001 idx))
                   (width (string-width s))
                   (prefix ())
                   (suffix '(?\s (Br . Br)))
                   (n 1))
              (while (< n width)
                (setq prefix (append prefix '(?\s (Br . Bl))))
                (setq n (1+ n)))
              (cons s (append prefix suffix (list (decode-char 'ucs code)))))))
      list)))

 (defconst jetbrains-ligature-mode--ligatures
   '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
     "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
     nil nil nil nil nil nil nil nil nil nil nil nil nil nil
     "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
     "|||>" "<|||" ">>" "<<" nil nil "::=" "|]" "[|" "{|" "|}"
     "[<" ">]" ":?>" ":?" nil "/=" "[||]" "!!" "?:" "?." "::"
     "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
     "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
     "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
     ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
     "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
     "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
     "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
     "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
     "&="))

(defvar jetbrains-ligature-mode--old-prettify-alist)

(defun jetbrains-ligature-mode--enable ()
    "Enable JetBrains Mono ligatures in current buffer."
    (setq-local jetbrains-ligature-mode--old-prettify-alist prettify-symbols-alist)
       (setq-local prettify-symbols-alist (append (jetbrains-ligature-mode--make-alist jetbrains-ligature-mode--ligatures) jetbrains-ligature-mode--old-prettify-alist))
       (prettify-symbols-mode t))

(defun jetbrains-ligature-mode--disable ()
    "Disable JetBrains Mono ligatures in current buffer."
    (setq-local prettify-symbols-alist jetbrains-ligature-mode--old-prettify-alist)
    (prettify-symbols-mode -1))

(define-minor-mode jetbrains-ligature-mode
    "JetBrains Mono ligatures minor mode"
    :lighter " JetBrains Mono"
    (setq-local prettify-symbols-unprettify-at-point 'right-edge)
    (if jetbrains-ligature-mode
        (jetbrains-ligature-mode--enable)
      (jetbrains-ligature-mode--disable)))

(defun jetbrains-ligature-mode--setup ()
    "Setup JetBrains Mono Symbols"
    (set-fontset-font t '(#X10001 . #X1009c) "JetBrains Mono"))

(provide 'jetbrains-ligature-mode)
