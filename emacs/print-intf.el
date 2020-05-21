
(defun ocaml-print-buffer-intf ()
  (interactive)
  (let* ((file-name (buffer-file-name)))
    (progn
      (with-temp-buffer
        (progn
          (setq return-code
                (call-process "ocaml-print-intf" nil (current-buffer) nil file-name))
          (setq output (buffer-string))))
          (if (zerop return-code)
              (message "> %s" output)
            (message output)
        ))))

(provide 'ocaml-print-buffer-intf)
