(require 'yasnippet)
(when (and (> emacs-major-version 21)
           (or (require 'yasnippet-bundle nil 'noerror)
               (require 'yasnippet nil 'noerror)))
  (setq yas/wrap-around-region t)
  (unless (featurep 'yasnippet-bundle)
    (yas/initialize))
  (let* ((dir (file-name-directory (or load-file-name (buffer-file-name))))
         (snippets-dir (expand-file-name "snippets" dir)))
    (when (file-exists-p snippets-dir)
      (yas/load-directory snippets-dir)))
  (when (require 'org nil 'noerror)
    (add-hook 'org-mode-hook
              (let ((original-command (lookup-key org-mode-map [tab])))
                `(lambda ()
                   (setq yas/fallback-behavior
                         '(apply ,original-command))
                   (local-set-key [tab] 'yas/expand))))))



(provide 'init-yasnippet)
