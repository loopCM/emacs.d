;; emacs-nav : simple file-system navigation

(require 'nav)
(nav-disable-overeager-window-splitting)
;; Optional: set up a quick key to toggle nav
(global-set-key [C-f1] 'nav-toggle)


(provide 'init-nav)
