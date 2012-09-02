;; init-personal.el
;;

;; User Info
;; User Info
(setq user-full-name "loopcm")
(setq user-mail-address "loop.cm@gmail.com")

;;(global-linum-mode 1) ; always show line numbers

(set-default-font " -unknown-Ubuntu Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")



(require 'cl)
;; flod code  (copy from DelphiNew)
;;
(defun fold-this-sexp ()
  "把当前位置的括号内容折叠起来，展开的话...wait"
  (interactive)
  (save-excursion
    (let ((ov (make-overlay (point) (progn (forward-list) (point)))))
      (overlay-put ov 'display "<....>")
      (overlay-put ov 'name     "jrfold"))))
(defun unfold-this-sexp ()
  "括号内容折叠展开"
  (interactive)
  (save-excursion
  (remove-overlays (point)  (progn (forward-list) (point))))) ; 'display "<....>")
(defun sexp-code-folding-dwim ()
  "在括号处交替折叠."
  (interactive)
  (save-excursion
    (let* ((ovs-1 (save-excursion (overlays-in (point) (progn (forward-list) (point)))))
           (ovs (remove-if-not #'(lambda (x) (equal (overlay-get x 'name) "jrfold")) ovs-1)))
      (if (null ovs)
          (fold-this-sexp)
        (unfold-this-sexp)))))
(global-set-key (kbd "C-c C-f") 'sexp-code-folding-dwim)


;; Bookmarks
;;(setq bookmark-default-file "~/.emacs.d/.bookmarks")

(provide 'init-personal)

