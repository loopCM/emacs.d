;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-delay 0
 blink-cursor-interval 0.4
 bookmark-default-file "~/.emacs.d/.bookmarks.el"
 buffers-menu-max-size 30
 case-fold-search t
 compilation-scroll-output t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 grep-highlight-matches t
 grep-scroll-output t
 indent-tabs-mode nil
 line-spacing 0.2
 make-backup-files nil
 mouse-yank-at-point t
 set-mark-command-repeat-pop t
 show-trailing-whitespace t
 tooltip-delay 1.5
 truncate-lines nil
 truncate-partial-width-windows nil
 visible-bell t)

(transient-mark-mode t)


;;----------------------------------------------------------------------------
;; Zap *up* to char is a more sensible default
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "M-Z") 'zap-to-char)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(paren-activate)     ; activating mic-paren

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(global-set-key (kbd "C-=") 'er/expand-region)

;;----------------------------------------------------------------------------
;; Autopair quotes and parentheses
;;----------------------------------------------------------------------------
(require 'autopair)
(setq autopair-autowrap t)
(autopair-global-mode t)

(defun inhibit-autopair ()
  "Prevent autopair from enabling in the current buffer."
  (setq autopair-dont-activate t)
  (autopair-mode -1))

;;----------------------------------------------------------------------------
;; Fix per-window memory of buffer point positions
;;----------------------------------------------------------------------------
(global-pointback-mode)


;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice


;;----------------------------------------------------------------------------
;; Handy key bindings
;;----------------------------------------------------------------------------
;; To be able to M-x without meta
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; Vimmy alternatives to M-^ and C-u M-^
(global-set-key (kbd "C-c j") 'join-line)
(global-set-key (kbd "C-c J") (lambda () (interactive) (join-line 1)))

(global-set-key (kbd "M-T") 'transpose-lines)
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "C-;") 'ace-jump-mode)
(global-set-key (kbd "C-:") 'ace-jump-word-mode)


;; Mark-multiple and friends
(global-set-key (kbd "C-x r t") 'inline-string-rectangle)
(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this)


(defun duplicate-line ()
  (interactive)
  (save-excursion
    (let ((line-text (buffer-substring-no-properties
                      (line-beginning-position)
                      (line-end-position))))
      (move-end-of-line 1)
      (newline)
      (insert line-text))))

(global-set-key (kbd "C-c p") 'duplicate-line)

;; Train myself to use M-f and M-b instead
(global-unset-key [M-left])
(global-unset-key [M-right])



;;----------------------------------------------------------------------------
;; Fill column indicator
;;----------------------------------------------------------------------------
(when (> emacs-major-version 23)
  (defun sanityinc/prog-mode-fci-settings ()
    (turn-on-fci-mode)
    (when show-trailing-whitespace
      (set (make-local-variable 'whitespace-style) '(face trailing))
      (whitespace-mode 1)))

  (add-hook 'prog-mode-hook 'sanityinc/prog-mode-fci-settings)

  (defvar sanityinc/fci-mode-suppressed nil)
  (defadvice popup-create (before suppress-fci-mode activate)
    "Suspend fci-mode while popups are visible"
    (set (make-local-variable 'sanityinc/fci-mode-suppressed) fci-mode)
    (when fci-mode
      (turn-off-fci-mode)))
  (defadvice popup-delete (after restore-fci-mode activate)
    "Restore fci-mode when all popups have closed"
    (when (and (not popup-instances) sanityinc/fci-mode-suppressed)
      (setq sanityinc/fci-mode-suppressed nil)
      (turn-on-fci-mode))))


;;----------------------------------------------------------------------------
;; Shift lines up and down with M-up and M-down
;;----------------------------------------------------------------------------
(move-text-default-bindings)


;;----------------------------------------------------------------------------
;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
;;----------------------------------------------------------------------------
(defun backward-up-sexp (arg)
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up


;;----------------------------------------------------------------------------
;; Cut/copy the current line if no region is active
;;----------------------------------------------------------------------------
(whole-line-or-region-mode t)
(diminish 'whole-line-or-region-mode)
(make-variable-buffer-local 'whole-line-or-region-mode)

(defun suspend-mode-during-cua-rect-selection (mode-name)
  "Add an advice to suspend `MODE-NAME' while selecting a CUA rectangle."
  (let ((flagvar (intern (format "%s-was-active-before-cua-rectangle" mode-name)))
        (advice-name (intern (format "suspend-%s" mode-name))))
    (eval-after-load 'cua-rect
      `(progn
         (defvar ,flagvar nil)
         (make-variable-buffer-local ',flagvar)
         (defadvice cua--activate-rectangle (after ,advice-name activate)
           (setq ,flagvar (and (boundp ',mode-name) ,mode-name))
           (when ,flagvar
             (,mode-name 0)))
         (defadvice cua--deactivate-rectangle (after ,advice-name activate)
           (when ,flagvar
             (,mode-name 1)))))))

(suspend-mode-during-cua-rect-selection 'whole-line-or-region-mode)


;;----------------------------------------------------------------------------
;; Random line sorting
;;----------------------------------------------------------------------------
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))



;; Highlight

(when (fboundp 'global-font-lock-mode)                                        ;;
  (global-font-lock-mode t))                                                  ;;
(when (fboundp 'transient-mark-mode)                                          ;;
  (transient-mark-mode t))                                                    ;;
(setq hl-line-face 'underline)          ; for highlight-symbol                ;;
;;;;;;;(global-hl-line-mode 1)                 ; (if window-system 1 -1)      ;;
;; (global-highlight-changes-mode t)       ; use cedet instead                ;;
(dolist (mode '(c-mode c++-mode objc-mode java-mode jde-mode                  ;;
                       perl-mode cperl-mode python-mode                       ;;
                       ruby-mode lisp-mode emacs-lisp-mode                    ;;
                       lisp-interaction-mode sh-mode                          ;;
                       org-mode sgml-mode))                                   ;;
  (font-lock-add-keywords                                                     ;;
   mode                                                                       ;;
   '(("\\<\\(FIXME\\|TODO\\|HACKS\\)\\>" 1 font-lock-warning-face prepend)    ;;
     ("\\<\\(FIXME\\|TODO\\|HACKS\\):" 1 font-lock-warning-face prepend))))   ;;
;; (setq-default show-trailing-whitespace t) ; use whitespace-mode instead    ;;
(setq whitespace-style '(face trailing lines-tail newline empty tab-mark))    ;;
                                                                              ;;
                                                                              ;;
                                                                              ;;
;;-----------------------------------------------------------------------     ;;
;; editor                                                                     ;;
;;----------------                                                            ;;
;; clang autocomplete
(defadvice find-tag (before tags-file-name-advice activate)                   ;;
  "Find TAGS file in ./ or ../ or ../../ dirs"                                ;;
  (let ((list (mapcar 'expand-file-name '("./TAGS" "../TAGS" "../../TAGS")))) ;;
    (while list                                                               ;;
      (if (file-exists-p (car list))                                          ;;
          (progn                                                              ;;
            (setq tags-file-name (car list))                                  ;;
            (setq list nil))                                                  ;;
        (setq list (cdr list))))))                                            ;;
                                                                              ;;
(defun find-dotemacs-file ()                                                  ;;
  "Open .emacs file"                                                          ;;
  (interactive)                                                               ;;
  (let* ((homedir (getenv "HOME"))                                            ;;
         (path1 (expand-file-name ".emacs" homedir))                          ;;
         (path2 (expand-file-name "_emacs" homedir))                          ;;
         (path3 (expand-file-name "site-start.el" homedir))                   ;;
         (dotemacs-path path1))                                               ;;
    (when (file-exists-p path3)                                               ;;
      (setq dotemacs-path path3))                                             ;;
    (when (file-exists-p path2)                                               ;;
      (setq dotemacs-path path2))                                             ;;
    (when (file-exists-p path1)                                               ;;
      (setq dotemacs-path path1))                                             ;;
    (find-file dotemacs-path)))                                               ;;
                                                                              ;;
(defun move-line-up (p)                                                       ;;
  "Move current line up, copy from crazycool@smth"                            ;;
  (interactive "*p")                                                          ;;
  (let ((c (current-column)))                                                 ;;
    (beginning-of-line)                                                       ;;
    (kill-line 1)                                                             ;;
    (previous-line p)                                                         ;;
    (beginning-of-line)                                                       ;;
    (yank)                                                                    ;;
    (previous-line 1)                                                         ;;
    (move-to-column c)))                                                      ;;
                                                                              ;;
(defun move-line-down (p)                                                     ;;
  "Move current line down, copy from crazycool@smth"                          ;;
  (interactive "*p")                                                          ;;
  (let ((c (current-column)))                                                 ;;
    (beginning-of-line)                                                       ;;
    (kill-line 1)                                                             ;;
    (next-line p)                                                             ;;
    (beginning-of-line)                                                       ;;
    (yank)                                                                    ;;
    (previous-line 1)                                                         ;;
    (move-to-column c)))                                                      ;;
                                                                              ;;
(defun format-region ()                                                       ;;
  "Format region, if no region actived, format current buffer.                ;;
Like eclipse's Ctrl+Alt+F."                                                   ;;
  (interactive)                                                               ;;
  (let ((start (point-min))                                                   ;;
        (end (point-max)))                                                    ;;
    (if (and (fboundp 'region-active-p) (region-active-p))                    ;;
        (progn (setq start (region-beginning))                                ;;
               (setq end (region-end)))                                       ;;
      (progn (when (fboundp 'whitespace-cleanup)                              ;;
               (whitespace-cleanup))                                          ;;
             (setq end (point-max))))                                         ;;
    (save-excursion                                                           ;;
      (save-restriction                                                       ;;
        (narrow-to-region (point-min) end)                                    ;;
        (push-mark (point))                                                   ;;
        (push-mark (point-max) nil t)                                         ;;
        (goto-char start)                                                     ;;
        (when (fboundp 'whitespace-cleanup)                                   ;;
          (whitespace-cleanup))                                               ;;
        (untabify start (point-max))                                          ;;
        (indent-region start (point-max) nil)))))                             ;;
                                                                              ;;
(defun cxx-file-p (file)                                                      ;;
  (let ((file-extension (file-name-extension file)))                          ;;
    (and file-extension                                                       ;;
         (string= file (file-name-sans-versions file))                        ;;
         (find file-extension                                                 ;;
               '("h" "hpp" "hxx" "c" "cpp" "cc" "cxx" "CC" "C")               ;;
               :test 'string=))))                                             ;;
                                                                              ;;
(defun format-cxx-file (file)                                                 ;;
  "Format a c/c++ file."                                                      ;;
  (interactive "F")                                                           ;;
  (if (cxx-file-p file)                                                       ;;
      (let ((buffer (find-file-noselect file))) ;; open buffer                ;;
        (set-buffer buffer)                                                   ;;
        ;; (mark-whole-buffer)                                                ;;
        (when (fboundp 'whitespace-cleanup)                                   ;;
          (whitespace-cleanup))                                               ;;
        (untabify (point-min) (point-max))                                    ;;
        (indent-region (point-min) (point-max))                               ;;
        (save-buffer)                                                         ;;
        (kill-buffer)                                                         ;;
        (message "Formated c++ file:%s" file))                                ;;
    (message "%s isn't a c++ file" file)))                                    ;;
                                                                              ;;
(defun format-cxx-directory (dirname)                                         ;;
  "Format all c/c++ file in a directory."                                     ;;
  (interactive "D")                                                           ;;
  ;; (message "directory:%s" dirname)                                         ;;
  (let ((files (directory-files dirname t)))                                  ;;
    (dolist (x files)                                                         ;;
      (if (not (string= "." (substring (file-name-nondirectory x) 0 1)))      ;;
          (if (file-directory-p x)                                            ;;
              (format-cxx-directory x)                                        ;;
            (if (and (file-regular-p x)                                       ;;
                     (not (file-symlink-p x))                                 ;;
                     (cxx-file-p x))                                          ;;
                (format-cxx-file x)))))))                                     ;;
                                                                              ;;
(defun moccur-word-all-buffers (regexp)                                       ;;
  "Run `multi-occur' to find regexp in all buffers."                          ;;
  (if (= 0 (length regexp))                                                   ;;
      (message "Regexp is blank.")                                            ;;
    (let ((buffers (buffer-list)))                                            ;;
      (dolist (buffer buffers)                                                ;;
        (let ((pos (string-match " *\\*" (buffer-name buffer))))              ;;
          (when (and pos (= 0 pos))                                           ;;
            (setq buffers (remq buffer buffers)))))                           ;;
      (multi-occur buffers regexp))))                                         ;;
                                                                              ;;
(defun moccur-all-buffers (&optional prompt)                                  ;;
  "Run `multi-occur' to find current word in all buffers."                    ;;
  (interactive "P")                                                           ;;
  (let ((word (grep-tag-default)))                                            ;;
    (when (or prompt (= (length word) 0))                                     ;;
      (setq word (read-regexp "List lines matching regexp" word)))            ;;
    (moccur-word-all-buffers word)))                                          ;;
                                                                              ;;
(defun moccur-todo-all-buffers ()                                             ;;
  "Run `multi-occur' to find 'TODO' in all buffers."                          ;;
  (interactive)                                                               ;;
  (moccur-word-all-buffers                                                    ;;
   "\\<\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)\\>"))                    ;;
                                                                              ;;
(autoload 'grep-tag-default "grep")                                           ;;
(autoload 'grep-apply-setting "grep")                                         ;;
(defun grep-current-dir (&optional prompt wd)                                 ;;
  "Run `grep' to find current word in current directory."                     ;;
  (interactive "P")                                                           ;;
  (let* ((word (or wd (grep-tag-default)))                                    ;;
         (cmd (concat "grep -inrHIE \"" word "\" ."                           ;;
                      " | grep -vE \"\.svn/|\.git/|\.hg/|\.bzr/|CVS/\"")))    ;;
    (grep-apply-setting 'grep-use-null-device nil)                            ;;
    (if (or prompt (= (length word) 0))                                       ;;
        (grep (read-shell-command                                             ;;
               "Run grep (like this): " cmd 'grep-history))                   ;;
      (if (= 0 (length word))                                                 ;;
          (message "Word is blank.")                                          ;;
        (grep cmd)))))                                                        ;;
                                                                              ;;
(defun grep-todo-current-dir ()                                               ;;
  "Run `grep' to find 'TODO' in current directory."                           ;;
  (interactive)                                                               ;;
  (grep-current-dir nil "TODO|FIXME|HACKS"))                                  ;;
                                                                              ;;
(defun switch-to-other-buffer ()                                              ;;
  "Switch to (other-buffer)."                                                 ;;
  (interactive)                                                               ;;
  (switch-to-buffer (other-buffer)))                                          ;;
(defadvice switch-to-other-buffer (after pulse-advice activate)               ;;
  "After switch-to-other-buffer, pulse the line the cursor lands on."         ;;
  (when (and (boundp 'pulse-command-advice-flag) pulse-command-advice-flag    ;;
             (interactive-p))                                                 ;;
    (pulse-momentary-highlight-one-line (point))))                            ;;
                                                                              ;;
(defun mark-current-line ()                                                   ;;
  "Put point at beginning of this line, mark at end."                         ;;
  (interactive)                                                               ;;
  (move-beginning-of-line 1)                                                  ;;
  (set-mark (point))                                                          ;;
  (move-end-of-line 1))                                                       ;;
                                                                              ;;
(defun mark-current-line-mouse (ev)                                           ;;
  "Mark current line with a mouse click. EV is the mouse event."              ;;
  (interactive "e")                                                           ;;
  (mouse-set-point ev)                                                        ;;
  (mark-current-line))                                                        ;;


;; global key bindings

(global-set-key (kbd "<M-up>") 'move-line-up)                              ;;
(global-set-key (kbd "<M-down>") 'move-line-down)                          ;;
(global-set-key (kbd "<find>") 'move-beginning-of-line) ; putty            ;;
(global-set-key (kbd "<select>") 'move-end-of-line) ; putty                ;;
(unless (key-binding [mouse-4])                                            ;;
  (global-set-key [mouse-4] 'mwheel-scroll)) ; putty                       ;;
(unless (key-binding [mouse-5])                                            ;;
  (global-set-key [mouse-5] 'mwheel-scroll)) ; putty                       ;;
(global-set-key (kbd "C-=") 'align)                                        ;;
(global-set-key (kbd "C-S-u") 'upcase-region)                              ;;
(global-set-key (kbd "C-S-l") 'downcase-region)                            ;;
;; (global-set-key (kbd "C-M-;") 'comment-or-uncomment-region)             ;;
;; (global-set-key (kbd "ESC M-;") 'comment-or-uncomment-region) ; putty   ;;
(global-set-key [M-f8] 'format-region)                                     ;;
(global-set-key (kbd "ESC <f8>") 'format-region) ; putty                   ;;
(global-set-key (kbd "C-S-f") 'format-region)                              ;;
(global-set-key (kbd "M-P") 'previous-buffer)                              ;;
(global-set-key (kbd "M-N") 'next-buffer)                                  ;;
;;(global-set-key [(control tab)] 'switch-to-other-buffer)                   ;;
(global-set-key (kbd "C-c q") 'auto-fill-mode)                             ;;
                                                                           ;;
(define-key global-map "\C-x\C-j"                                          ;;
  (lambda ()                                                               ;;
    (interactive)                                                          ;;
    (when (require 'dired-x nil 'noerror)                                  ;;
      (dired-jump))))                                                      ;;
(global-set-key [f4] 'next-error)                                          ;;
(global-set-key [S-f4] 'previous-error)                                    ;;
(global-set-key [M-f4] 'kill-this-buffer)                                  ;;
(global-set-key (kbd "ESC <f4>") 'kill-this-buffer) ; putty                ;;
(global-set-key [f6] 'grep-current-dir)                                    ;;
(global-set-key [C-f6] 'moccur-all-buffers)                                ;;
(global-set-key [M-f6] 'grep-todo-current-dir)                             ;;
;; (lambda () (interactive) (grep-current-dir nil "TODO|FIXME")))          ;;
(global-set-key (kbd "ESC <f6>") (key-binding [M-f6]))                     ;;
(global-set-key [C-M-f6] 'moccur-todo-all-buffers)                         ;;
;; '(lambda ()                                                             ;;
;;    (interactive)                                                        ;;
;;    (moccur-word-all-buffers                                             ;;
;;     "\\<\\([Tt][Oo][Dd][Oo]\\|[Ff][Ii][Xx][Mm][Ee]\\)\\>")))            ;;
(global-set-key (kbd "ESC <C-f6>") (key-binding [C-M-f6]))                 ;;
(global-set-key [f7] '(lambda () (interactive) (compile compile-command))) ;;
;; (global-set-key [header-line double-mouse-1] 'kill-this-buffer)         ;;
(global-set-key [header-line mouse-3] 'kill-this-buffer)                   ;;
(global-set-key [mouse-2] nil)                                             ;;
(global-set-key [left-fringe mouse-2] nil)                                 ;;
(global-set-key [left-margin mouse-2] nil)                                 ;;
(global-set-key [mouse-3] menu-bar-edit-menu)                              ;;
(global-set-key (kbd "<left-margin> <mouse-2>") 'mark-current-line-mouse)  ;;
(global-set-key (kbd "C-S-t") 'undo-kill-buffer)                           ;;





(provide 'init-editing-utils)
