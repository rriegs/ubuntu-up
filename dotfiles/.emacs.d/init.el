(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))
 '(column-number-mode t)
 '(default-frame-alist '((fullscreen . maximized)))
 '(desktop-files-not-to-save "(ftp)\\'")
 '(desktop-save-mode t)
 '(flyspell-mark-duplications-flag nil)
 '(global-auto-revert-mode t)
 '(global-subword-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(ispell-dictionary "en_US-w_accents")
 '(ispell-extra-args '("--camel-case"))
 '(ispell-local-dictionary-alist
   '(("en_US-w_accents" "[[:alpha:]]" "[^[:alpha:]]" "['’]" t nil nil utf-8)))
 '(make-backup-files nil)
 '(menu-bar-mode nil)
 '(mode-require-final-newline 'ask)
 '(mouse-wheel-progressive-speed nil)
 '(package-selected-packages '(google-c-style markdown-mode web-mode yaml-mode))
 '(ring-bell-function 'ignore)
 '(scroll-bar-mode nil)
 '(search-default-mode 'char-fold-to-regexp)
 '(show-paren-mode t)
 '(size-indication-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-method "sshx")
 '(uniquify-buffer-name-style 'forward nil (uniquify))
 '(uniquify-min-dir-content 2)
 '(whitespace-style
   '(face tabs spaces trailing space-before-tab newline indentation empty space-after-tab space-mark tab-mark newline-mark)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "light gray" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "DAMA" :family "Ubuntu Mono"))))
 '(fixed-pitch ((t (:family "Ubuntu Mono"))))
 '(fixed-pitch-serif ((t (:height 200 :family "Monospace Serif"))))
 '(italic ((t (:slant italic))))
 '(variable-pitch ((t (:family "Ubuntu"))))
 '(whitespace-hspace ((t (:foreground "dim gray"))))
 '(whitespace-newline ((t (:foreground "dim gray" :weight normal))))
 '(whitespace-space ((t (:foreground "dim gray"))))
 '(whitespace-tab ((t (:foreground "dim gray")))))

;; Register a local site-lisp directory
(add-to-list 'load-path "~/.emacs.d/site-lisp")

;; Enable MELPA and install various packages
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(package-refresh-contents)
;(package-install-selected-packages 'no-confirm)

;; Enable a handy ruler at the top of each window
(add-hook 'find-file-hook (lambda () (ruler-mode 1)))



(require 'tramp)

;; Don't backup remote files
(defun my-tramp-find-file-hook ()
  (if (file-remote-p buffer-file-name) (setq make-backup-files nil)))
(add-hook 'find-file-hook #'my-tramp-find-file-hook)

;; Fix Tramp offering .. and . as "completions"
(advice-add #'file-name-all-completions :filter-return #'completion-pcm--filename-try-filter)



;; Auto-save the desktop
(require 'desktop)
(defun my-desktop-save ()
  ;; Don't call desktop-save-in-desktop-dir, as it prints a message
  (if (eq (desktop-owner) (emacs-pid)) (desktop-save desktop-dirname)))
(add-hook 'auto-save-hook #'my-desktop-save)



(require 'flyspell-subword)

;; Use aspell if installed
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (setq ispell-list-command "--list"))

;; Enable flyspell (with subword support) by default
(defun my-flyspell-find-file-hook ()
  (flyspell-mode t))
  ;(flyspell-buffer))
(add-hook 'find-file-hook #'my-flyspell-find-file-hook)



;; Navigate windows with PGUP/PGDN
;(define-key global-map (kbd "<C-M-next>") 'next-buffer)
;(define-key global-map (kbd "<C-M-prior>") 'previous-buffer)
;(define-key global-map (kbd "<C-next>") (kbd "C-1 C-x o"))
;(define-key global-map (kbd "<C-prior>") (kbd "C-- C-x o"))

;; Navigate windows with up/down
(define-key global-map (kbd "<M-S-down>") #'next-buffer)
(define-key global-map (kbd "<M-S-up>") #'previous-buffer)
(define-key global-map (kbd "<M-down>") (kbd "C-1 C-x o"))
(define-key global-map (kbd "<M-up>") (kbd "C-- C-x o"))

(defun toggle-fullscreen ()
  "Toggle fullscreen mode on X11."
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))
(global-set-key [f11] #'toggle-fullscreen)

(defun insert-date ()
  "Insert today's date."
  (interactive)
  (insert (format-time-string "%F")))
(define-key global-map (kbd "C-c C-m") #'insert-date)

(define-key global-map (kbd "C-=") #'count-matches)


(require 'cc-mode)

;; Define a function to add inclusion guards
(defun insert-inclusion-guards (&optional depth)
  "Automatically generate C-style inclusion guards."
  (interactive "P")
  (save-excursion
    (let ((file-path (directory-file-name (file-name-directory (buffer-file-name))))
          (file-symbol (file-name-nondirectory (buffer-file-name))))
      (when (and depth (> depth 0))
        (dotimes (number depth)
          (setq file-symbol (concat (file-name-as-directory (file-name-nondirectory file-path)) file-symbol))
          (setq file-path (directory-file-name (file-name-directory file-path)))))
      (setq file-symbol (replace-regexp-in-string "[^[:alnum:]]" "_" (upcase file-symbol)))
      (goto-char (point-min))
      (insert "#ifndef " file-symbol "\n#define " file-symbol "\n\n")
      (goto-char (point-max))
      (insert "\n#endif // " file-symbol "\n"))))

;; Some useful key bindings for programming
(define-key c-mode-base-map (kbd "C-c C-f") #'compile)
(define-key c-mode-base-map (kbd "C-c C-u") #'uncomment-region)
(define-key c-mode-base-map (kbd "C-c C-i") #'insert-inclusion-guards)

;; Open .h files in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tcc\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode))

;; A few tweaks
(defun my-c-mode-common-hook ()
  ;; Indent class content two units, access labels one unit
  ;(c-set-offset 'inclass '++)
  ;(c-set-offset 'access-label '-)

  ;; Indent close parens to match the lines that open them
  (c-set-offset 'arglist-close 0)

  ;; Set the comment/fill columns to something more reasonable
  (setq comment-column 30)
  (setq fill-column 80))
(add-hook 'c-mode-common-hook #'my-c-mode-common-hook)

;; Use the Google C++ Style Guide
(require 'google-c-style)
(add-hook 'c-mode-common-hook #'google-make-newline-indent)
(add-hook 'c-mode-common-hook #'google-set-c-style)

;; Also only indent two spaces in sh and python
(add-hook 'sh-mode-hook (lambda () (setq sh-basic-offset 2)))
(add-hook 'python-mode-hook (lambda () (setq python-indent 2)))

;; Also only indent two spaces in web-mode
(require 'web-mode)
(defun my-web-mode-hook ()
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-style-padding 2)
  (setq web-mode-script-padding 2))
(add-hook 'web-mode-hook #'my-web-mode-hook)

;; Open various web files in web-mode
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.s?css\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))


;; Highlight strings in Markdown mode
(require 'markdown-mode)
(let ((patterns '(("\\(\\$\\)\\([^[:space:]]\\(?:[^\n]*?[^[:space:]]\\)??\\)\\(\\$\\)"
                   (1 'markdown-markup-face t)
                   (2 'markdown-inline-code-face t)
                   (3 'markdown-markup-face t)
                   ("[[:alpha:]]+"
                    (progn (goto-char (match-beginning 2)) (match-end 2)) nil
                    (0 '(:inherit markdown-inline-code-face :slant italic) t))
                   ("\\(\\\\\\)\\([[:alpha:]]+\\|.\\)"
                    (progn (goto-char (match-beginning 2)) (match-end 2)) nil
                    (1 'markdown-markup-face t)
                    (2 'markdown-inline-code-face t)))
                  ("\"\\(?:\n?[^\"\n]+\\)*\\(?:\"\\|$\\)"
                   0 'font-lock-string-face keep)
                  ("“\\(?:\n?[^”\n]+\\)*\\(?:”\\|$\\)"
                   0 'font-lock-string-face keep))))
  (font-lock-add-keywords 'markdown-mode patterns)
  (font-lock-add-keywords 'gfm-mode patterns))

;; Use curly quotes in text and Markdown modes
(require 'smart-quotes)
(add-hook 'text-mode-hook #'turn-on-smart-quotes)
(add-hook 'markdown-mode-hook #'turn-on-smart-quotes)


(put 'downcase-region 'disabled nil)

(put 'upcase-region 'disabled nil)
