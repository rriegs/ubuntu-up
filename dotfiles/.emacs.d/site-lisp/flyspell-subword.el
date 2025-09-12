;;; flyspell-subword.el --- fix Flyspell in subword-mode

;; Author: Ryan Riegel <rriegs@gmail.com>
;; Created: 2018-02-15

;;; Commentary:

;; Flyspell has no built-in support for subword-mode, however, it is
;; possible to adapt Flyspell to be more competent when subword-mode
;; is active.  There are three primary changes that need to be made:
;;
;;  1. Word boundary detection needs to updated for both Flyspell
;;     and ispell
;;
;;  2. `flyspell-small-region' needs to be updated to advance by
;;     subwords, not whole words
;;
;;      a. `flyspell-large-region' has similar issues which are more
;;         difficult to fix, though aspell offers a pseudo-solution
;;
;;  3. `flyspell-post-command-hook' needs to know to trigger after
;;     typing a subword boundary
;;
;; This file is also compatible with disabling subword-mode, though
;; it still makes some changes: notably, it unconditionally calls
;; `flyspell-region' in `flyspell-post-command-hook'.

;;; Code:

(require 'flyspell)
(require 'ispell)

;; Like looking-at but with an offset
(defun sub-flyspell-looking-at (regexp &optional offset)
  (let ((pos (+ (or offset 0) (point))))
    (when (and (>= pos (point-min)) (< pos (point-max)))
      (string-match regexp (string (char-after pos))))))

;; Call the right forward function and move past otherchars
(defun sub-flyspell-forward (&optional backward)
  (let ((ispell-casechars (ispell-get-casechars))
        (ispell-otherchars (ispell-get-otherchars))
        (ispell-many-otherchars-p (ispell-get-many-otherchars-p))
        (offset (if backward -1 0))
        (dir (if backward -1 1))
        (continue t))
    (if subword-mode (subword-forward dir) (forward-word dir))
    (while (and continue
                (not (string= "" ispell-otherchars))
                (sub-flyspell-looking-at ispell-otherchars offset)
                (sub-flyspell-looking-at ispell-casechars (+ dir offset)))
      (if subword-mode (subword-forward dir) (forward-word dir))
      (setq continue ispell-many-otherchars-p))))
(defun sub-flyspell-backward () (sub-flyspell-forward t))

;; Properly find boundaries of words in CamelCase
(defun sub-flyspell-get-word (orig-fun &optional following extra-otherchars)
  (if (not subword-mode)
      (funcall orig-fun following extra-otherchars)
    (if following (sub-flyspell-forward) (if (not (eobp)) (forward-char)))
    (let* ((beg (progn (sub-flyspell-backward) (point-marker)))
           (end (progn (sub-flyspell-forward) (point-marker)))
           (word (buffer-substring-no-properties beg end)))
      (list word beg end))))
(advice-add #'ispell-get-word :around #'sub-flyspell-get-word)
(advice-add #'flyspell-get-word :around #'sub-flyspell-get-word)

;; Simplify and use sub-flyspell-forward to handle CamelCase words
(defun sub-flyspell-small-region (orig-fun beg end)
  ;; (if (not subword-mode)
  ;;     (funcall orig-fun beg end)
  (save-excursion
    (if (> beg end) (setq beg (prog1 end (setq end beg))))
    (if (< beg (point-min)) (setq beg (point-min)))
    (if (> end (point-max)) (setq end (point-max)))
    (goto-char beg)
    (while (< (point) end)
      (flyspell-word t)
      ;; (sit-for 0) ;; uncomment to enable animation
      (sub-flyspell-forward))))
(advice-add #'flyspell-small-region :around #'sub-flyspell-small-region)

;; Fake handling of CamelCase words in flyspell-large-region
(defun sub-flyspell-large-region (orig-fun beg end)
  (let ((ispell-extra-args ispell-extra-args)
        (subword-mode subword-mode))
    (when (and subword-mode (string-match "aspell\\'" ispell-program-name))
      (push "--camel-case" ispell-extra-args)
      (setq subword-mode nil))
    (funcall orig-fun beg end)))
(advice-add #'flyspell-large-region :around #'sub-flyspell-large-region)

;; Only check the previous word if no longer editing it
(defun sub-flyspell-check-pre-word-p ()
  (and (eq flyspell-pre-buffer (current-buffer))
       (numberp flyspell-pre-point)
       (/= (save-excursion
             (goto-char (1- flyspell-pre-point))
             (sub-flyspell-forward)
             (point))
           (save-excursion
             (if (not (bobp)) (backward-char))
             (sub-flyspell-forward)
             (point)))))

;; Simplify and use flyspell-region in the post-command-hook
(defun sub-flyspell-post-command-hook (orig-fun)
  (when flyspell-mode
    (with-local-quit
      (let ((command this-command)
            deactivate-mark)
        ;; Handle word at previous location
        (when (sub-flyspell-check-pre-word-p)
          (save-excursion
            (goto-char (1- flyspell-pre-point))
            (flyspell-word)))
        ;; Handle word at current location
        (when (flyspell-check-word-p)
          (flyspell-word))
        ;; Handle all other recent changes
        (while (and (not (input-pending-p)) (consp flyspell-changes))
          (let* ((change (pop flyspell-changes))
                 (beg (car change))
                 (end (cdr change)))
            (flyspell-region beg end)))
        (setq flyspell-previous-command command)))))
(advice-add #'flyspell-post-command-hook :around #'sub-flyspell-post-command-hook)

(provide 'flyspell-subword)

;;; flyspell-subword.el ends here
