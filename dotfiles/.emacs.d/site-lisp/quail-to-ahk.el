;;; quail-to-ahk.el --- convert a quail map to an ahk script

;; Author: Ryan Riegel <rriegs@gmail.com>
;; Created: 2023-11-03

;;; Commentary:

;; The entry point, `quail-to-ahk', unpacks a tree-structured quail
;; map into a flat and correctly formatted AutoHotkey v2 script.

;;; Code:

(require 'quail)

(defun quail-to-ahk-internal (map prefix)
  (let ((translation (car map))
        (alist (cdr map)))
    (if (functionp translation)
        (setq translation (funcall translation prefix (length prefix))))
    (if (vectorp translation)
        (setq translation (aref translation 0)))
    (if (stringp translation)
        (setq translation (aref translation 0)))
    (if translation
        (insert (format "%05X  %-38s\n" translation prefix)))
                        ;; translation prefix translation
                        ;; (get-char-code-property translation 'name))))
    (if (functionp alist)
        (setq alist (funcall alist)))
    (dolist (next alist)
      (let ((key (string (car next))))
        ;; (if (memq (car next) '(?` ?:))
        ;;     (setq key (concat "`" key)))
        (quail-to-ahk-internal (cdr next) (concat prefix key))))))

;;;###autoload
(defun quail-to-ahk ()
  "Convert `quail-map' into an AutoHotkey v2 script."
  (interactive)
  (quail-to-ahk-internal (quail-map) ""))

(provide 'quail-to-ahk)

;;; quail-to-ahk.el ends here
