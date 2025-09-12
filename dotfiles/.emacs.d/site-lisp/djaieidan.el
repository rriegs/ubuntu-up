;;; djaieidan.el --- translate to and from the Djaieidan cipher

;; Author: Ryan Riegel <rriegs@gmail.com>
;; Created: 2021-08-13

;;; Commentary:

;; The entry points, `djaiei-encode', `djaiei-encode-string',
;; `djaiei-encode-region', and their decode counterparts, perform a
;; mildly elaborated substitution cipher on buffers and strings.

;;; Code:

(defvar djaiei-translate-table
  (make-translation-table-from-alist
   '((?a . ?y) (?A . ?Y)
     (?b . ?p) (?B . ?P)
     (?c . ?l) (?C . ?L)
     (?d . ?t) (?D . ?T)
     (?e . ?a) (?E . ?A)
     (?f . ?v) (?F . ?V)
     (?g . ?k) (?G . ?K)
     (?h . ?r) (?H . ?R)
     (?i . ?e) (?I . ?E)
     (?j . ?z) (?J . ?Z)
     (?k . ?g) (?K . ?G)
     (?l . ?m) (?L . ?M)
     (?m . ?s) (?M . ?S)
     (?n . ?h) (?N . ?H)
     (?o . ?u) (?O . ?U)
     (?p . ?b) (?P . ?B)
     (?q . ?x) (?Q . ?X)
     (?r . ?n) (?R . ?N)
     (?s . ?c) (?S . ?C)
     (?t . ?d) (?T . ?D)
     (?u . ?i) (?U . ?I)
     (?v . ?j) (?V . ?J)
     (?w . ?f) (?W . ?F)
     (?x . ?q) (?X . ?Q)
     (?y . ?o) (?Y . ?O)
     (?z . ?w) (?Z . ?W)))
  "Association list table for Djaieidan translation.")

;;;###autoload
(defun djaiei-encode (object &optional start end)
  "Djaiei encrypt OBJECT, a buffer or string.
If OBJECT is a buffer, encrypt the region between START and END.
If OBJECT is a string, encrypt it in its entirety, ignoring START
and END, and return the encrypted string."
  (if (bufferp object)
      (with-current-buffer object
	(djaiei-encode-region start end))
    (djaiei-encode-string object)))

;;;###autoload
(defun djaiei-encode-string (string)
  "Return Djaiei encryption of STRING."
  (with-temp-buffer
    (insert string)
    (djaiei-encode-region (point-min) (point-max))
    (buffer-string)))

;;;###autoload
(defun djaiei-encode-region (start end)
  "Djaiei encrypt the region between START and END in current buffer."
  (interactive "r")
  (translate-region start end djaiei-translate-table))

(provide 'djaieidan)

;;; djaieidan.el ends here
