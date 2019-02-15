(defvar lunar-highlights nil "")

(setq lunar-highlights
      '(("\\<\\(class\\|where\\|fn\\|infix\\|inst\\|let\\|if\\)\\>" . font-lock-keyword-face)
	("//.*" . font-lock-comment-face)
	(setq indent-tabs-mode nil)
        ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)))

(define-derived-mode lunar-mode prog-mode "lunar"
  "major mode for editing Lunar language code."
  (setq font-lock-defaults '(lunar-highlights))
  (setq tab-width 4)
  ;; (set (make-local-variable 'indent-line-function) 'lunar-indent-line)
  )

;; (defconst lunar-mode-smie-grammar
;;   (smie-prec2->grammar
;;    (smie-bnf->prec2
;;     '((id)
;;       (inst ("{" insts "}"))
;;       ))))

;; (smie-setup lunar-mode-smie-grammar 'lunar-mode-smie-rules
;;             :forward-token lunar-mode-forward-token
;;             :backward-token 'lunar-mode-backward-token)

(defun lunar-indent-line ()
  "Indent current line as Lunar language."
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0) ; First line is always non-indented
    (let ((not-indented t) cur-indent)
      (if (looking-at "^[ \t]*[})]") ; If the line we are looking at is the end of a block, then decrease the indentation
	  (progn
	    (save-excursion
	      (forward-line -1)
	      (setq cur-indent (- (current-indentation) tab-width)))
	    (if (< cur-indent 0) ; We can't indent past the left margin
		(setq cur-indent 0)))
	(save-excursion
	  (while not-indented ; Iterate backwards until we find an indentation hint
	    (forward-line -1)
	    (if (looking-at "^.*[})]") ; This hint indicates that we need to indent at the level of the } or ) token
		(progn
		  (setq cur-indent (current-indentation))
		  (setq not-indented nil))
	      (if (looking-at "^.*[{(]") ; This hint indicates that we need to indent an extra level
		  (progn
		    (setq cur-indent (+ (current-indentation) tab-width)) ; Do the actual indenting
		    (setq not-indented nil))
		(if (bobp)
		    (setq not-indented nil)))))))
      (if cur-indent
	  (indent-line-to cur-indent)
	(indent-line-to 0))))) ; If we didn't see an indentation hint, then allow no indentation

(add-to-list 'auto-mode-alist '("\\.lunar\\'" . lunar-mode))

(provide 'lunar-mode)
