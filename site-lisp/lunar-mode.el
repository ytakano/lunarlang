(defvar lunar-highlights nil "")

(setq lunar-highlights
      '(("//.*" . font-lock-comment-face)
	("`\\<\\w+\\>" . font-lock-variable-name-face)
	("\\<\\(class\\|where\\|fn\\|infix\\|inst\\|let\\|if\\)\\>" . font-lock-keyword-face)
        ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)))

(define-derived-mode lunar-mode prog-mode "lunar"
  "major mode for editing Lunar language."
  (setq font-lock-defaults '(lunar-highlights))
  (set (make-local-variable 'tab-width) 4)
  (set (make-local-variable 'indent-tabs-mode) nil)
  (set (make-local-variable 'indent-line-function) 'lunar-indent-line))

(defun lunar-indent-num ()
  (interactive)
  (let ((num 0) (line (split-string (thing-at-point `line t) "")))
    (while line
      (let ((s (car line)))
	(if (or (string= s "{") (string= s "("))
	    (setq num (+ num 1))
	  (if (or (string= s "}") (string= s ")"))
	      (setq num (- num 1)))))
      (setq line (cdr line)))
    num))

(defun lunar-test ()
  (interactive)
  (print (lunar-indent-num)))

(defun lunar-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0) ; First line is always non-indented
    (let ((indent-num 0))
      (progn
	(save-excursion
	  (forward-line -1)
	  (setq indent-num (+ (current-indentation)
			      (* tab-width (lunar-indent-num))))
	  (if (looking-at "[ \t]*[})]")
	      (setq indent-num (+ indent-num tab-width))))
	(if (looking-at "^[ \t]*[})]")
	    (setq indent-num (- indent-num tab-width)))
	(if (< indent-num 0)
	    (indent-line-to 0)
	  (indent-line-to indent-num))))))

(add-to-list 'auto-mode-alist '("\\.lunar\\'" . lunar-mode))

(provide 'lunar-mode)
