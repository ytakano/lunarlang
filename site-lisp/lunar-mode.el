(defvar lunar-highlights nil "")

(setq lunar-highlights
      '(("//.*" . font-lock-comment-face)
	("`\\<\\w+\\>" . font-lock-variable-name-face)
	("\\<\\(bool\\|u64\\|s64\\|u32\\|s32\\|u16\\|s16\\|u8\\|s8\\)\\>" . font-lock-type-face)
	("\\<\\(class\\|require\\|func\\|infix\\|instance\\|let\\|in\\|if\\|elif\\|else\\|match\\|struct\\|union\\)\\>" . font-lock-keyword-face)
        ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)))

(define-derived-mode lunar-mode prog-mode "lunar"
  "major mode for editing Lunar language."
  (setq font-lock-defaults '(lunar-highlights))
  (set (make-local-variable 'comment-start) "//")
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

(defun lunar-backward ()
  (interactive)
  (catch 'exit
    (while t
      (forward-line -1)
      (if (or (not (string= (thing-at-point `line t) "\n"))
	      (bobp))
	  (throw 'exit "exit loop")))))

(defun lunar-indent-line ()
  (interactive)
  (beginning-of-line)
  (if (bobp)
      (indent-line-to 0) ; First line is always non-indented
    (let ((indent-num 0))
      (progn
	(save-excursion
	  (lunar-backward)
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
