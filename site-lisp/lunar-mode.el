(defvar lunar-highlights nil "")

(setq lunar-highlights
      '(("\\<\\(class\\|where\\|fn\\|infix\\|inst\\|let\\|if\\)\\>" . font-lock-keyword-face)
	("//.*" . font-lock-comment-face)
        ("\\<\\(true\\|false\\)\\>" . font-lock-constant-face)))

(define-derived-mode lunar-mode prog-mode "lunar"
  "major mode for editing Lunar language code."
  (setq font-lock-defaults '(lunar-highlights)))

;; (defconst lunar-mode-smie-grammar
;;   (smie-prec2->grammar
;;    (smie-bnf->prec2
;;     '((id)
;;       (inst ("{" insts "}"))
;;       ))))

;; (smie-setup lunar-mode-smie-grammar 'lunar-mode-smie-rules
;;             :forward-token lunar-mode-forward-token
;;             :backward-token 'lunar-mode-backward-token)


(add-to-list 'auto-mode-alist '("\\.lunar\\'" . lunar-mode))

(provide 'lunar-mode)
