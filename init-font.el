(set-face-attribute 'default nil
                    :family "Fira Code"
                    :weight 'normal
                    :width 'normal)

;; (defun my-correct-symbol-bounds (pretty-alist)
;;   "Prepend a TAB character to each symbol in this alist,
;; this way compose-region called by prettify-symbols-mode
;; will use the correct width of the symhttps://github.com/johnw42/fira-code-emacs/tree/217f3f540d8d25fb825da484b076d1e4345e6150bols
;; instead of the width measured by char-width."
;;   (mapcar (lambda (el)
;;             (setcdr el (string ?\t (cdr el)))
;;             el)
;;           pretty-alist))

;; (defun fira-code-ligature-list (ligatures codepoint-start)
;;   "Create an alist of strings to replace with
;; codepoints starting from codepoint-start."
;;   (let ((codepoints (-iterate '1+ codepoint-start (length ligatures))))
;;     (-zip-pair ligatures codepoints)))

;; (setq fira-code-ligatures
;;       (let* ((ligs '("www" "**" "***" "**/" "*>" "*/" "\\\\" "\\\\\\"
;;                      "{-" "[]" "::" ":::" ":=" "!!" "!=" "!==" "-}"
;;                      "--" "---" "-->" "->" "->>" "-<" "-<<" "-~"
;;                      "#{" "#[" "##" "###" "####" "#(" "#?" "#_" "#_("
;;                      ".-" ".=" ".." "..<" "..." "?=" "??" ";;" "/*"
;;                      "/**" "/=" "/==" "/>" "//" "///" "&&" "||" "||="
;;                      "|=" "|>" "^=" "$>" "++" "+++" "+>" "=:=" "=="
;;                      "===" "==>" "=>" "=>>" "<=" "=<<" "=/=" ">-" ">="
;;                      ">=>" ">>" ">>-" ">>=" ">>>" "<*" "<*>" "<|" "<|>"
;;                      "<$" "<$>" "<!--" "<-" "<--" "<->" "<+" "<+>" "<="
;;                      "<==" "<=>" "<=<" "<>" "<<" "<<-" "<<=" "<<<" "<~"
;;                      "<~~" "</" "</>" "~@" "~-" "~=" "~>" "~~" "~~>" "%%"
;;                      "x" ":" "+" "+" "*")))
;;         (my-correct-symbol-bounds (fira-code-ligature-list ligs #Xe100))))

;; ;; nice glyphs for haskell with hasklig
;; (defun set-fira-code-ligatures ()
;;   "Add hasklig ligatures for use with prettify-symbols-mode."
;;   (set-fontset-font t '(#Xe100 . #Xe189) "Fira Code Symbol")
;;   (setq prettify-symbols-alist
;;         (append fira-code-ligatures prettify-symbols-alist))
;;   (prettify-symbols-mode))

;; (add-hook 'prog-mode-hook #'set-fira-code-ligatures)

(let ((alist '((33 . ".\\(?:\\(?:==\\|!!\\)\\|[!=]\\)")
               (35 . ".\\(?:###\\|##\\|_(\\|[#(?[_{]\\)")
               (36 . ".\\(?:>\\)")
               (37 . ".\\(?:\\(?:%%\\)\\|%\\)")
               (38 . ".\\(?:\\(?:&&\\)\\|&\\)")
               (42 . ".\\(?:\\(?:\\*\\*/\\)\\|\\(?:\\*[*/]\\)\\|[*/>]\\)")
               (43 . ".\\(?:\\(?:\\+\\+\\)\\|[+>]\\)")
               (45 . ".\\(?:\\(?:-[>-]\\|<<\\|>>\\)\\|[<>}~-]\\)")
               (46 . ".\\(?:\\(?:\\.[.<]\\)\\|[.=-]\\)")
               (47 . ".\\(?:\\(?:\\*\\*\\|//\\|==\\)\\|[*/=>]\\)")
               (48 . ".\\(?:x[a-zA-Z]\\)")
               (58 . ".\\(?:::\\|[:=]\\)")
               (59 . ".\\(?:;;\\|;\\)")
               (60 . ".\\(?:\\(?:!--\\)\\|\\(?:~~\\|->\\|\\$>\\|\\*>\\|\\+>\\|--\\|<[<=-]\\|=[<=>]\\||>\\)\\|[*$+~/<=>|-]\\)")
               (61 . ".\\(?:\\(?:/=\\|:=\\|<<\\|=[=>]\\|>>\\)\\|[<=>~]\\)")
               (62 . ".\\(?:\\(?:=>\\|>[=>-]\\)\\|[=>-]\\)")
               (63 . ".\\(?:\\(\\?\\?\\)\\|[:=?]\\)")
               (91 . ".\\(?:]\\)")
               (92 . ".\\(?:\\(?:\\\\\\\\\\)\\|\\\\\\)")
               (94 . ".\\(?:=\\)")
               (119 . ".\\(?:ww\\)")
               (123 . ".\\(?:-\\)")
               (124 . ".\\(?:\\(?:|[=|]\\)\\|[=>|]\\)")
               (126 . ".\\(?:~>\\|~~\\|[>=@~-]\\)")
               )
             ))
  (dolist (char-regexp alist)
    (set-char-table-range composition-function-table (car char-regexp)
                          `([,(cdr char-regexp) 0 font-shape-gstring]))))
;; Note! If you get error in process filter: Attempt to shape unibyte text, check out this
;; issue. Emacs Cider users may avoid this issue by commenting the line with (46 and maybe also 45

