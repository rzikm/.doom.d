;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Radek Zikmund")
(setq user-mail-address "r.zikmund.rz@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code Regular" :size 12))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'sanityinc-tomorrow-night)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(setq custom-safe-themes t)
(setq backup-directory-alist `(("." . "~/.backups")))
(setq-default fill-column 100)
(setq column-enforce-column 100)
(setq display-line-numbers-type 'relative)

;; custom keymaps
(map! :nv ";" #'evil-ex)
(map! :map evil-snipe-parent-transient-map ";" #'evil-ex)
(map! :nv ":" #'evil-snipe-repeat)

;; simple ask
(fset 'yes-or-no-p 'y-or-n-p)
(put 'dired-find-alternate-file 'disabled nil)


(defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                            :hint nil)
  "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
  ("j" git-gutter:next-hunk)
  ("k" git-gutter:previous-hunk)
  ("h" (progn (goto-char (point-min))
              (git-gutter:next-hunk 1)))
  ("l" (progn (goto-char (point-min))
              (git-gutter:previous-hunk 1)))
  ("s" git-gutter:stage-hunk)
  ("r" git-gutter:revert-hunk)
  ("p" git-gutter:popup-hunk)
  ("R" git-gutter:set-start-revision)
  ("q" nil :color blue)
  ("Q" (progn (git-gutter-mode -1)
              ;; git-gutter-fringe doesn't seem to
              ;; clear the markup right away
              (sit-for 0.1)
              (git-gutter:clear))
       :color blue))
(define-key global-map (kbd "M-g M-g") 'hydra-git-gutter/body)

(use-package helm
  :diminish helm-mode
  :commands helm-mode
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t)
  (setq helm-autoresize-mode t)
  (setq helm-buffer-max-length 40)
  (define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)
  (define-key helm-read-file-map (kbd "C-k")  'helm-find-files-up-one-level))

(use-package git-timemachine)

(use-package evil-collection)

(use-package org
  :config
  (setq org-startup-indented t)
  (setq org-indent-indentation-per-level 1))

(use-package evil-easymotion
  :config
  (evilem-default-keybindings "SPC")
  (setq avy-keys (number-sequence ?a ?z)))

(use-package pos-tip)

(use-package company
  :defer t
  :config
  (add-hook 'prog-mode-hook (lambda () (company-mode 1)))

  (setq company-idle-delay 0)
  (setq company-selection-wrap-around t)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  (setq company-show-numbers t)
  (setq company-dabbrev-downcase nil)

  (define-key company-active-map (kbd "ESC") 'company-abort)
  (define-key company-active-map (kbd "C-n") 'company-select-next-or-abort)
  (define-key company-active-map (kbd "C-p") 'company-select-previous-or-abort))

(use-package swiper
  :commands swiper
  :bind ("C-s" . counsel-grep-or-swiper)
  :config
  (require 'counsel)
  (setq counsel-grep-base-command "grep -niE \"%s\" %s")
  (setq ivy-height 20))

(use-package rx)

(use-package company-quickhelp
  :init
  (company-quickhelp-mode t))

(use-package flycheck)

(use-package yasnippet
  :defer t
  :config
  (add-hook 'prog-mode-hook (lambda () (yas-minor-mode)))
  (yas-reload-all)
  ;(setq yas-snippet-dirs '("~/.emacs.d/snippets"
  ;                         "~/.emacs.d/remote-snippets"))
  (setq tab-always-indent 'complete)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt
                               yas-dropdown-prompt))
  (define-key yas-minor-mode-map (kbd "<escape>") 'yas-exit-snippet))

(use-package yasnippet-snippets)         ; Collection of snippets

(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
    (backward-char 1)
    (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
    ;(message "tab-indent-or-complete")
  (cond
   ((minibufferp)
    (minibuffer-complete))
   (t
    (indent-for-tab-command)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
    (if (check-expansion)
        (progn
          (company-manual-begin)
          (if (null company-candidates)
          (progn
            (company-abort)
            (indent-for-tab-command)))))))))

(defun tab-complete-or-next-field ()
  (interactive)
  ;(message "tab-or-complete-or-next-field")
  (if (or (not yas/minor-mode)
      (null (do-yas-expand)))
      (if company-candidates
      (company-complete-selection)
    (if (check-expansion)
      (progn
        (company-manual-begin)
        (if (null company-candidates)
        (progn
          (company-abort)
          (yas-next-field))))
      (yas-next-field)))))

(defun expand-snippet-or-complete-selection ()
  (interactive)
  ;(message "expand-snippet-or-complete-selection")
  (if (or (not yas/minor-mode)
      (null (do-yas-expand))
      (company-abort))
      (company-complete-selection)))

(defun abort-company-or-yas ()
  (interactive)
  (if (null company-candidates)
      (yas-abort-snippet)
    (company-abort)))

;;; Maps to better reconcile behavior of yasnippet and company
(define-key prog-mode-map [tab] 'tab-indent-or-complete)
(define-key prog-mode-map (kbd "TAB") 'tab-indent-or-complete)
(define-key prog-mode-map [(control return)] 'company-complete-common)

(define-key company-active-map [tab] 'company-complete-selection)
(define-key company-active-map (kbd "TAB") 'company-complete-selection)

(define-key yas-keymap [tab] 'tab-complete-or-next-field)
(define-key yas-keymap (kbd "TAB") 'tab-complete-or-next-field)
(define-key yas-keymap [(control tab)] 'yas-next-field)
(define-key yas-keymap (kbd "C-g") 'abort-company-or-yas)

(use-package cmake-mode)

(use-package markdown-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.md\\’" . markdown-mode)))

(setq markdown-command "pandoc")

(use-package flymd
  :config
  (setq flymd-output-directory temporary-file-directory))

(setq server-auth-dir "~/.emacs.d/server")

(use-package impatient-mode)

(defun markdown-html (buffer)
  (princ (with-current-buffer buffer
           (format "<!DOCTYPE html><html><title>Impatient Markdown</title><xmp theme=\"united\" style=\"display:none;\"> %s  </xmp><script src=\"http://strapdownjs.com/v/0.2/strapdown.js\"></script></html>" (buffer-substring-no-properties (point-min) (point-max))))
         (current-buffer)))

(defun markdown-filter (buffer)
     (princ
       (with-temp-buffer
         (let ((tmpname (buffer-name)))
           (set-buffer buffer)
           (set-buffer (markdown tmpname)) ; the function markdown is in `markdown-mode.el'
           (buffer-string)))
       (current-buffer)))

(use-package which-key
  :config
  (which-key-mode 1))

(use-package iedit)

(defun narrow-or-widen-dwim (p)
  "Widen if buffer is narrowed, narrow-dwim otherwise.
Dwim means: region, org-src-block, org-subtree, or
defun, whichever applies first. Narrowing to
org-src-block actually calls `org-edit-src-code'.

With prefix P, don't widen, just narrow even if buffer
is already narrowed."
  (interactive "P")
  (declare (interactive-only))
  (cond ((and (buffer-narrowed-p) (not p)) (widen))
        ((region-active-p)
         (narrow-to-region (region-beginning)
                           (region-end)))
        ((derived-mode-p 'org-mode)
         ;; `org-edit-src-code' is not a real narrowing
         ;; command. Remove this first conditional if
         ;; you don't want it.
         (cond ((ignore-errors (org-edit-src-code) t)
                (delete-other-windows))
               ((ignore-errors (org-narrow-to-block) t))
               (t (org-narrow-to-subtree))))
        ((derived-mode-p 'latex-mode)
         (LaTeX-narrow-to-environment))
        (t (narrow-to-defun))))

(global-set-key "\M-n" #'narrow-or-widen-dwim)

(use-package yaml-mode
  :config
  ; auto indent on enter
  (add-hook 'yaml-mode-hook (lambda () (define-key yaml-mode-map "\C-m" 'newline-and-indent))))

(use-package powershell)

(use-package column-enforce-mode
  :config
  (global-column-enforce-mode))

(use-package smerge-mode
  :bind
  ("C-c m" . hydra-smerge/body)
  :config
  (defhydra hydra-smerge (:color pink
                                 :hint nil
                                 :pre (smerge-mode 1)
                                 ;; Disable `smerge-mode' when quitting hydra if
                                 ;; no merge conflicts remain.
                                 :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: mine/base        _C_ombine
_p_rev       _m_ine               _=_: mine/other       _r_esolve
^^           _o_ther              _>_: base/other       _d_: kill current
^^           _a_ll                _R_efine              _s_wap
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("m" smerge-keep-mine)
    ("o" smerge-keep-other)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-mine)
    ("=" smerge-diff-mine-other)
    (">" smerge-diff-base-other)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("d" smerge-kill-current)
    ("s" smerge-swap)
    ("q" nil "cancel" :color blue)))

(use-package color-theme-sanityinc-tomorrow)

(use-package lsp-mode
  :init (setq lsp-keymap-prefix "C-l")
  :defer t
  :hook ((fsharp-mode . lsp)
         (python-mode . lsp)
         (php-mode . lsp)
         (js2-mode . lsp)
         (powershell-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
(use-package dap-mode)

(use-package helm-lsp)

(use-package multi-term
  :config
  (if (eq system-type 'windows-nt)
      (setq multi-term-program "powershell.exe")
    ))

(use-package helm-posframe
  :config
  (progn
    (helm-posframe-enable)
    (setq helm-posframe-poshandler 'posframe-poshandler-frame-center)
    (setq helm-posframe-parameters
          '((left-fringe . 10)
            (right-fringe . 10)))))

(use-package which-key-posframe
  :config
  (which-key-posframe-mode))

(evil-put-command-property 'evil-yank-line :motion 'evil-line)

(load! "init-font")
(load! "init-powerline")
