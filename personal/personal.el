(setq-default cursor-type 'bar)
(set-cursor-color "#bf0000")

(prelude-require-package 'multiple-cursors)
(prelude-require-package 'org-bullets)
;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(prelude-require-package 'color-theme-sanityinc-tomorrow)
;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; general prelude override
;;(disable-theme 'zenburn)
(global-linum-mode t)

;;(prelude-require-package 'color-theme-sanityinc-tomorrow)
;; (setq prelude-theme 'color-theme-sanityinc-tomorrow-bright)
(ido-mode 1)


;; yasnippet
;;(add-to-list 'load-path (expand-file-name "yasnippet" prelude-dir))
;;(prelude-require-package 'yasnippet)
;;(prelude-require-package 'yasnippet-snippets)

;;(yas-global-mode 1)
(scroll-bar-mode -1)

;; project tree explorer

(prelude-require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)
;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))



(add-hook 'markdown-mode-hook (lambda()
                                (whitespace-mode -1)))

;; ---------------------- ORG MODE START------------------------------------
(setq org-bullets-bullet-list '("❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽" "❾"
                                "➀" "➁" "➂" "➃" "➄" "➅" "➆"))
(global-visual-line-mode 1)




;; org mode settings
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(prelude-require-package 'org)

(prelude-require-package 'ox-gfm)
(eval-after-load "org"
  '(require 'ox-gfm nil t))
(defvar org-notes-dir "~/Dropbox/org-notes")
(defvar org-todays-task-file-location (concat  org-notes-dir "/todays-task.org"))
(defvar org-notes-file-location (concat  org-notes-dir "/notes.org"))
(defvar org-journal-file-location (concat  org-notes-dir "/journal.org"))
(defvar org-blog-idea-file-location (concat  org-notes-dir "/blog-in-draft.org"))
(defvar org-archive-file-location (concat  org-notes-dir "/archive.org::"))

;; overriding org-mode system settings
(setq org-default-notes-file org-notes-file-location)
(setq org-agenda-files '("~/Dropbox/org-notes"))
(print org-agenda-files)
(setq org-archive-location org-archive-file-location)


(setq org-default-notes-file org-todays-task-file-location
      initial-buffer-choice  org-default-notes-file)

(add-hook 'org-mode-hook
          (lambda()
            (org-bullets-mode 1)
            (whitespace-mode -1)
            ))


(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-todays-task-file-location "Tasks")
         "* TODO %?\nEntered on %T\n")
        ("j" "Journal" entry (file+datetree org-journal-file-location)
         "* %?\nEntered on %T\n")
        ("b" "Blog Idea" entry (file+headline org-blog-idea-file-location "ELABORATE")
         "\n* TODO %?\nEntered on %T\n")
        ))


;; org-babel settings
(require 'ob-python)
(org-babel-do-load-languages
 'org-babel-load-languages
 '((python . t)))
(setq org-babel-python-command "python3")

;; ---------------------------- ORG MODE END ---------------------------

;;----------------- Python IDE setup --------------------
(setq python-shell-interpreter "/home/munawwarhussain/.pyenv/shims/ipython"
      python-shell-interpreter-args "-i --simple-prompt")
(prelude-require-packages '(elpy py-autopep8 epc jedi))
(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq elpy-rpc-python-conmmand "/home/munawwarhussain/.pyenv/shims/python3")

;; Global Jedi config vars

(defvar jedi-config:use-system-python nil
  "Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)")

(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

(defvar jedi-config:vcs-root-sentinel ".git")

(defvar jedi-config:python-module-sentinel "__init__.py")


;;---------------------------- Python End --------------------------

;; -------------------------- Ledger Mode ---------------------------
(prelude-require-package 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))

;; ------------------------------ ledger END-------------------------

;; -------------- multi term config -------------
(prelude-require-package 'multi-term)
(setq multi-term-program "/bin/zsh")

(global-set-key (kbd "C-c T") 'multi-term)
(global-set-key (kbd "C-c t") 'multi-term-next)
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)
            (setq show-trailing-whitespace nil)
            (linum-mode -1)
            ;;(autopair-mode -1)
            (define-key term-raw-map (kbd "C-y") 'term-paste)
            (add-to-list 'term-bind-key-alist '("C-c p" . multi-term-prev))
            (add-to-list 'term-bind-key-alist '("C-c n" . multi-term-next))))

(defcustom term-unbind-key-list
  '("C-z" "C-x" "C-c" "C-h" "C-y" "<ESC>")
  "The key list that will need to be unbind."
  :type 'list
  :group 'multi-term)

(defcustom term-bind-key-alist
  '(
    ("C-c C-c" . term-interrupt-subjob)
    ("C-p" . previous-line)
    ("C-n" . next-line)
    ("C-e" . end-line)
    ("C-a" . beginning-of-line)
    ("C-s" . isearch-forward)
    ("C-r" . isearch-backward)
    ("C-m" . term-send-raw)
    ("M-f" . term-send-forward-word)
    ("M-b" . term-send-backward-word)
    ("M-o" . term-send-backspace)
    ("M-p" . term-send-up)
    ("M-n" . term-send-down)
    ("M-M" . term-send-forward-kill-word)
    ("M-N" . term-send-backward-kill-word)
    ("M-r" . term-send-reverse-search-history)
    ("M-," . term-send-input)
    ("M-." . comint-dynamic-complete))
  "The key alist that will need to be bind.
  If you do not like default setup, modify it, with (KEY . COMMAND) format."
  :type 'alist
  :group 'multi-term)

;; -------------- Multi term config ends  ----------------

;; --------- [start] C / C++ configuration  --------------
;; this variables must be set before load helm-gtags
;; you can change to any prefix key of your choice
(prelude-require-package 'ggtags)
(require 'ggtags)
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode)
              (ggtags-mode 1))))

(dolist (map (list ggtags-mode-map dired-mode-map))
  (define-key map (kbd "C-c g s") 'ggtags-find-other-symbol)
  (define-key map (kbd "C-c g h") 'ggtags-view-tag-history)
  (define-key map (kbd "C-c g r") 'ggtags-find-reference)
  (define-key map (kbd "C-c g f") 'ggtags-find-file)
  (define-key map (kbd "C-c g c") 'ggtags-create-tags)
  (define-key map (kbd "C-c g u") 'ggtags-update-tags)
  (define-key map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
  (define-key map (kbd "M-.") 'ggtags-find-tag-dwim)
  (define-key map (kbd "M-,") 'pop-tag-mark)
  (define-key map (kbd "C-c <") 'ggtags-prev-mark)
  (define-key map (kbd "C-c >") 'ggtags-next-mark))


(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t
 )

(prelude-require-package 'helm-gtags)
(require 'helm-gtags)
;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "M-s") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)


;; ------- [end] c/c++ configuration ---------------------
