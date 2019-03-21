(setq-default cursor-type 'bar)
(set-cursor-color "#bf0000")

(prelude-require-package 'multiple-cursors)
(prelude-require-package 'org-bullets)
;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; general prelude override
(disable-theme 'zenburn)
(global-linum-mode t)

(prelude-require-package 'color-theme-sanityinc-tomorrow)
(setq prelude-theme 'sanityinc-tomorrow-eighties)
(ido-mode 1)


;; yasnippet
(add-to-list 'load-path (expand-file-name "yasnippet" prelude-dir))
(prelude-require-package 'yasnippet)
(prelude-require-package 'yasnippet-snippets)

(yas-global-mode 1)
(scroll-bar-mode -1)

;; project tree explorer

(prelude-require-package 'neotree)
(global-set-key [f8] 'neotree-toggle)
;;(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i")

(add-hook 'markdown-mode-hook (lambda()
                                (whitespace-mode -1)))

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

;;----------------- Email setup ------------------------
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(require 'mu4e)
`1m
(setq mu4e-contexts
      `( ,(make-mu4e-context
           :name "Gmail"
           :match-func (lambda (msg) (when msg
                                       (string-prefix-p "/Gmail" (mu4e-message-field msg :maildir))))
           :vars '(
                   (mu4e-trash-folder . "/Gmail/[Gmail].Trash")
                   (mu4e-refile-folder . "/Gmail/[Gmail].Archive")
                   ))
         ))


;; I have my "default" parameters from Gmail
(setq mu4e-sent-folder "/sent"
      ;; mu4e-sent-messages-behavior 'delete ;; Unsure how this should be configured
      mu4e-drafts-folder "/drafts"
      user-mail-address "munawwar.shelia@accionlabs.com"
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587)

;; Now I set a list of
(defvar my-mu4e-account-alist
  '(("Gmail"
     (mu4e-sent-folder "/Gmail/sent")
     (user-mail-address "munawwar.shelia@accionlabs.com")
     (smtpmail-smtp-user "munawwar.shelia")
     (smtpmail-local-domain "gmail.com")
     (smtpmail-default-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-server "smtp.gmail.com")
     (smtpmail-smtp-service 587)
     )
     ;; Include any other accounts here ...
    ))

(defun my-mu4e-set-account ()
  "Set the account for composing a message.
   This function is taken from:
     https://www.djcbsoftware.nl/code/mu/mu4e/Multiple-accounts.html"
  (let* ((account
    (if mu4e-compose-parent-message
        (let ((maildir (mu4e-message-field mu4e-compose-parent-message :maildir)))
    (string-match "/\\(.*?\\)/" maildir)
    (match-string 1 maildir))
      (completing-read (format "Compose with account: (%s) "
             (mapconcat #'(lambda (var) (car var))
            my-mu4e-account-alist "/"))
           (mapcar #'(lambda (var) (car var)) my-mu4e-account-alist)
           nil t nil nil (caar my-mu4e-account-alist))))
   (account-vars (cdr (assoc account my-mu4e-account-alist))))
    (if account-vars
  (mapc #'(lambda (var)
      (set (car var) (cadr var)))
        account-vars)
      (error "No email account found"))))
(add-hook 'mu4e-compose-pre-hook 'my-mu4e-set-account)


;;----------------- Email setup ends --------------------

;;----------------- Python IDE setup --------------------

(prelude-require-packages '(elpy py-autopep8 epc jedi))
(elpy-enable)

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(setq elpy-rpc-python-command "~/.pyenv/versions/3.5.5/bin/python3.5")

;; Global Jedi config vars

(defvar jedi-config:use-system-python nil
  "Will use system python and active environment for Jedi server.
May be necessary for some GUI environments (e.g., Mac OS X)")

(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

(defvar jedi-config:vcs-root-sentinel ".git")

(defvar jedi-config:python-module-sentinel "__init__.py")

;; Helper functions

;; Small helper to scrape text from shell output
(defun get-shell-output (cmd)
  (replace-regexp-in-string "[ \t\n]*$" "" (shell-command-to-string cmd)))

;; Ensure that PATH is taken from shell
;; Necessary on some environments without virtualenv
;; Taken from: http://stackoverflow.com/questions/8606954/path-and-exec-path-set-but-emacs-does-not-find-executable

;; --- ledger mode ---
(require 'ledger-mode)

(add-to-list 'auto-mode-alist '("\\.ledger$" . ledger-mode))
;;----------------- Python setup end ------------

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

;; -------------- Multi term config ends  -------------


;; --------- pdf config -------------
(add-hook 'prog-mode-hook 'linum-on)
(setq-default doc-view-pdfdraw-program "mudraw")
;;-----------------------------------
