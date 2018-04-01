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
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq python-shell-interpreter "ipython" python-shell-interpreter-args "--simple-prompt -i")

(add-hook 'markdown-mode-hook (lambda()
                                (whitespace-mode -1)))

(setq org-bullets-bullet-list '("❶" "❷" "❸" "❹" "❺" "❻" "❼" "❽" "❾"
                                "➀" "➁" "➂" "➃" "➄" "➅" "➆"))
(global-visual-line-mode 1)




;; org mode settings

(defvar org-notes-dir "~/org-notes")
(defvar org-todays-todo-file-location
  (concat  org-notes-dir "/todays-task.org"))
(defvar org-notes-file-location (concat  org-notes-dir "/notes.org"))
(defvar org-journal-file-location (concat  org-notes-dir "/journal.org"))
(defvar org-blog-idea-file-location (concat  org-notes-dir "/blog-ideas.org"))
(defvar org-archive-file-location (concat  org-notes-dir "/archive.org::"))

;; overriding org-mode system settings
(setq org-default-notes-file org-notes-file-location)
(setq org-agenda-files '("~/org-notes"))
(setq org-archive-location org-archive-file-location)


(setq org-default-notes-file org-todays-todo-file-location
   initial-buffer-choice  org-default-notes-file)

(add-hook 'org-mode-hook
          (lambda()
            (org-bullets-mode 1)
            (whitespace-mode -1)
            ))


(define-key global-map "\C-cc" 'org-capture)

(setq org-capture-templates
      '(("t" "Todo" entry (file+headline org-todays-task-file-location "Tasks")
         "* TODO %?\n %i\n %a")
        ("j" "Journal" entry (file+datetree org-journal-file-location)
         "* %?\nEntered on %T")
        ("b" "Blog Idea" entry (file org-blog-idea-file-location)
         "* TODO %? \t%^G\nEntered on %T  %i")
        ("r" "Research Notes" entry (file org-notes-file-location)
         "* %? \t%^G\nEntered on %T\n %i")))
