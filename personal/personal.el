(prelude-require-package 'multiple-cursors)

;; When you have an active region that spans multiple lines, the following will add a cursor to each line:
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; When you want to add multiple cursors not based on continuous lines, but based on keywords in the buffer, use:
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;; theme settings
(disable-theme 'zenburn)

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
