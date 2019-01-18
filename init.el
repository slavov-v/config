(require 'package)
;;melpa-packages
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

;;neotree config
(add-to-list 'load-path "/home/v-slavov/.emacs.d/neotree")
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; disable lockfiles
(setq create-lockfiles nil)

(package-initialize)

;; Git commit message config
(add-to-list 'auto-mode-alist '("\\COMMIT_EDITMSG\\'" . log-edit-mode))

;; C++ Configurations
(defun cpp-default-hook()
  (require 'auto-complete)
  (require 'auto-complete-config)
  (ac-config-default)
  (require 'ac-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-header-symbols t)
  (semantic-mode 1)
  (add-to-list 'ac-sources 'ac-source-semantic)
  )
(add-hook 'c++-mode-hook 'cpp-default-hook)

;;setup Tern for js2
(require 'prettier-js)
(setq prettier-js-args '(
                         "--print-width 80"
                         "--tab-width 2"
                         "--single-quote"
                         "--trailing-comma none"
                         "--jsx-bracket-same-line"
                         ))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))
(add-to-list 'load-path "~/Documents/tern/emacs/")
(autoload 'tern-mode "tern.el" nil t)
(defun my-tern-js2-hook ()
  (tern-mode t)
  (require 'auto-complete)
  (auto-complete-mode)
  (eval
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
  )
(add-hook 'js2-mode-hook 'my-tern-js2-hook)
(add-hook 'js2-mode-hook 'prettier-js-mode)
(add-hook 'js2-mode-hook (lambda () (setq js2-basic-offset 2)))
(add-hook 'rjsx-mode-hook 'my-tern-js2-hook)
(add-hook 'rjsx-mode-hook 'prettier-js-mode)
(add-hook 'rjsx-mode-hook (lambda () (setq js2-basic-offset 2)))
(add-hook 'css-mode-hook 'prettier-js-mode)
(add-hook 'scss-mode-hook 'prettier-js-mode)

;;Initialize theme
(require 'color-theme-sanityinc-tomorrow)
(load-theme 'sanityinc-tomorrow-eighties t)

;;projectile config
(projectile-mode)

;;paren
(show-paren-mode)

;; enable elpy
(elpy-enable)

;;web-mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(defun my-web-mode-hook ()
  ;;web-mode sets "js" content type for all js files
  ;;so "jsx" needs to be associated explicitly
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))

  ;; (require 'react-snippets)
  (yas-global-mode 1)
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-indent-style 2)
  (setq web-mode-style-padding 1)
  ;; For <script> parts
  (setq web-mode-script-padding 1)
  ;; For multi-line blocks
  (setq web-mode-block-padding 0)
  (setq web-mode-engines-alist
        '(("django"    . "\\.html\\'"))
        )
  (setq web-mode-enable-current-element-highlight t)
  (setq web-mode-enable-current-column-highlight t)
  (setq indent-tabs-mode nil)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  )

;;yasnippets
(global-set-key (kbd "C-c C-y") 'yas-insert-snippet)

(add-hook 'web-mode-hook 'my-web-mode-hook)
;;disable toolbar
(menu-bar-mode -1)
(tool-bar-mode -1)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(global-git-gutter-mode +1)
(global-hl-line-mode 1)
(setq linum-format "%4d \u2502 ")
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;;helm config
(require 'helm-config)
(require 'helm-projectile)
(helm-projectile-on)
(global-set-key (kbd "M-x") 'helm-M-x)

;; enable autopair
(electric-pair-mode)

;; trim whitespace and final blank line
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(defun my-other-delete-trailing-blank-lines ()
  "Deletes all blank lines at the end of the file, even the last one"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (delete-blank-lines)
      (let ((trailnewlines (abs (skip-chars-backward "\n\t"))))
        (if (> trailnewlines 0)
            (progn
              (delete-char trailnewlines)))))))

;; `Rust` setup
(require 'flymake-rust)
(add-hook 'rust-mode-hook 'flymake-rust-load)
(setq flymake-rust-use-cargo 1)
(add-hook 'rust-mode-hook 'racer-mode)
(add-hook 'rust-mode-hook 'cargo-minor-mode)
(add-hook 'rust-mode-hook 'company-mode)
