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

(global-set-key [f8] 'neotree-toggle)
(setq neo-theme 'icons)

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
			 "--print-width" "80"
			 "--tab-width" "2"
			 "--single-quote" "true"
			 "--trailing-comma" "none"
			 "--jsx-bracket-same-line" "true"
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

;;Initialize theme
(require 'color-theme-sanityinc-tomorrow)
(color-theme-sanityinc-tomorrow t)

;;projectile config
(projectile-mode)

;;paren
(show-paren-mode)

;; enable elpy
(elpy-enable)
(setq elpy-rpc-backend "jedi")

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

;;trim whitespace and final blank line
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
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#000000" "#8b0000" "#00ff00" "#ffa500" "#7b68ee" "#dc8cc3" "#93e0e3" "#dcdccc"])
 '(beacon-color "#d54e53")
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "57fe2bf84d81baecc6d89ed97bdb19936a3052fc2551ca178667fc45feef2381" "eecacf3fb8efc90e6f7478f6143fd168342bbfa261654a754c7d47761cec07c8" "d61fc0e6409f0c2a22e97162d7d151dee9e192a90fa623f8d6a071dbf49229c6" "9deeab438d1d798c26d41c759d74a2406802427ff6acb7dec8cec961bcb4e7d5" "d6922c974e8a78378eacb01414183ce32bc8dbf2de78aabcc6ad8172547cb074" "41f90b83fae6e57d37617a9998424cb78fa064fc79706442e677201084ee181d" "eea01f540a0f3bc7c755410ea146943688c4e29bea74a29568635670ab22f9bc" "f41ecd2c34a9347aeec0a187a87f9668fa8efb843b2606b6d5d92a653abe2439" default)))
 '(elpy-test-pytest-runner-command (quote ("py.test --no-print-log")))
 '(fci-rule-color "#383838")
 '(flycheck-color-mode-line-face-to-color (quote mode-line-buffer-id))
 '(frame-background-mode (quote dark))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#d54e53")
     (40 . "#e78c45")
     (60 . "#e7c547")
     (80 . "#b9ca4a")
     (100 . "#70c0b1")
     (120 . "#7aa6da")
     (140 . "#c397d8")
     (160 . "#d54e53")
     (180 . "#e78c45")
     (200 . "#e7c547")
     (220 . "#b9ca4a")
     (240 . "#70c0b1")
     (260 . "#7aa6da")
     (280 . "#c397d8")
     (300 . "#d54e53")
     (320 . "#e78c45")
     (340 . "#e7c547")
     (360 . "#b9ca4a"))))
 '(vc-annotate-very-old-color nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
