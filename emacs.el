;; Local Variables:
;; eval: (emacs-lisp-mode)
;; End:

(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "256bd513a9875cd855077162cdfee8d75b0ad7e18fe8b8cbc10412561fbef892" "1cfc3c062790a8d6f9ce677c50cf671609f45c32695778873b4a7619f1e749b5" "a6e3dec0d16222cc5747743c87ef7da79186f7282e2ec4ff74c7f08ed7fe28d2" "955426466aa729d7d32483d3b2408cf474a1332550ad364848d1dfe9eecc8a16" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/workspace/my-org-mode/my-org.org")))
 '(package-selected-packages
   (quote
    (jedi ein doom-modeline doom-themes all-the-icons-gnus all-the-icons-dired all-the-icons-ivy treemacs-icons-dired treemacs centaur-tabs use-package company-tabnine company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; deals internally with package list

(setq package-list '(doom-modeline doom-themes
				   all-the-icons-gnus
				   all-the-icons-dired
				   all-the-icons-ivy
				   treemacs-icons-dired
				   treemacs centaur-tabs use-package company-tabnine
				   company
				   helm
				   undo-tree
				   autopair
				   gnuplot-mode
				   markdown-mode
				   auctex
				   latex-preview-pane
				   yaml-mode
				   elpy
				   highlight-parentheses
				   )) 

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available 
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))



;; SOME INSTALLATION STEPS:
;; all the icons: M-x all-the-icons-install-fonts
;; M-x company-tabnine-install-binary


(require 'company)



(require 'company-tabnine)
(add-to-list 'company-backends #'company-tabnine)

;; GENERAL COMPANY CONFIG

;; Trigger completion immediately.
(setq company-idle-delay 0)

;; Number the candidates (use M-1, M-2 etc to select completions).
(setq company-show-numbers t)

;; Use the tab-and-go frontend.
;; Allows TAB to select and complete at the same time.
(company-tng-configure-default)
(setq company-frontends
      '(company-tng-frontend
        company-pseudo-tooltip-frontend
        company-echo-metadata-frontend))



;; config of centaur-tabls
(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-<prior>" . centaur-tabs-backward)
  ("C-<next>" . centaur-tabs-forward))
(centaur-tabs-headline-match)
(setq centaur-tabs-style "bar")
(setq centaur-tabs-set-icons t)
;;(setq centaur-tabs-gray-out-icons 'buffer)
(setq centaur-tabs-set-bar 'left)
(setq centaur-tabs-set-modified-marker t)



;; remove welcome screen
(setq inhibit-startup-screen t)




;; dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(require 'doom-themes)

;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
;; may have their own settings.
;; (load-theme 'doom-opera-light t)

;; (if (display-graphic-p) 
;;     (enable-theme 'solarized) 
;;   (enable-theme 'wheatgrass))

;; tweaks the theme in dependence on whether terminal or not.
(if (display-graphic-p) 
    (load-theme 'doom-opera-light t) 
  (load-theme 'tsdh-light))


;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)

;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)
;; or for treemacs users
(doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


;; doom-modeline
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(require 'helm-config)


;; disables the traditional toolbar
(tool-bar-mode -1) 


;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; some default modes
(treemacs)
;; (ido-mode)
(helm-mode 1)
;;(helm-autoresize-mode 1)
(put 'downcase-region 'disabled nil)
(setq helm-display-function #'helm-display-buffer-in-own-frame)
(setq helm-display-buffer-height 20)
(setq helm-actions-inherit-frame-settings t)


;; winner
(winner-mode)

;; highlight-parentheses
(require 'highlight-parentheses)
(global-highlight-parentheses-mode 1)


;; tramp
(setq tramp-default-method "ssh")


;; autocloses the compilation window after successful compilation
(setq compilation-finish-functions 'compile-autoclose)
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
	 (bury-buffer "*compilation*")
	 (winner-undo)
	 (message "Build successful."))
	(t
	 (message "Compilation exited abnormally: %s" string))))


;; undo tree
(global-undo-tree-mode t)
(setq undo-tree-visualizer-relative-timestamps t)
(setq undo-tree-visualizer-timestamps t)
;; (use-package undo-tree
;;           :defer t
;;           :ensure t
;;           :diminish undo-tree-mode
;;           :idle
;;           (progn
;;             (global-undo-tree-mode)
;;             (setq undo-tree-visualizer-timestamps t)
;;                 (setq undo-tree-visualizer-diff t)))   


;; CEDET
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: Tou must place this *before* any CEDET component
;; gets activated by another package (Gnus, auth-source, ...).
(load-file "/home/acorbe/workspace/cedet/cedet-devel-load.el")

;; Add further minor-modes to be enabled by semantic-mode.
;; See doc-string of `semantic-default-submodes' for other things
;; you can use here.
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
;; (setq semantic-complete-inline-analyzer-displayor-class
;;       'semantic-displayor-tooltip)

;; Enable Semantic
(semantic-mode 1)
;; Enable EDE (Project Management) features - This adds a menu named: "Developement"
;; (global-ede-mode 1)
;;
(if (fboundp 'semantic-load-enable-code-helpers)
    ;; checks whether semantic-load-enable-code-helpers from CEDET exists, if yes it loads it
    (progn
      (message "cedet:semantic-load-enable-code-helpers found. enabling")
      (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
     )
  (message "cedet:semantic-load-enable-code-helpers NOT FOUND.")
  )

(if (boundp 'global-srecode-minor-mode)
    ;; checks whether global-srecode-minor-mode from CEDET exists
    (progn
      (message "cedet:global-srecode-minor-mode found. enabling")
      (global-srecode-minor-mode 1)            ; Enable template insertion menu     
      )
  (message "cedet:global-srecode-minor-mode NOT FOUND.")
  )



(require 'company)

(defun custom-company-cpp-mode ()
  (company-mode 1)
  (company-semantic 1)
  )

(add-hook 'c++-mode-hook #'custom-company-cpp-mode)


;; autopair
(require 'autopair)
  (autopair-global-mode) ;; enable autopair in all buffers

;; elpy
(elpy-enable)

;; compile hacks -- makefile in current dir or upstaris. To be improved
(defun get-above-makefile ()
  (let ((dir (locate-dominating-file "." "Makefile")))
    (when dir
      (concat dir "Makefile"))))


;; auctex
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; shell -- opens shell in the same buffer
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)


;; keys customization

;; customizing occur
(global-set-key (kbd "C-o") 'occur)

;; compile hotkey
;; (global-set-key [(C-f5)] 'compile)
;; (global-set-key [(f5)] 'recompile)
;; bind compiling with get-above-makefile to f5
(global-set-key [f5] (lambda () (interactive) (compile (format
	   "make -f %s" (get-above-makefile)))))

;; winner
(global-set-key (kbd "<f7>") 'winner-undo)
(global-set-key (kbd "C-<f7>") 'winner-redo)

;; helm
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
;; (global-set-key (kbd "C-x C-f") #'helm-find-files)








