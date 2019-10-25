
;; Local Variables:
;; eval: (emacs-lisp-mode)
;; End:
;;; Code:
(defconst *start-time* (current-time)) ;; record start time to time .emacs load time

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
;; (package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("9c27124b3a653d43b3ffa088cd092c34f3f82296cf0d5d4f719c0c0817e1afa6" "e3c87e869f94af65d358aa279945a3daf46f8185f1a5756ca1c90759024593dd" "a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "256bd513a9875cd855077162cdfee8d75b0ad7e18fe8b8cbc10412561fbef892" "1cfc3c062790a8d6f9ce677c50cf671609f45c32695778873b4a7619f1e749b5" "a6e3dec0d16222cc5747743c87ef7da79186f7282e2ec4ff74c7f08ed7fe28d2" "955426466aa729d7d32483d3b2408cf474a1332550ad364848d1dfe9eecc8a16" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/workspace/my-org-mode/my-org.org")))
 '(package-selected-packages
   (quote
    (tree-mode json-navigator ejson-mode gnuplot-mode cmake-font-lock cmake-mode auctex elpy yaml-mode undo-tree highlight-parentheses magit counsel ivy-rich cdlatex say-what-im-doing latex-extra gitlab-ci-mode-flycheck gitlab-ci-mode encourage-mode wc-mode langtool wttrin ivy-posframe ivy-postframe poly-markdown flycheck zenburn esup dired-rainbow shell-pop rainbow-delimiters rainbow-mode ag howdoi yasnippet-snippets pdf-tools gscholar-bibtex jedi ein doom-modeline doom-themes all-the-icons-gnus all-the-icons-dired all-the-icons-ivy treemacs-icons-dired treemacs centaur-tabs use-package company-tabnine company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; deals internally with package list

(setq package-list '(
		     ;; doom-modeline - moved below
		     ;; doom-themes
		     use-package
		     ;; all-the-icons-gnus
		     ;; all-the-icons-dired ;; moved below
		     ;; all-the-icons-ivy ;; moved below
		     ;; treemacs-icons-dired
		     ;; treemacs
		     ;; centaur-tabs
		     ;; company-tabnine ;; removed
		     ;; company ;; moved below
		     ;; helm ;; moved below
		     ;; undo-tree ;; moved below
		     ;; autopair ;; removed
		     ;; gnuplot-mode ;; moved below
		     ;; markdown-mode ;; moved below
		     ;; auctex ;; moved below
		     ;; latex-preview-pane ;; disabled below
		     ;; yaml-mode ;; moved below
		     ;; elpy ;; moved below
		     ;; highlight-parentheses ;; moved below
		     ;; magit ;; moved below
		     ;; company-box
		     ;; ein
		     ;; framemove
		     ;; gscholar-bibtex ;; set below
		     ;; zenburn-theme ;; moved below
		     ;; cmake-mode cmake-font-lock
		     ;; swiper ;; moved below
		     ;; ivy
		     ;; counsel
		     ))
(defconst *start-time-after-half-block* (current-time)) 

; activate all the packages (in particular autoloads)
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))


(defconst *start-time-after-first-block* (current-time)) 

;; SOME INSTALLATION STEPS:
;; all the icons: M-x all-the-icons-install-fonts
;; M-x company-tabnine-install-binary


;; Don’t compact font caches during GC.
(setq inhibit-compacting-font-caches t)

(require 'cl)



(use-package company
  :ensure t
  :hook ((prog-mode . company-mode)
	 (latex-mode . company-mode))
  :config
  (progn
    (add-to-list 'company-backends #'ein:company-backend)
    (setq ein:completion-backend #'ein:use-company-backend)

    ;; adding company mode to emacs lisp
    (add-hook 'emacs-lisp-mode-hook #'company-mode)
    
    ;; GENERAL COMPANY CONFIG

    ;; Trigger completion immediately.
    (setq company-idle-delay 0.3)

    ;; Number the candidates (use M-1, M-2 etc to select completions).
    (setq company-show-numbers 1)

    ;; (require 'company-box)
    ;; (add-hook 'company-mode-hook 'company-box-mode)
    ;; Use the tab-and-go frontend.
    ;; Allows TAB to select and complete at the same time.
    (company-tng-configure-default)
    (setq company-frontends
	  '(company-tng-frontend
	    company-pseudo-tooltip-frontend
	    company-echo-metadata-frontend))

    )
  )
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)


(unless (version< emacs-version "25.2")
  ;; only from emacs 25.2 onwards
  (use-package treemacs
    :ensure t
    :bind
    (:map global-map
	  ([f8] . treemacs)
	  )
    )
  (use-package treemacs-icons-dired
    :after treemacs dired
    :ensure t
    
    ;; :config (treemacs-icons-dired-mode) ;; this conflicts with normal icons
    )
)
  
(use-package all-the-icons-gnus
  :disabled t
  :ensure t)

;; remove welcome screen
(setq inhibit-startup-screen t)

(unless (version< emacs-version "25.3")
  ;;only for emacs 25.3 and older
  (use-package ein
    :ensure t
    :commands (ein:notebooklist-open))
  )


;; dired
(use-package all-the-icons-dired
  :ensure t  
  ;; :defer t
  :hook (dired-mode . all-the-icons-dired-mode)
  ;; :config
  ;; (progn
  ;;   (add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
  ;;   )
  )


;; ivy icons -- don't like the spacing, so disabled.
;; (use-package all-the-icons-ivy
;;   :ensure t
;;   ;; :defer t
;;   :disabled t
;;   :config
;;   (all-the-icons-ivy-setup))

(defun ivy-rich-switch-buffer-icon (candidate)
     (with-current-buffer
   	  (get-buffer candidate)
	(let ((icon (all-the-icons-icon-for-mode major-mode)))
	  (if (symbolp icon)
	      (all-the-icons-icon-for-mode 'fundamental-mode)
	    icon))))


(unless (version< emacs-version "25.1")
  (use-package ivy-rich
    :ensure t
    :after (ivy counsel)
    :init
    (progn 
      (setq ivy-rich-path-style 'abbrev
	    ivy-virtual-abbreviate 'full)
      (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line)
      (setq ivy-rich--display-transformers-list
	    '(ivy-switch-buffer
	      (:columns
	       ((ivy-rich-switch-buffer-icon :width 2)
		(ivy-rich-candidate (:width 30))
		(ivy-rich-switch-buffer-size (:width 7))
		(ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
		(ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
		(ivy-rich-switch-buffer-project (:width 15 :face success))
		(ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
	       :predicate
	       (lambda (cand) (get-buffer cand)))))
      )
    :config (ivy-rich-mode 1))
)

;; (if (display-graphic-p) 
;;     (enable-theme 'solarized) 
;;   (enable-theme 'wheatgrass))



(defun my-behavior-enable-doom-theme ()
  (use-package doom-themes
    :ensure t
    :defer
    :init
    (progn 
      ;; Global settings (defaults)
      (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	    doom-themes-enable-italic t) ; if nil, italics is universally disabled

      
      ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
      ;; may have their own settings.
      ;; (load-theme 'doom-opera-light t)  
      ;; (load-theme 'doom-opera-light t)
      (load-theme 'doom-one t)
      ;; Enable flashing mode-line on errors
      (doom-themes-visual-bell-config)
      ;; Enable custom neotree theme (all-the-icons must be installed!)
      ;; (doom-themes-neotree-config)
      (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
      ;; or for treemacs users
      (doom-themes-treemacs-config)
      ;; Corrects (and improves) org-mode's native fontification.
      (doom-themes-org-config)
      )
    )
  )


(defun my-behavior-enable-centaur-tabs ()
  ;; config of centaur-tabls
  (use-package centaur-tabs
    :ensure t
    ;; :demand
    :bind (
	   ;; (global-set-key (kbd "S-<f8>") 'centaur-tabs-mode)
	   ("C-<prior>" . centaur-tabs-backward)
	   ("C-<next>" . centaur-tabs-forward)
	   ("S-<f8>" . centaur-tabs-mode)
	   )

    :hook (after-init . centaur-tabs-mode)
    :config
    (progn   
      (centaur-tabs-mode t)
      (centaur-tabs-headline-match)
      (setq centaur-tabs-style "bar")
      (setq centaur-tabs-set-icons t)
      ;;(setq centaur-tabs-gray-out-icons 'buffer)
      (setq centaur-tabs-set-bar 'left)
      (setq centaur-tabs-set-modified-marker t)
      )  
    )
  )

;; behavior with or without GUI (display-graphic-p)
(defun my-behavior-with-graphic ()
  ;; (my-behavior-enable-centaur-tabs)  
  (my-behavior-enable-doom-theme)
  ;;(treemacs)
  )

(defun my-behavior-without-graphic ()
  (use-package zenburn-theme
    :ensure t
    :config
    (progn
      (load-theme 'zenburn)
      )
    )  
  )


;; tweaks the theme in dependence on whether terminal or not.
(if (display-graphic-p) 
    (my-behavior-with-graphic)
  (my-behavior-without-graphic)
  )


(use-package ivy
  :ensure t)
(use-package counsel
  :ensure t)

(use-package swiper
  :ensure t
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq enable-recursive-minibuffers t)
    ;; enable this if you want `swiper' to use it
    ;; (setq search-default-mode #'char-fold-to-regexp)
    (global-set-key "\C-s" 'swiper)
    (global-set-key (kbd "C-c C-r") 'ivy-resume)
    (global-set-key (kbd "<f6>") 'ivy-resume)
    (global-set-key (kbd "M-x") 'counsel-M-x)
    (global-set-key (kbd "C-x C-f") 'counsel-find-file)
    (global-set-key (kbd "<f1> f") 'counsel-describe-function)
    (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
    (global-set-key (kbd "<f1> l") 'counsel-find-library)
    (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
    (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
    ;; (global-set-key (kbd "C-c g") 'counsel-git)
    ;; (global-set-key (kbd "C-c j") 'counsel-git-grep)
    (global-set-key (kbd "C-c k") 'counsel-ag)
    (global-set-key (kbd "C-x l") 'counsel-locate)
    ;; (global-set-key (kbd "C-S-o") 'counsel-rhythmbox)
    (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history)
    ))

;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-center)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-window-bottom-left)))
;; (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))


(unless (version< emacs-version "26.0")  
   (use-package ivy-posframe
     :ensure t
     :config
     (progn
       ;; Different command can use different display function.
       (setq ivy-posframe-display-functions-alist
	     '((swiper          . nil)
	       (complete-symbol . ivy-posframe-display-at-point)
	       (counsel-M-x     . ivy-posframe-display-at-point)
	       (t               . nil)))
       (ivy-posframe-mode 1)    
       )
     )
   )
  



;; avy
(use-package avy
  :ensure t
  :config
  (progn
    (global-set-key (kbd "C-\"") 'avy-goto-char-2)
    (setq avy-all-windows 'all-frames)
    ) ;;-timer  
  )		       

;;ag
(use-package ag  
  :ensure t)

;; magit
(unless (version< emacs-version "25.1")
  (use-package magit
    :ensure t
    :bind (("C-x g" . magit-status)))
  )

(unless (version< emacs-version "25.1")
  ;; doom-modeline
  (use-package doom-modeline
    :ensure t
    ;; :disabled
    :hook (after-init . doom-modeline-mode)
    ;;:defer 2
    :config
    (progn
      (setq doom-modeline-height 25)
      ;; Whether display icons in mode-line or not.
      (setq doom-modeline-icon (display-graphic-p))
      ;; Whether display the icon for major mode. It respects `doom-modeline-icon'.
      (setq doom-modeline-major-mode-icon t)
      ;; If non-nil, a word count will be added to the selection-info modeline segment.
      (setq doom-modeline-enable-word-count t)
      ))
)

;; disables the traditional toolbar
(tool-bar-mode -1) 

;; howdoi
(use-package howdoi
  :ensure t)


;; windmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; did not manage to get it to work
;; (setq framemove-hook-into-windmove t)

;; some default modes

;; (ido-mode 1)
;; (require 'helm-config)
(use-package helm
  :ensure t
  :disabled
  :defer t
  :config
  (progn
    (helm-mode 0)
    ;; (helm-autoresize-mode 1)
    ;; (setq helm-display-function #'helm-display-buffer-popup-frame)
    ;; (setq helm-display-function #'helm-display-buffer-in-own-frame)

    ;; helm fuzzy matching for M-x
    (setq helm-M-x-fuzzy-match t)

    ;; in own frame best settings
    (setq helm-display-function 'helm-display-buffer-in-own-frame
	  helm-display-buffer-reuse-frame t
	  helm-use-undecorated-frame-option t)

    ;; custom version attempt
    ;; (setq helm-display-function
    ;;       (lambda (buf)
    ;;         (split-window-horizontally)
    ;;         (other-window 1)
    ;;         (switch-to-buffer buf)))
    
    (setq helm-display-buffer-height 15)
    (setq helm-actions-inherit-frame-settings t)
    ;; helm
    (global-set-key (kbd "M-x") #'helm-M-x)
    (global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
    )
  )

(put 'downcase-region 'disabled nil)



;; winner
(winner-mode 1)

;; highlight-parentheses
(use-package highlight-parentheses
  :ensure t
  :config
  (global-highlight-parentheses-mode 1)
  )


;; tramp
(setq tramp-default-method "ssh")


;; autocloses the compilation window after successful compilation
(defun compile-autoclose (buffer string)
  (cond ((string-match "finished" string)
	 (bury-buffer "*compilation*")
	 (winner-undo)
	 (message "Build successful."))
	(t
	 (message "Compilation exited abnormally: %s" string))))
(setq compilation-finish-functions 'compile-autoclose)



;; undo tree

(use-package undo-tree
  :ensure t
  :config
  (progn
    (global-undo-tree-mode t)
    (setq undo-tree-visualizer-relative-timestamps t)
    (setq undo-tree-visualizer-timestamps t)
   )
  )

(use-package yaml-mode
  :ensure t
  :defer t
  :mode (
	 "\\.yaml\\'"
	 "\\.yml\\'"
	 )
  )

(use-package markdown-mode
  :ensure t
  :defer t
  :mode ("\\.md\\'" "\\.MD\\'")
  )

(use-package json-mode
  :ensure t
  :mode (("\\.json\\'" . json-mode)
         ("\\.tmpl\\'" . json-mode)
         ("\\.eslintrc\\'" . json-mode))
  :config (setq-default js-indent-level 4))


(unless (version< emacs-version "25.1")
  (use-package json-navigator
    :ensure t
    )

  (use-package tree-mode
    :ensure t
    )
)

(use-package toml-mode
  :ensure t
  :mode ("\\.toml\\'" . toml-mode))

(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile\\'" . dockerfile-mode))

;; CEDET
;; Load CEDET.
;; See cedet/common/cedet.info for configuration details.
;; IMPORTANT: Tou must place this *before* any CEDET component
;; gets activated by another package (Gnus, auth-source, ...).
;; (setq CEDET_PATH_cedet_dev_load_el '"/home/acorbe/workspace/cedet/cedet-devel-load.el")

;; (setq CEDET_PATH_cedet_dev_load_el '"/home/acorbe/ce.el")
;; ;;   "the path of the cedet loadable .el file. Change if needed."
;; (message (concat "your CEDET path is" CEDET_PATH_cedet_dev_load_el))

;; (if (eq 0 1) ;; (file-exists-p CEDET_PATH_cedet_dev_load_el)
;;     (progn
;;       (message "your CEDET path exists. Loading...")
;;       (load-file CEDET_PATH_cedet_dev_load_el)

;;       ;; Add further minor-modes to be enabled by semantic-mode.
;;       ;; See doc-string of `semantic-default-submodes' for other things
;;       ;; you can use here.
;;       ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
;;       ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
;;       ;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
;;       ;; (setq semantic-complete-inline-analyzer-displayor-class
;;       ;;       'semantic-displayor-tooltip)

;;       (if (fboundp 'semantic-load-enable-code-helpers)
;; 	  ;; checks whether semantic-load-enable-code-helpers from CEDET exists, if yes it loads it
;; 	  (progn
;; 	    (message "cedet:semantic-load-enable-code-helpers found. enabling")
;; 	    (semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
;; 	    )
;; 	(message "cedet:semantic-load-enable-code-helpers NOT FOUND.")
;; 	)
      
;;       (if (boundp 'global-srecode-minor-mode)
;; 	  ;; checks whether global-srecode-minor-mode from CEDET exists
;; 	  (progn
;; 	    (message "cedet:global-srecode-minor-mode found. enabling")
;; 	    (global-srecode-minor-mode 1)            ; Enable template insertion menu     
;; 	    )
;; 	(message "cedet:global-srecode-minor-mode NOT FOUND.")
;; 	)
;;       )
;;   (progn 
;;    (message "your CEDET path does not exist. Download it from: http://cedet.sourceforge.net/setup.shtml and make it.")
;;    )
;;   )

;; Enable EDE (Project Management) features - This adds a menu named: "Developement"
;; (global-ede-mode 1)
;;

(defun my-behavior-custom-company-cpp-mode ()
  ;; (company-mode 1) ;; should be already enabled by prog-mode
  ;; Enable Semantic
  (semantic-mode 1)
  ;; (company-semantic 1)
  )

(add-hook 'c++-mode-hook #'my-behavior-custom-company-cpp-mode)
(add-hook 'c-mode-hook #'my-behavior-custom-company-cpp-mode)






;; autopair
;; (require 'autopair)
;;   (autopair-global-mode) ;; enable autopair in all buffers

;; going for electric-pair
(electric-pair-mode 1)

;; elpy
;; (use-package elpy
;;   :ensure t
;;   :init
;;   (elpy-enable))

;; trying deferred elpy
;; https://emacs.stackexchange.com/a/50757/8641
(use-package elpy
  :ensure t
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(use-package yasnippet-snippets         ; Collection of snippets
  :defer 5
  :ensure t)

;; anyway loaded by elpy.
(use-package yasnippet
  :ensure t
  :defer 3
  :config
  (progn
    (yas-global-mode 1)
    ;; (with-eval-after-load 'yasnippet
    ;;   (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))
    ))


(defun hydra-move-splitter-left (arg)
  "Move window splitter left."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (shrink-window-horizontally arg)
    (enlarge-window-horizontally arg)))

(defun hydra-move-splitter-right (arg)
  "Move window splitter right."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'right))
      (enlarge-window-horizontally arg)
    (shrink-window-horizontally arg)))

(defun hydra-move-splitter-up (arg)
  "Move window splitter up."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (enlarge-window arg)
    (shrink-window arg)))

(defun hydra-move-splitter-down (arg)
  "Move window splitter down."
  (interactive "p")
  (if (let ((windmove-wrap-around))
        (windmove-find-other-window 'up))
      (shrink-window arg)
    (enlarge-window arg)))

(use-package hydra  
  :ensure t
  :config
  (defhydra hydra-zoom (global-map "<f2>")
    "zoom"
    ("g" text-scale-increase "in")
    ("l" text-scale-decrease "out"))
  (defhydra hydra-splitter (global-map "C-M-s")
    "splitter"
    ("h" hydra-move-splitter-left)
    ("j" hydra-move-splitter-down)
    ("k" hydra-move-splitter-up)
    ("l" hydra-move-splitter-right))
  (defhydra hydra-buffer-menu (:color pink
                             :hint nil)
  "
^Mark^             ^Unmark^           ^Actions^          ^Search
^^^^^^^^-----------------------------------------------------------------
_m_: mark          _u_: unmark        _x_: execute       _R_: re-isearch
_s_: save          _U_: unmark up     _b_: bury          _I_: isearch
_d_: delete        ^ ^                _g_: refresh       _O_: multi-occur
_D_: delete up     ^ ^                _T_: files only: % -28`Buffer-menu-files-only
_~_: modified
"
  ("m" Buffer-menu-mark)
  ("u" Buffer-menu-unmark)
  ("U" Buffer-menu-backup-unmark)
  ("d" Buffer-menu-delete)
  ("D" Buffer-menu-delete-backwards)
  ("s" Buffer-menu-save)
  ("~" Buffer-menu-not-modified)
  ("x" Buffer-menu-execute)
  ("b" Buffer-menu-bury)
  ("g" revert-buffer)
  ("T" Buffer-menu-toggle-files-only)
  ("O" Buffer-menu-multi-occur :color blue)
  ("I" Buffer-menu-isearch-buffers :color blue)
  ("R" Buffer-menu-isearch-buffers-regexp :color blue)
  ("c" nil "cancel")
  ("v" Buffer-menu-select "select" :color blue)
  ("o" Buffer-menu-other-window "other-window" :color blue)
  ("q" quit-window "quit" :color blue))

  (define-key Buffer-menu-mode-map "." 'hydra-buffer-menu/body)
  )

;; (use-package rainbow-mode
;;   :ensure t)

;; removes python native completion warnings
;; https://emacs.stackexchange.com/questions/30082/your-python-shell-interpreter-doesn-t-seem-to-support-readline
(with-eval-after-load 'python
  (defun python-shell-completion-native-try ()
    "Return non-nil if can trigger native completion."
    (let ((python-shell-completion-native-enable t)
          (python-shell-completion-native-output-timeout
           python-shell-completion-native-try-output-timeout))
      (python-shell-completion-native-get-completions
       (get-buffer-process (current-buffer))
       nil "_"))))


;; this compromises the correct working!!
;; (when (executable-find "ipython")
;;   (setq python-shell-interpreter "ipython"))


;; (defun toggle-boolean ()
;;   "Toggle any booleans found on the current line."
;;   (interactive)
;;   (let ((booleans (ht ("true" "false")
;;                       ("false" "true")
;;                       ("True" "False")
;;                       ("False" "True"))))
;;     (save-excursion
;;       (save-restriction
;;         (call-interactively 'select-current-line)
;;         (call-interactively 'narrow-to-region)
;;         (setq toggle-boolean-re (-reduce (lambda (memo item) (format "%s\\|%s" memo item)) (ht-keys booleans)))
;;         (goto-char (point-min))
;;         (re-search-forward toggle-boolean-re nil t))
;;       (let* ((thing2 (thing-at-point 'word))
;;              (bounds (bounds-of-thing-at-point 'word))
;;              (pos1 (car bounds))
;;              (pos2 (cdr bounds)))
;;         (setq replacement (ht-get booleans thing2 nil))
;;         (when replacement
;;           (delete-region pos1 pos2)
;;           (insert replacement))))))


;; (global-set-key (kbd "C-c !") 'toggle-boolean)


;; compile hacks -- makefile in current dir or upstaris. To be improved
(defun get-above-makefile ()
  (let ((dir (locate-dominating-file "." "Makefile")))
    (when dir
      (concat dir "Makefile"))))


;; auctex
(use-package tex-mode
  :ensure auctex
  ;; :defer t
  :init
  (progn
    (setq TeX-auto-save t)
    (setq TeX-parse-self t)
    (setq-default TeX-master nil)

    (add-hook 'LaTeX-mode-hook 'visual-line-mode)
    (add-hook 'LaTeX-mode-hook 'flyspell-mode)
    (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

    (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)
    )
    ;; :mode (
    ;; 	   "\\.tex\\'"
    ;; 	   "\\.TEX\\'"
    ;; 	   "\\.bib\\'"	 
    ;; 	 )
    )
(use-package gscholar-bibtex
  :ensure t
  :hook latex-mode 
  :config  
  (progn
    (setq gscholar-bibtex-default-source "Google Scholar")
    )
  )

(use-package latex-extra
  :ensure t
  :hook (LaTeX-mode . latex-extra-mode))

(use-package langtool
  :ensure t
  :hook latex-mode
  :init  
  (setq langtool-language-tool-jar "/home/acorbe/Downloads/LanguageTool-4.7/languagetool-commandline.jar")  
  (setq langtool-default-language "en-US")
  )

(use-package wc-mode
  :ensure t)


(use-package latex-preview-pane
  :ensure t
  :disabled t)

(use-package cmake-mode
  :ensure t
  :mode "CMakeLists.txt")

(use-package cmake-font-lock
  :ensure t
  :hook cmake-mode
  :init
  (progn
    (autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
    (add-hook 'cmake-mode-hook 'cmake-font-lock-activate)
    )
  )

;;cmake





;; aim -- pdf tools installs itself, including pdf-tools-install, if needed. Only on linux.
(use-package pdf-tools  
  :if (memq window-system '(x))
  :ensure t
  :magic ("%PDF" . pdf-view-mode)
  :defer 
  :config
  (progn
    (pdf-tools-install :no-query) ;; :no-query
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    )
  ;; :mode (
  ;; 	 "\\.PDF\\'"
  ;; 	 "\\.pdf\\'"
  ;; 	 )
  )





;; shell -- opens shell in the same buffer
(push (cons "\\*shell\\*" display-buffer--same-window-action) display-buffer-alist)

;; doc-view comes with auto-revert-mode
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; increases doc-view-resolution to match higher screen resolution
(setq doc-view-resolution 192)


;; when opening a file prevents double window - ignore errors should no file be opened and so no further window to close
(defun delete-other-window-if-one-buffer-open ()
  ;; deletes other window (supposedly buffer/messages) if a window with content is opened
  ;; check the length of window list, comapares it with a target, based on that decides whether to kill or not.
  (setq target_no_win_for_killing 2)
  (if (fboundp 'treemacs-current-visibility)
      (if
	  (eq (treemacs-current-visibility) 'visible)
	  (progn
	    ;; (message "treemacs is visible")
	    (incf target_no_win_for_killing)	
	    )
	)
    )
  (if (eq
       (length (window-list))
       target_no_win_for_killing
       )
      (delete-other-windows)
    )
  )
;; (delete-other-window-if-one-buffer-open)

(add-hook 'window-setup-hook #'delete-other-window-if-one-buffer-open)

(use-package rainbow-delimiters
  :ensure t  
  ;; :defer t
  :init
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    ))

(use-package shell-pop
  :ensure t
  :bind (("<C-M-return>" . shell-pop))
  ;; :config
  ;; (progn
  ;;   (global-set-key (kbd "<C-M-return>") 'shell-pop)
  ;;   )
  )

(use-package gnuplot-mode
  ;; :defer t
  :ensure t
  :mode ("\\.gnu\\'")
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))
    ))

(use-package dired-rainbow
  :ensure t
  :disabled t
  :config
  (progn
    (dired-rainbow-define-chmod directory "#6cb2eb" "d.*")
    (dired-rainbow-define html "#eb5286" ("css" "less" "sass" "scss" "htm" "html" "jhtm" "mht" "eml" "mustache" "xhtml"))
    (dired-rainbow-define xml "#f2d024" ("xml" "xsd" "xsl" "xslt" "wsdl" "bib" "json" "msg" "pgn" "rss" "yaml" "yml" "rdata"))
    (dired-rainbow-define document "#9561e2" ("docm" "doc" "docx" "odb" "odt" "pdb" "pdf" "ps" "rtf" "djvu" "epub" "odp" "ppt" "pptx"))
    (dired-rainbow-define markdown "#ffed4a" ("org" "etx" "info" "markdown" "md" "mkd" "nfo" "pod" "rst" "tex" "textfile" "txt"))
    (dired-rainbow-define database "#6574cd" ("xlsx" "xls" "csv" "accdb" "db" "mdb" "sqlite" "nc"))
    (dired-rainbow-define media "#de751f" ("mp3" "mp4" "MP3" "MP4" "avi" "mpeg" "mpg" "flv" "ogg" "mov" "mid" "midi" "wav" "aiff" "flac"))
    (dired-rainbow-define image "#f66d9b" ("tiff" "tif" "cdr" "gif" "ico" "jpeg" "jpg" "png" "psd" "eps" "svg"))
    (dired-rainbow-define log "#c17d11" ("log"))
    (dired-rainbow-define shell "#f6993f" ("awk" "bash" "bat" "sed" "sh" "zsh" "vim"))
    (dired-rainbow-define interpreted "#38c172" ("py" "ipynb" "rb" "pl" "t" "msql" "mysql" "pgsql" "sql" "r" "clj" "cljs" "scala" "js"))
    (dired-rainbow-define compiled "#4dc0b5" ("asm" "cl" "lisp" "el" "c" "h" "c++" "h++" "hpp" "hxx" "m" "cc" "cs" "cp" "cpp" "go" "f" "for" "ftn" "f90" "f95" "f03" "f08" "s" "rs" "hi" "hs" "pyc" ".java"))
    (dired-rainbow-define executable "#8cc4ff" ("exe" "msi"))
    (dired-rainbow-define compressed "#51d88a" ("7z" "zip" "bz2" "tgz" "txz" "gz" "xz" "z" "Z" "jar" "war" "ear" "rar" "sar" "xpi" "apk" "xz" "tar"))
    (dired-rainbow-define packaged "#faad63" ("deb" "rpm" "apk" "jad" "jar" "cab" "pak" "pk3" "vdf" "vpk" "bsp"))
    (dired-rainbow-define encrypted "#ffed4a" ("gpg" "pgp" "asc" "bfe" "enc" "signature" "sig" "p12" "pem"))
    (dired-rainbow-define fonts "#6cb2eb" ("afm" "fon" "fnt" "pfb" "pfm" "ttf" "otf"))
    (dired-rainbow-define partition "#e3342f" ("dmg" "iso" "bin" "nrg" "qcow" "toast" "vcd" "vmdk" "bak"))
    (dired-rainbow-define vc "#0074d9" ("git" "gitignore" "gitattributes" "gitmodules"))
    (dired-rainbow-define-chmod executable-unix "#38c172" "-.*x.*")
    )) 

(use-package esup
  :ensure t)

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

(use-package flycheck
  :ensure t
  :hook ('prog-mode . flycheck-mode)
  ;; :init
  ;; (add-hook 'prog-mode-hook 'flycheck-mode)
  )

;; (use-package flycheck-inline
;;   :demand t
;;   :require flycheck 
;;   :init
;;   (add-hook 'flycheck-mode-hook #'turn-on-flycheck-inline))


;; (use-package polymode
;;   :defer
;;   :ensure t)

;; (use-package poly-markdown
;;   :defer
;;   :ensure t)

(use-package wttrin
  :ensure t
  :defer t)

(use-package gitlab-ci-mode
  :ensure t
  :defer t
  :mode
  ("\\.gitlab-ci.yaml\\'"
   "\\.gitlab-ci.yml\\'")
  )

(use-package gitlab-ci-mode-flycheck
  :after flycheck gitlab-ci-mode
  :init
  (gitlab-ci-mode-flycheck-enable))

;; https://emacs.stackexchange.com/a/21154/8641
(defun my-switch-to-buffer (buffer)
  "Display BUFFER in the selected window.
If BUFFER is displayed in an existing window, select that window instead."
  (interactive
   (list (get-buffer (read-buffer
                      "Switch to buffer: "
                      (other-buffer (current-buffer)
				    )
		      ))))
  (if-let ((win (get-buffer-window buffer)))
      (select-window win)
    (switch-to-buffer buffer)
    ))

(defun my-open-shell-if-closed-else-my-switch-there ()
  "Tries to move to an open shell or moves there"
  (interactive)
   (if (get-buffer "*shell*")
       (my-switch-to-buffer "*shell*")
     (shell)
     )   
  )




;; (add-hook 'window-setup-hook #'delete-other-windows)

;; keys customization

;; customizing occur
(global-set-key (kbd "C-o") 'occur)

;; compile hotkey
(global-set-key [(C-f5)] 'compile)
(global-set-key [(f5)] 'recompile)
;; bind compiling with get-above-makefile to f5
;; (global-set-key [f5] (lambda () (interactive) (compile (format
;; 	   "make -f %s" (get-above-makefile)))))

;; winner
(global-set-key (kbd "<f7>") 'winner-undo)
(global-set-key (kbd "C-<f7>") 'winner-redo)

;; (global-set-key (kbd "C-x C-f") #'helm-find-files)

;; treemacs appear/disappears with F8
;; (global-set-key (kbd "<f8>") 'treemacs)
;; (global-set-key (kbd "S-<f8>") 'centaur-tabs-mode)

;; ace window shortcut
(global-set-key (kbd "M-p") 'ace-window)

;; (global-set-key (kbd "C-=") #'next-multiframe-window)
;; (global-set-key (kbd "C-M-=") #'previous-multiframe-window)
;; changes frame and back
(global-set-key (kbd "C-=") 'other-frame)
(global-set-key (kbd "C-M-=")
		( lambda () (interactive) (other-frame -1) )
		)


;; easy access to shell
;; (global-set-key (kbd "C-x t") 'shell)
(global-set-key (kbd "C-x t") 'my-open-shell-if-closed-else-my-switch-there)

;; window dimensions
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)


(defun resize-window (&optional arg)    ; Hirose Yuuji and Bob Wiener
  "*Resize window interactively."
  (interactive "p")
  (if (one-window-p) (error "Cannot resize sole window"))
  (or arg (setq arg 1))
  (let (c)
    (catch 'done
      (while t
	(message
	 "h=heighten, s=shrink, w=widen, n=narrow (by %d);  1-9=unit, q=quit"
	 arg)
	(setq c (read-char))
	(condition-case ()
	    (cond
	     ((= c ?h) (enlarge-window arg))
	     ((= c ?s) (shrink-window arg))
	     ((= c ?w) (enlarge-window-horizontally arg))
	     ((= c ?n) (shrink-window-horizontally arg))
	     ((= c ?\^G) (keyboard-quit))
	     ((= c ?q) (throw 'done t))
	     ((and (> c ?0) (<= c ?9)) (setq arg (- c ?0)))
	     (t (beep)))
	  (error (beep)))))
    (message "Done.")))


(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(message ".emacs. load: %s s; half: %s s; first  %s s."
	 (mapconcat 'int-to-string (rest (time-since *start-time*)) "." )
	 (mapconcat 'int-to-string (rest (time-since *start-time-after-half-block*)) "." )
	 (mapconcat 'int-to-string (rest (time-since *start-time-after-first-block*)) "." )
	 )
	 
