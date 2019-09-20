
;; Local Variables:
;; eval: (emacs-lisp-mode)
;; End:

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
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a7051d761a713aaf5b893c90eaba27463c791cd75d7257d3a8e66b0c8c346e77" "10461a3c8ca61c52dfbbdedd974319b7f7fd720b091996481c8fb1dded6c6116" "ab9456aaeab81ba46a815c00930345ada223e1e7c7ab839659b382b52437b9ea" "cd736a63aa586be066d5a1f0e51179239fe70e16a9f18991f6f5d99732cabb32" "34c99997eaa73d64b1aaa95caca9f0d64229871c200c5254526d0062f8074693" "256bd513a9875cd855077162cdfee8d75b0ad7e18fe8b8cbc10412561fbef892" "1cfc3c062790a8d6f9ce677c50cf671609f45c32695778873b4a7619f1e749b5" "a6e3dec0d16222cc5747743c87ef7da79186f7282e2ec4ff74c7f08ed7fe28d2" "955426466aa729d7d32483d3b2408cf474a1332550ad364848d1dfe9eecc8a16" default)))
 '(inhibit-startup-screen t)
 '(org-agenda-files (quote ("~/workspace/my-org-mode/my-org.org")))
 '(package-selected-packages
   (quote
    (dired-rainbow shell-pop rainbow-delimiters rainbow-mode ag howdoi yasnippet-snippets pdf-tools gscholar-bibtex jedi ein doom-modeline doom-themes all-the-icons-gnus all-the-icons-dired all-the-icons-ivy treemacs-icons-dired treemacs centaur-tabs use-package company-tabnine company))))
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
				   magit
				   ;; company-box
				   ein
				   ;; framemove
				   gscholar-bibtex
				   zenburn-theme
				   cmake-mode cmake-font-lock
				   swiper ivy counsel				   )) 

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


(require 'cl)

(require 'company)
;; (require 'company-tabnine)
;; (add-to-list 'company-backends #'company-tabnine)
(add-to-list 'company-backends #'ein:company-backend)

(setq ein:completion-backend #'ein:use-company-backend)

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




;; remove welcome screen
(setq inhibit-startup-screen t)


;; dired
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

;; ivy icons -- don't like the spacing, so disabled.
(use-package all-the-icons-ivy
  :ensure t
  :disabled t
  :config
  (all-the-icons-ivy-setup))


;; (if (display-graphic-p) 
;;     (enable-theme 'solarized) 
;;   (enable-theme 'wheatgrass))



(defun my-behavior-enable-doom-theme ()
  (require 'doom-themes)
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled

  ;; Load the theme (doom-one, doom-molokai, etc); keep in mind that each theme
  ;; may have their own settings.
  ;; (load-theme 'doom-opera-light t)  
  (load-theme 'doom-opera-light t)
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

(defun my-behavior-enable-centaur-tabs ()
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
  
  )

;; behavior with or without GUI (display-graphic-p)
(defun my-behavior-with-graphic ()
  (my-behavior-enable-centaur-tabs)  
  (my-behavior-enable-doom-theme)
  (treemacs)
  )

(defun my-behavior-without-graphic ()
  (load-theme 'zenburn)
  )


;; tweaks the theme in dependence on whether terminal or not.
(if (display-graphic-p) 
    (my-behavior-with-graphic)
  (my-behavior-without-graphic)
  )


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
    ;; (global-set-key (kbd "M-x") 'counsel-M-x)
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

;; avy
(use-package avy
  :ensure t
  :config
  (global-set-key (kbd "M-'") 'avy-goto-char-2) ;;-timer 
  )		       

;;ag
(use-package ag
  :ensure t)

;; magit
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)))

;; doom-modeline
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      (progn
	(setq doom-modeline-height 25)))


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
  :config
  (progn
    (helm-mode 1)
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
(setq CEDET_PATH_cedet_dev_load_el '"/home/acorbe/workspace/cedet/cedet-devel-load.el")
;;   "the path of the cedet loadable .el file. Change if needed."
(message (concat "your CEDET path is" CEDET_PATH_cedet_dev_load_el))

(if (file-exists-p CEDET_PATH_cedet_dev_load_el)
    (progn
      (message "your CEDET path exists. Loading...")
      (load-file CEDET_PATH_cedet_dev_load_el)

      ;; Add further minor-modes to be enabled by semantic-mode.
      ;; See doc-string of `semantic-default-submodes' for other things
      ;; you can use here.
      ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-summary-mode)
      ;; (add-to-list 'semantic-default-submodes 'global-semantic-idle-completions-mode)
      ;; (add-to-list 'semantic-default-submodes 'global-cedet-m3-minor-mode t)
      ;; (setq semantic-complete-inline-analyzer-displayor-class
      ;;       'semantic-displayor-tooltip)

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
      )
  (progn 
   (message "your CEDET path does not exist. Download it from: http://cedet.sourceforge.net/setup.shtml and make it.")
   )
  )

;; Enable Semantic
(semantic-mode 1)
;; Enable EDE (Project Management) features - This adds a menu named: "Developement"
;; (global-ede-mode 1)
;;

(defun my-behavior-custom-company-cpp-mode ()
  (company-mode 1)
  (company-semantic 1)
  )

(add-hook 'c++-mode-hook #'my-behavior-custom-company-cpp-mode)
(add-hook 'c-mode-hook #'my-behavior-custom-company-cpp-mode)


;;cmake
(autoload 'cmake-font-lock-activate "cmake-font-lock" nil t)
(add-hook 'cmake-mode-hook 'cmake-font-lock-activate)


;; adding company mode to emacs lisp
(add-hook 'emacs-lisp-mode-hook #'company-mode)


;; autopair
;; (require 'autopair)
;;   (autopair-global-mode) ;; enable autopair in all buffers

;; going for electric-pair
(electric-pair-mode 1)

;; elpy
;; (elpy-enable)
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(use-package yasnippet-snippets         ; Collection of snippets
  :ensure t)

;; anyway loaded by elpy.
(use-package yasnippet
  :ensure t
  :config
  (progn
    (yas-global-mode 1)
    ;; (with-eval-after-load 'yasnippet
    ;;   (validate-setq yas-snippet-dirs '(yasnippet-snippets-dir)))
    ))

(use-package hydra
  :ensure t)

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
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; aim -- pdf tools installs itself, including pdf-tools-install, if needed. Only on linux.
(use-package pdf-tools
  :if (memq window-system '(x))
  :ensure t
  ;; :magic ("%PDF" . pdf-view-mode)
  :config
  (progn
    (pdf-tools-install :no-query)
    (add-hook 'pdf-view-mode-hook 'auto-revert-mode)
    )  
  )


;; gscholar-bibtex
(setq gscholar-bibtex-default-source "Google Scholar")



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
  :config
  (progn
    (add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
    ))

(use-package shell-pop
  :ensure t
  :config
  (progn
    (global-set-key (kbd "<C-M-return>") 'shell-pop)
    ))

(use-package gnuplot-mode
  :ensure t
  :config
  (progn
    (add-to-list 'auto-mode-alist '("\\.gnu\\'" . gnuplot-mode))
    ))

(use-package dired-rainbow
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
  "todo"
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
(global-set-key (kbd "<f8>") 'treemacs)
(global-set-key (kbd "S-<f8>") 'centaur-tabs-mode)

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

(message ".emacs loaded in %s seconds" (mapconcat 'int-to-string (rest (time-since *start-time*)) "."))
