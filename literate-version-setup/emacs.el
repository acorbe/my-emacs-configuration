;; tries to load the latest org if sraight installed it
(if (file-directory-p "~/.emacs.d/straight/build/org/")
    (add-to-list 'load-path "~/.emacs.d/straight/build/org/"))

;; loads org-mode and then configuration as literate 
(require 'org)
(org-babel-load-file
 (expand-file-name "emacs_2023.org"
		   "GIT_REPO_DIR"
		   ))
