;; -*- mode: dotspacemacs -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '("~/.spacemacs.d/private/")
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     php
     better-defaults
     helm
     (auto-completion :variables
                      auto-completion-return-key-behavior nil
                      auto-completion-tab-key-behavior 'complete
                      auto-completion-enable-snippets-in-popup t
                      auto-completion-enable-help-tooltip t
                      auto-completion-complete-with-key-sequence-delay 0.1
                      auto-completion-enable-sort-by-usage t)
     syntax-checking
     (spell-checking :variables
                     spell-checking-enable-by-default nil
                     enable-flyspell-auto-completion nil)
     themes-megapack
     better-defaults
     (colors :variables
             colors-enable-rainbow-identifiers t)

     ;; Programming Languages
     (c-c++ :variables
            c-c++-default-mode-for-headers 'c++-mode)
     clojure
     emacs-lisp
     major-modes
     (javascript
      :variables javascript-backend 'tern
      node-add-modules-path t)
     (tern :variables tern-disable-port-files nil)
     python
     ruby
     (sql :variables sql-capitalize-keywords t)

     ;; Non-programming languages
     markdown
     yaml
     csv
     raml

     ;; Bring order to life
     (org :variables org-enable-reveal-js-support t)
     calendar
     wakatime
     wesitos-gcal ;; Gcal configuration

     ;; Documents
     latex
     bibtex
     restructuredtext

     ;; Web
     html
     react
     restclient
     nginx

     ;; Tools
     floobits
     platformio
     ipython-notebook
     ansible
     docker
     git
     systemd
     gnus
     (shell :variables shell-default-shell 'multi-term)

     ;; fun stuff
     xkcd
     spotify

     ;; spell-checking
     ;; version-control
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages, then consider creating a layer. You can also put the
   ;; configuration in `dotspacemacs/user-config'.
   dotspacemacs-additional-packages
   '(
     add-node-modules-path
     js-comint
     editorconfig
     graphql-mode
     (yasnippet :location elpa)
     gnus-desktop-notify
     exec-path-from-shell
     )
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '(auto-complete-rst)
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; If non nil ELPA repositories are contacted via HTTPS whenever it's
   ;; possible. Set it to nil if you have no way to use HTTPS in your
   ;; environment, otherwise it is strongly recommended to let it set to t.
   ;; This variable has no effect if Emacs is launched with the parameter
   ;; `--insecure' which forces the value of this variable to nil.
   ;; (default t)
   dotspacemacs-elpa-https t
   ;; Maximum allowed time in seconds to contact an ELPA repository.
   dotspacemacs-elpa-timeout 5
   ;; If non nil then spacemacs will check for updates at startup
   ;; when the current branch is not `develop'. (default t)
   dotspacemacs-check-for-update t
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-editing-style 'emacs
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner 'official
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '((recents . 7)
                                (projects . 5))
   ;; True if the home buffer should respond to resize events.
   dotspacemacs-startup-buffer-responsive t
   ;; Default major mode of the scratch buffer (default `text-mode')
   dotspacemacs-scratch-mode 'text-mode
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(spacemacs-dark
                         spacemacs-light
                         solarized-light
                         solarized-dark
                         leuven
                         monokai
                         zenburn)

   ;; Set the theme for the Spaceline. Supported themes are `spacemacs',
   ;; `all-the-icons', `custom', `vim-powerline' and `vanilla'. The first three
   ;; are spaceline themes. `vanilla' is default Emacs mode-line. `custom' is a
   ;; user defined themes, refer to the DOCUMENTATION.org for more info on how
   ;; to create your own spaceline theme. Value can be a symbol or list with\
   ;; additional properties.
   ;; (default '(spacemacs :separator wave :separator-scale 1.5))
   dotspacemacs-mode-line-theme '(spacemacs :separator wave :separator-scale 1.5)

   ;; If non nil the cursor color matches the state color in GUI Emacs.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-Scale' Allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Mono"
                               :size 13
                               :weight normal
                               :width normal
                               :powerline-scale 1.0)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The key used for Emacs commands (M-x) (after pressing on the leader key).
   ;; (default "SPC")
   dotspacemacs-emacs-command-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; These variables control whether separate commands are bound in the GUI to
   ;; the key pairs C-i, TAB and C-m, RET.
   ;; Setting it to a non-nil value, allows for separate commands under <C-i>
   ;; and TAB or <C-m> and RET.
   ;; In the terminal, these pairs are generally indistinguishable, so this only
   ;; works in the GUI. (default nil)
   dotspacemacs-distinguish-gui-tab nil
   ;; (Not implemented) dotspacemacs-distinguish-gui-ret nil
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Name of the default layout (default "Default")
   dotspacemacs-default-layout-name "Default"
   ;; If non nil the default layout name is displayed in the mode-line.
   ;; (default nil)
   dotspacemacs-display-default-layout nil
   ;; If non nil then the last auto saved layouts are resume automatically upon
   ;; start. (default nil)
   dotspacemacs-auto-resume-layouts nil
   ;; Size (in MB) above which spacemacs will prompt to open the large file
   ;; literally to avoid performance issues. Opening a file literally means that
   ;; no major mode or minor modes are active. (default is 1)
   dotspacemacs-large-file-size 1
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; Maximum number of rollback slots to keep in the cache. (default 5)
   dotspacemacs-max-rollback-slots 5
   ;; If non nil, `helm' will try to minimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; Controls fuzzy matching in helm. If set to `always', force fuzzy matching
   ;; in all non-asynchronous sources. If set to `source', preserve individual
   ;; source settings. Else, disable fuzzy matching in all sources.
   ;; (default 'always)
   dotspacemacs-helm-use-fuzzy 'always
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.3
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar t
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup t
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup nil
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 97
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 97
   ;; If non nil show the titles of transient states. (default t)
   dotspacemacs-show-transient-state-title t
   ;; If non nil show the color guide hint for transient state keys. (default t)
   dotspacemacs-show-transient-state-color-guide t
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols t
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non nil line numbers are turned on in all `prog-mode' and `text-mode'
   ;; derivatives. If set to `relative', also turns on relative line numbers.
   ;; (default nil)
   dotspacemacs-line-numbers t
   ;; Code folding method. Possible values are `evil' and `origami'.
   ;; (default 'evil)
   dotspacemacs-folding-method 'evil
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; If non-nil pressing the closing parenthesis `)' key in insert mode passes
   ;; over any automatically added closing parenthesis, bracket, quote, etc…
   ;; This can be temporary disabled by pressing `C-q' before `)'. (default nil)
   dotspacemacs-smart-closing-parenthesis nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "pt" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   ;; Delete whitespace while saving buffer. Possible values are `all'
   ;; to aggressively delete empty line and long sequences of whitespace,
   ;; `trailing' to delete only the whitespace at end of lines, `changed'to
   ;; delete only whitespace for changed lines or `nil' to disable cleanup.
   ;; (default nil)
   dotspacemacs-whitespace-cleanup "changed"
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init', before layer configuration
executes.
 This function is mostly useful for variables that need to be set
before packages are loaded. If you are unsure, you should try in setting them in
`dotspacemacs/user-config' first."
  (setq
   custom-file "~/.spacemacs.d/.custom-settings"
   ;; Avoid using helm when completing at point
   ;; helm-mode-handle-completion-in-region nil
   gnus-init-file "~/.spacemacs.d/gnus.el"
   )
  (add-to-list 'default-frame-alist
               '(font . "-ADBE-Fira Mono-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1"))
  )

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
This function is called at the very end of Spacemacs initialization after
layers configuration.
This is the place where most of your configurations should be done. Unless it is
explicitly specified that a variable should be set before a package is loaded,
you should place you code here."
  (setq
   user-full-name "Pedro Palacios Avila"
   user-mail-address "wesitos@makerlabperu.org")

  ;; GPG related options
  (setq
   magit-commit-arguments '("-S")
   magit-log-arguments '("--graph" "--decorate" "--show-signature" "-n256")
   )

  ;; Exec-path-from-shell
  (setq
   exec-path-from-shell-variables '(
                                    "PATH"
                                    "MANPATH"
                                    "SSH_AUTHSOCK"
                                    "WORKON_HOME"
                                    "PYENV_ROOT"
                                    "PIPENV_DEFAULT_PYTHON_VERSION"
                                    ))
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  ;; Mode line
  (setq
   spaceline-battery-p nil
   spaceline-org-clock-p t
   spaceline-hud-p nil
   )
  (spacemacs/toggle-mode-line-battery-on)

  ;; Dired
  ;; Load Dired X when Dired is loaded.
  (require 'dired-x)
  (setq-default dired-omit-files-p t) ; Buffer-local variable
  (setq
   dired-omit-files (concat "^\\.?#\\|^\\.$\\|^\\.\\.$\\|^\\.tern-port$")
   dired-omit-verbose nil
   dired-listing-switches "-alh --group-directories-first"
   )

  ;; Gnus
  (setq gnus-secondary-select-methods
        '(
          (nnimap "mkl"
                  (nnimap-address
                   "imap.makerlabperu.org")
                  (nnimap-inbox "INBOX")
                  (nnimap-server-port "imaps")
                  (nnimap-server-port 993)
                  (nnimap-stream ssl)
                  (nnir-search-engine imap))
          ))

  (setq gnus-asynchronous t
        gnus-use-cache t
        gnus-read-active-file 'some)

  (setq gnus-thread-sort-functions
        '(gnus-thread-sort-by-most-recent-date
          (not gnus-thread-sort-by-number)))

  (defun th/message-send-and-exit-multiple (addresses)
    (interactive (list (split-string (read-string "Adresses: ")
                                     "," t "[[:space:]]")))
    (while addresses
      (let ((address (car addresses)))
        (setq addresses (cdr addresses))
        (message-remove-header "To")
        (message-add-header (format "To: %s" address))
        (if addresses
            (message-send)
          (message-send)))))

  ;; Outbound server
  (setq message-send-mail-function 'smtpmail-send-it
        smtpmail-default-smtp-server "smtp.makerlabperu.org")

  (setq gnus-message-archive-method '(nnimap "imap.makerlabperu.org")
        gnus-message-archive-group "Enviados")

  ;; set return email address based on incoming email address
  (setq gnus-posting-styles
        '(((header "to" "wesitos@makerlabperu.org")
           (address "wesitos@makerlabperu.org"))
          ((header "to" "admin@makerlabperu.org")
           (address "admin@makerlabperu.org"))
          ((header "to" "admision@makerlabperu.org")
           (address "admision@makerlabperu.org"))))

  (setq nnmh-directory "~/mail/gnus/")
  (setq message-directory "~/mail/gnus/")

  (add-hook 'gnus-article-mode-hook
            (lambda ()
              (face-remap-add-relative 'default :size 16)
  ))

  ;; set-mark bug on emacs 25.1 workaround... in theory
  (require 'ansible-doc)

  ;; Avoid calling autocompletion-in-region
  (with-eval-after-load "company"
    (define-key spacemacs-js2-mode-map-root-map
      (kbd "<tab>") 'company-indent-or-complete-common)
    (define-key spacemacs-rjsx-mode-map-root-map
      (kbd "<tab>") 'company-indent-or-complete-common)
    )


  ;; Ispell config
  (with-eval-after-load "ispell"
    (setq-default ispell-program-name "hunspell"
                  ispell-dictionary "castellano,english"
                  )
    (ispell-set-spellchecker-params)
    (ispell-hunspell-add-multi-dic "castellano,english"))
  (list-load-path-shadows)

  ;; File lookup
  (use-package helm
    :defer nil
    :bind ("C-c o" . helm-overlord)
    :config
    (progn
      (require 'helm-projectile)
      (require 'helm-x-files)
      (defun helm-overlord ()
        (interactive)
        (helm :sources
              '(helm-source-buffers-list
                helm-source-projectile-buffers-list
                helm-source-projectile-files-list
                helm-source-projectile-projects
                helm-source-recentf
                helm-source-file-cache
                )
              :ff-transformer-show-only-basename nil
              :buffer "*helm-overlord*"
              :truncate-lines helm-buffers-truncate-lines
              ))))

  (use-package projectile
    :config
    (add-to-list 'projectile-globally-ignored-directories "node_modules")
    )

  ;; Org config
  ;; Fontify org-mode code blocks
  (setq-default
   org-src-fontify-natively t
   ;; org-mode: Don't ruin S-arrow to switch windows please (use M-+ and M--
   ;; instead to toggle)
   org-replace-disputed-keys t
   org-hide-leading-stars t
   org-odd-levels-only t
   org-ref-default-bibliography '("references.bib")
   reftex-default-bibliography '("references.bib")
   ;; TODO progress logging stuff
   org-log-done 'time
   org-latex-listings 'minted
   org-latex-compiler "xelatex"
   org-latex-pdf-process'("latexmk -pdf -f -pdflatex='xelatex --shell-escape -file-line-error -interaction=nonstopmode' -outdir=%o %f")
   org-latex-default-packages-alist '(
                                      ;; Not used with XeLaTeX
                                      ("AUTO" "inputenc" t ("pdflatex"))
                                      ("T1" "fontenc" t ("pdflatex"))
                                      ("" "fontspec" t ("xelatex"))
                                      ("" "polyglossia" t ("xelatex"))
                                      ("" "graphicx" t)
                                      ("" "grffile" t)
                                      ("" "longtable" nil)
                                      ("" "wrapfig" nil)
                                      ("" "rotating" nil)
                                      ("normalem" "ulem" t)
                                      ("" "amsmath" t)
                                      ("" "textcomp" t)
                                      ("" "amssymb" t)
                                      ("" "capt-of" nil)
                                      ("" "hyperref" nil)
                                      ("dvipsnames" "xcolor")
                                      )
   ;; Agenda and clock
   org-clock-persist 'history
   )

  (with-eval-after-load 'org
    (org-clock-persistence-insinuate)
    (defun refresh-org-agenda-files ()
      (interactive)
      (load-library "find-lisp")
      (setq org-agenda-files (find-lisp-find-files "~/org/agenda" "\.org$"))
      )
    (refresh-org-agenda-files)

    (add-to-list 'org-latex-packages-alist '("" "minted"))
    (defun toggle-org-reveal-export-on-save ()
      (interactive)
      (if (memq 'org-reveal-export-to-html after-save-hook)
          (progn
            (remove-hook 'after-save-hook 'org-reveal-export-to-html t)
            (message "Disabled org reveal export on save for current buffer..."))
        (if (memq 'org-reveal-export-current-subtree after-save-hook)
            (remove-hook 'after-save-hook 'org-reveal-export-current-subtree t))
        (add-hook 'after-save-hook 'org-reveal-export-to-html nil t)
        (message "Enabled org reveal export on save for current buffer...")))
    (defun toggle-org-reveal-export-subtree-on-save ()
      (interactive)
      (if (memq 'org-reveal-export-current-subtree after-save-hook)
          (progn
            (remove-hook 'after-save-hook 'org-reveal-export-current-subtree t)
            (message "Disabled org reveal export current subtree on save for current buffer..."))
        (if (memq 'org-reveal-export-to-html after-save-hook)
            (remove-hook 'after-save-hook 'org-reveal-export-to-html t))
        (add-hook 'after-save-hook 'org-reveal-export-current-subtree nil t)
        (message "Enabled org reveal export current subtree save for current buffer...")))
    )

  ;; Javascript
  (setq-default
   js2-basic-offset 2
   js-indent-level 2
   js2-mode-assume-strict t
   js-switch-indent-offset 2
   js2-mode-show-strict-warnings nil
   js2-mode-show-parse-errors nil
   js-expr-indent-offset 0
   js-paren-indent-offset 0
   json-reformat:indent-width 2
   )

  ;; Add .mjs extension autoload
  (add-to-list 'auto-mode-alist '("\\.mjs\\'" . js2-mode))
  (add-to-list 'auto-mode-alist '("\\.tern-project\\'" . json-mode))

  ;; json-mode by default locally sets indent-level to 4
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))

  (advice-add
   'js--multi-line-declaration-indentation
   :around (lambda (orig-fun &rest args) nil))

  (advice-add 'js--proper-indentation :override 'js--proper-indentation-custom)

  (use-package js-comint
    :ensure t
    :init
    (progn
      (defun inferior-js-mode-hook-setup ()
        (add-hook 'comint-output-filter-functions 'js-comint-process-output))
      (add-hook 'inferior-js-mode-hook 'inferior-js-mode-hook-setup t)
      (add-hook 'js2-mode-hook
                (lambda ()
                  (let ((bindlist
                         '(("C-c c" . 'js-send-last-sexp)
                           ("C-c C-b" . 'js-send-buffer)
                           ("C-c C-f" . 'js-load-file))))
                    (dolist (pair bindlist)
                      (local-set-key (kbd (car pair)) (cdr pair)))))))
    )

  (add-hook 'scss-mode-hook
            (lambda ()
              (use-linter-from-node-modules
               "sass/scss-sass-lint"
               "sass-lint/bin/sass-lint.js")))

  (add-hook 'web-mode-hook 'rainbow-mode)
  (add-hook 'js2-mode-hook 'rainbow-mode)

  (use-package mmm-mode
    :defer t
    :init
    (setq mmm-global-mode 'maybe
          mmm-parse-when-idle t
          )
    :config
    (progn
      (mmm-add-classes
       '((rjsx-graphql
          :submode graphql-mode
          :front "\bgql`"
          :back "`"
          ))
       )
      (mmm-add-mode-ext-class 'rjsx-mode nil 'rjsx-graphql)
      )
    )

  (use-package rjsx-mode
    :defer t
    :config
    (require 'mmm-mode)
    )

  ;; GraphQL
  (use-package graphql-mode
    :ensure t
    :mode "\\.graphql\\'"
    :init
    (spacemacs|diminish graphql-mode "⬡" "g")
    )

  (setq-default
   flycheck-eslintrc ".eslintrc*")

  ;; Web-mode
  (setq-default
   css-indent-offset 2
   web-mode-markup-indent-offset 2
   web-mode-css-indent-offset 2
   web-mode-code-indent-offset 2
   web-mode-attr-indent-offset 2
   emmet-self-closing-tag-style " /"
   emmet-indentation 2
   )
  (use-package emmet-mode
    :defer t
    :config
    (progn
      (unbind-key "<emacs-state> TAB" emmet-mode-keymap)
      (unbind-key "<emacs-state> <tab>" emmet-mode-keymap)))

  ;; Python
  (setq
   flycheck-python-flake8-executable "python3"
   flycheck-python-pylint-executable "python3"
   )

  (use-package pipenv
    :defer t
    :config
    (progn
      (setq
       pipenv-projectile-after-switch-function
       #'pipenv-projectile-after-switch-extended)
      (spacemacs|diminish pipenv-mode "🎁" "p")
      (add-hook 'python-mode-hook
                #'(lambda ()
                    (setq flycheck-checker 'python-pylint)
                    (pipenv-mode)))
      ))

  (spacemacs|diminish anaconda-mode "🐍" "a")
  ;; Matlab
  (setq-default
   pipenv-with-flycheck t
   pipenv-with-projectile t
   matlab-shell-command-switches '("-nodesktop" "-nosplash")
   )

  ;; Expand Region
  (setq-default
   expand-region-fast-keys-enabled nil
   )

  ;; Smart parens global mode
  (spacemacs/toggle-smartparens-globally-on)

  ;; ------Smart parens bindings------
  ;; Delete
  (define-key sp-keymap (kbd "C-k") 'sp-kill-hybrid-sexp)
  (define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)
  (define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
  (define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
  (define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)
  (define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
  (define-key sp-keymap (kbd "C-M-a") 'sp-beginning-of-sexp)
  (define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
  ;; Move parens
  (define-key sp-keymap (kbd "<C-right>") 'sp-forward-slurp-sexp)
  (define-key sp-keymap (kbd "<C-left>") 'sp-backward-slurp-sexp)

  ;; Expand region binding
  (global-set-key (kbd "C-\\") 'er/expand-region)
  ;; Mark paragraph
  (global-set-key (kbd "M-h") 'mark-paragraph)
  ;; <menu> key
  (global-set-key (kbd "<menu>") 'helm-M-x)
  ;; prevent madness
  (global-set-key (kbd "C-x 2")
                  (lambda ()
                    (interactive)
                    (split-window-vertically)
                    (other-window 1)))
  (global-set-key (kbd "C-x 3")
                  (lambda ()
                    (interactive)
                    (split-window-horizontally)
                    (other-window 1)))
  (delete-selection-mode t)

  ;; Wakatime
  (setq
   wakatime-cli-path "~/.pyenv/shims/wakatime"
   wakatime-python-bin nil
   )

  (global-wakatime-mode t)
  (spacemacs|diminish wakatime-mode "🕑" "w")

  (spacemacs|diminish holy-mode "✝️" "h")
  (setq
   safe-local-variable-values
   '(
     (encoding . utf-8)
     (encoding . utf8)
     (org-latex-minted-options '(("bgcolor=MonokaiBg")))
     (org-latex-minted-options quote
                               (("bgcolor" "MonokaiBg")))
     (ispell-dictionary . "castellano,english")
     (ispell-dictionary . "castellano")
     (ispell-dictionary . "english")))
  )


(defun js--proper-indentation-custom (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)    ; inside comment
           (js--get-c-offset 'c (nth 8 parse-status)))
          ((nth 3 parse-status) 0) ; inside string
          ((eq (char-after) ?#) 0)
          ((save-excursion (js--beginning-of-macro)) 4)
          ;; Indent array comprehension continuation lines specially.
          ((let ((bracket (nth 1 parse-status))
                 beg)
             (and bracket
                  (not (js--same-line bracket))
                  (setq beg (js--indent-in-array-comp bracket))
                  ;; At or after the first loop?
                  (>= (point) beg)
                  (js--array-comp-indentation bracket beg))))
          ((js--ctrl-statement-indentation))
          ((nth 1 parse-status)
           ;; A single closing paren/bracket should be indented at the
           ;; same level as the opening statement. Same goes for
           ;; "case" and "default".
           (let ((same-indent-p (looking-at "[]})]"))
                 (switch-keyword-p (looking-at "default\\_>\\|case\\_>[^:]"))
                 (continued-expr-p (js--continued-expression-p))
                 (original-point (point))
                 (open-symbol (nth 1 parse-status)))
             (goto-char (nth 1 parse-status)) ; go to the opening char
             (skip-syntax-backward " ")
             (when (eq (char-before) ?\)) (backward-list))
             (back-to-indentation)
             (js--maybe-goto-declaration-keyword-end parse-status)
             (let* ((in-switch-p (unless same-indent-p
                                   (looking-at "\\_<switch\\_>")))
                    (same-indent-p (or same-indent-p
                                       (and switch-keyword-p
                                            in-switch-p)))
                    (indent
                     (cond (same-indent-p
                            (current-column))
                           (continued-expr-p
                            (goto-char original-point)
                            ;; Go to beginning line of continued expression.
                            (while (js--continued-expression-p)
                              (forward-line -1))
                            ;; Go to the open symbol if it appears later.
                            (when (> open-symbol (point))
                              (goto-char open-symbol))
                            (back-to-indentation)
                            (+ (current-column)
                               js-indent-level
                               js-expr-indent-offset))
                           (t
                            (+ (current-column) js-indent-level
                               (pcase (char-after (nth 1 parse-status))
                                 (?\( js-paren-indent-offset)
                                 (?\[ js-square-indent-offset)
                                 (?\{ js-curly-indent-offset)))))))
               (if in-switch-p
                   (+ indent js-switch-indent-offset)
                 indent))))
          ((js--continued-expression-p)
           (+ js-indent-level js-expr-indent-offset))
          (t 0))))
