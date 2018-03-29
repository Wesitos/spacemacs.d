;;; packages.el --- platformio layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: Pedro Palacios Avila <pedro_gpa@hotmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `platformio-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `platformio/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `platformio/pre-init-PACKAGE' and/or
;;   `platformio/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:

(defconst platformio-packages
  '(
    platformio-mode
    company
    irony
    projectile
    (irony-eldoc :toogle (configuration-layer/package-usedp 'eldoc))
    (company-irony :toogle (configuration-layer/package-usedp 'company))
    flycheck
    (flycheck-irony :toogle (configuration-layer/package-usedp 'flycheck))
    )
  "The list of Lisp packages required by the platformio layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun platformio/init-platformio-mode ()
  (use-package platformio-mode
    :defer t
    :commands (platformio-conditionally-enable)
    :init
    (add-hook 'c++-mode-hook
              (lambda ()
                (irony-mode t)
                (irony-eldoc t)
                (platformio-conditionally-enable)
                ))
    :config
    (progn
      (spacemacs|diminish platformio-mode " 👽" " p"))
    ))

(defun platformio/init-irony ()
  (use-package irony
    :if (configuration-layer/package-usedp 'company)
    :defer t
    :init
    (progn
      (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
      (spacemacs|diminish irony-mode " ⓘ" " i")
      )))

(defun platformio/init-irony-eldoc ()
  (use-package irony-eldoc
    :defer t))

(defun platformio/post-init-company ())

(defun platformio/post-init-projectile ()
  (with-eval-after-load 'projectile
      (add-to-list 'projectile-project-root-files "platformio.ini"))
  )

(when (configuration-layer/layer-usedp 'auto-completion)

  ;; Add the backend to the major-mode specific backend list
  (defun platformio/init-company-irony ()
    (use-package company-irony
      :defer t
      :init
      (spacemacs|add-company-backends
        :backends company-irony
        :modes platformio-mode)
       )))

(defun platformio/init-flycheck-irony ()
  (use-package flycheck-irony
    :defer t
    :init
    (eval-after-load 'flycheck
      '(add-hook 'flycheck-mode-hook 'flycheck-irony-setup))))

(defun platformio/post-init-flycheck ()
  (spacemacs/enable-flycheck 'platformio-mode))

;;; packages.el ends here
