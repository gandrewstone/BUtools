
;; Choose your operating system (All of these variables must be set to nil except for one!)
(setq cfg-linux 1)
(setq cfg-nt nil)
(setq cfg-solaris nil)

;; Choose what you want integrated into emacs. Set to a 1 to use, nil if not using

(setq use-python 1)
(setq use-java nil)
(setq use-c 1)
(setq use-cpp 1)  ;; c++
(setq use-sml nil)  ;; standard ML
(setq use-git 1)
(setq use-clang 1)
(setq use-cscope 1)               ;; Actually cscope will not work on NT

;; C flags
(setq compile-command "make -j11 V=1")

;; Python
(setq py-python-command "/usr/bin/python3.6")

;; delete trailing whitespace?
;(add-hook 'c-mode-hook
;                (lambda () (add-to-list 'write-file-functions 'delete-trailing-whitespace)))

;; BASIC EMACS CONFIGURATION

;; Allow emacs to copy-paste to other applications
(setq x-select-enable-clipboard t)

(setq shell-command "")  ;; Use whatever is set in your environment

; Display columns as well
(setq column-number-mode t)

; don't go to the end of each line if the prev line was longer.
;(setq picture-mode t)

;; indentation and tabs vs spaces
(setq c-tab-always-indent nil)
(setq-default indent-tabs-mode nil)   ;; use spaces to implement tabs
(setq tab-width 4)
(setq standard-indent 1)
(setq c-default-style "bsd"
      c-basic-offset 4)

(setq default-major-mode 'indented-text-mode)
;;(setq default-case-fold-search nil)   ;; case is significant when expanding

(setq termtype (getenv "TERM"))
(setq auto-save-interval 800)
(setq inhibit-startup-message 1)
(setq max-lisp-eval-depth 400)        ;; default is 200, bump up for mail
(setq make-backup-files nil)
(setq display-time-day-and-date t)

; Turn it on in all modes that support it
(setq global-font-lock-mode t)

; Use maximum decoration
(setq font-lock-maximum-decoration 't)

; Turn on colorization automatically
(setq-default font-lock-auto-fontify t)

; Don't provide filename completion for those ending in the following.
(setq completion-ignored-extensions
      '("~" ".o" ".so" ".a" "#" ))

; case insensitive search
(setq case-fold-search t)

(setq show-paren-delay 0)
(show-paren-mode t)

(display-time)
(delete-selection-mode 1)

(put 'eval-expression 'disabled nil)


(defvar font-lock-display-type "color")

(add-hook 'find-file-hooks 'turn-on-font-lock)
(add-hook 'emacs-lisp-mode-hook 'turn-on-font-lock)
(add-hook 'makefile-mode-hook 'turn-on-font-lock)
(add-hook 'cperl-mode-hook 'turn-on-font-lock)
(add-hook 'dired-mode-hook 'turn-on-font-lock)
(add-hook 'info-mode-hook 'turn-on-font-lock)
(add-hook 'perl-mode-hook 'turn-on-font-lock)
(add-hook 'mail-mode-hook 'turn-on-font-lock)
(add-hook 'rmail-mode-hook 'turn-on-font-lock)
(add-hook 'c-mode-hook 'turn-on-font-lock)
(add-hook 'vm-mode-hook 'turn-on-font-lock)
(add-hook 'vm-summary-mode-hook 'turn-on-font-lock)
(add-hook 'mime/viewer-mode-hook 'turn-on-font-lock)
(add-hook 'TeX-mode-hook 'turn-on-font-lock)
(add-hook 'LaTeX-mode-hook 'turn-on-font-lock)

;;; Fix junk characters in shell mode
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun track-shell-directory/procfs ()
    (shell-dirtrack-mode 0)
    (add-hook 'comint-preoutput-filter-functions
              (lambda (str)
                (prog1 str
                  (when (string-match comint-prompt-regexp str)
                    (cd (file-symlink-p
                         (format "/proc/%s/cwd" (process-id
                                                 (get-buffer-process
                                                  (current-buffer)))))))))
              nil t))

(add-hook 'shell-mode-hook 'track-shell-directory/procfs)


;; BASIC EMACS CONFIGURATION COMPLETE

;; Set up the emacs load path
(setq emacs-env-path (file-name-directory load-file-name))
(add-to-list 'load-path emacs-env-path)



;; TOOLS

;; format highlighting
(require 'whitespace)
(setq whitespace-line-column 120) ;; limit line length to 120 chars
;(setq whitespace-style '(face lines-tail trailing tabs))
(setq whitespace-style '(face trailing tabs)) ; add lines-tail if you don't have "fill-column-indicator.el"
(add-hook 'prog-mode-hook 'whitespace-mode)
(load "fill-column-indicator.el")
; (require 'fill-column-indicator)
(setq fci-rule-width 1)
(setq fci-rule-color "darkblue")
(setq fci-rule-column 120)
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'c++-mode-hook 'fci-mode)

;; clang formatting
(if use-clang (let ()
  (load "clang-format-3.8/clang-format.el")
  (global-set-key [C-tab] 'clang-format-region)
))

;; git
(if use-git
  (add-to-list 'load-path "git/")
    (require 'git)
    (require 'git-blame))


;; My key mappings
(load "stonekeys.el")

;;(load "python-mode.el")

(if use-cscope (load "stonecscope.el") (princ "Using cscope"))

(defun tags-build() (interactive)
   "Build an emacs tags file of the .c and .h files in this and all subdirectories."
   (shell-command "find . -name \"*.[ch]\" | etags - TAGS"))

(load "langtemplates.el")
