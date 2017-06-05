(load "zoom-frm.el")

(define-key text-mode-map "\M-s" 'isearch-forward)             ;; ESC s
(define-key text-mode-map "\C-i" 'tab-to-tab-stop)             ;; C-i
(define-key text-mode-map "\M-\C-i" 'indent-relative)          ;; M-C-i

(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-=") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "s-+") '(lambda() (interactive) (zoom-all-frames-in)))
(global-set-key (kbd "s-=") '(lambda() (interactive) (zoom-all-frames-in)))
(global-set-key (kbd "s--") '(lambda() (interactive) (zoom-all-frames-out)))


(defun gas-buffer-file-list (lst)
  (if lst
      (if (buffer-file-name (car lst))
          (cons (buffer-file-name (car lst)) (gas-buffer-file-list (cdr lst)))
          (gas-buffer-file-list (cdr lst)))
      nil))

; test (sort (gas-buffer-file-list (buffer-list)) 'string<)

(defun lastcar (lst) (if (cdr lst) (lastcar (cdr lst)) (car lst)))
; test (lastcar '(1 2 3 4 5 6))

(defun gas-prev-string-rec (x lst)
  (if lst
      (if (and x (cdr lst))
          (if (string= (car (cdr lst)) x) (car lst) (gas-prev-string-rec x (cdr lst)))
        (car lst)) nil))

(defun gas-next-string-rec (x lst)
  (if (and x lst)
      (if (string= (car lst) x)
          (if (cdr lst) (car (cdr lst)) nil)
        (gas-next-string-rec x (cdr lst)))
    nil))

(defun gas-prev-string (x lst)
  (if (string= (car lst) x) (lastcar lst) (gas-prev-string-rec x lst)))

(defun gas-next-string (x lst)
  (let ((resul (gas-next-string-rec x lst)))
    (if resul resul (car lst))))

                                        ;test
;(gas-prev-string "e" '("a" "b" "c" "d"))
                                        ;test
;(gas-next-string "e" '("a" "b" "c" "d"))


(defun lem () "reload _emacs file" (interactive) (load-file "~/.emacs"))

(defun gas-previous-buffer () "*go to the previous buffer" (interactive)
(switch-to-buffer (get-file-buffer (gas-prev-string (buffer-file-name (current-buffer))
                                                    (sort (gas-buffer-file-list (buffer-list)) 'string<)
                                                    ))))

(defun gas-next-buffer () "*go to the next buffer" (interactive)
(switch-to-buffer (get-file-buffer (gas-next-string (buffer-file-name (current-buffer))
                                                    (sort (gas-buffer-file-list (buffer-list)) 'string<)
))))

(defun gas-nth-buffer (i) "*go to the nth buffer" (interactive)
  (let ((fname (nth (- i 1) (sort (gas-buffer-file-list (buffer-list)) 'string<))))
  (if fname (switch-to-buffer (get-file-buffer fname)) (message "no buffer"))))


(defvar fileorder nil)

(defun gas-ff-hook (filename)
  (setq fileorder (cons filename fileorder)))


;; python stuff

(setq message-log-max 10000)
(setq py-directory nil)
(setq py-first-line nil)

(defun py-gas-run()
  "start a new python shell, doing import pdb; import <filename-of-current-buffer>"
  (interactive)
  (message "1")
  ; set the python process vars to this buffer if its a python file
  ;; (let ((file-buffer (if (get-buffer-process (current-buffer)) nil t))  ; true if this buffer is a file, false if its python
  (let ((file-buffer (if (buffer-file-name (current-buffer)) t nil))  ; true if this buffer is a file, false if its python
        )
    (message "1.1")
    (if file-buffer
        (let* ((file (buffer-file-name (current-buffer)))
               (tmp (message "1.2"))
               (file-name (file-name-nondirectory (file-name-sans-extension file)))
               (directory (file-name-directory  file)))
          (message (concat "File name is: " file))
          (setq py-directory directory)
          (setq py-first-line (concat "import pdb ; import " file-name "; " file-name ".Test()"))
          )))
  (message "2")
  ; kill the old python process
  (if (get-buffer "*Python*")
        (let ( (py-process (get-buffer-process "*Python*")))
          (if py-process (kill-process py-process)))
    nil)

  ; wait for python process to be dead
  (sit-for 1)
  (if py-directory (cd py-directory))
  (message "3")
  ; run python
  (if (get-buffer "*Python*")
      (progn
        (pop-to-buffer (get-buffer "*Python*"))
        (run-python py-python-command nil))

    (progn
  (if (fboundp 'python-switch-to-python)
        (python-switch-to-python "")
    (pop-to-buffer (process-buffer (python-shell-get-or-create-process py-python-command nil)) t))
  ))

  (goto-char (point-max)) ; go to the buffer's end

  (let ((buf  (get-buffer "*Python*")))
    (set-buffer buf)
    (if py-first-line (insert py-first-line))
    )
  )


(defun py-gas-run-str(str)
  "start a new python shell, running a string"
  (interactive)
  (if (get-buffer "*Python*") (kill-buffer "*Python*") nil)
  (let* (
         (arg  (if (string-equal py-which-bufname "Python") "-u" ""))
         (file (buffer-file-name (current-buffer)))
         (e1  (py-shell arg))
         (buf  (get-buffer "*Python*"))
         (file-name (file-name-nondirectory (file-name-sans-extension file)))
        )
        (insert str )
  ))

(global-set-key "\M-p" '(lambda() (interactive) (pop-to-buffer (process-buffer (python-shell-get-or-create-process py-python-command nil)) t)))
;; end python stuff

(global-unset-key '[f2])
(global-unset-key '[f10])
(global-set-key '[f1] 'find-file)
(global-set-key '[f2] 'save-buffer)
(global-set-key '[f3] 'gas-previous-buffer)
(global-set-key '[f4] 'gas-next-buffer)
(global-set-key '[f5] 'rename-buffer)
(global-set-key '[f6] 'switch-to-buffer)
(global-set-key '[f7] 'compile)
(global-set-key '[f8] 'next-error)
(global-set-key '[f9]  'gdb)
(global-set-key '[f10] 'shell)


(global-set-key '[f11] 'kill-buffer)
(global-set-key '[f12] 'save-buffers-kill-emacs)

(global-set-key '[M-right] 'gas-next-buffer)
(global-set-key '[M-left] 'gas-previous-buffer)

;; (global-set-key '[C-|] '(lambda() (interactive) (princ "worked")))
(global-set-key "\M--" 'split-window-vertically)
(global-set-key "\M-\\" 'split-window-horizontally)

(global-set-key "\M-0"  'delete-window)

; Ctrl + function key switches between files.
(global-set-key (quote [C-f1]) '(lambda() (interactive) (gas-nth-buffer 1)))
(global-set-key (quote [C-f2]) '(lambda() (interactive) (gas-nth-buffer 2)))
(global-set-key (quote [C-f3]) '(lambda() (interactive) (gas-nth-buffer 3)))
(global-set-key (quote [C-f4]) '(lambda() (interactive) (gas-nth-buffer 4)))

(global-set-key (quote [C-f5]) 'entitle)

(global-set-key (quote [C-f7])
                '(lambda() (interactive)
                   (let ((buf (get-buffer "*compilation*")))
                     (if buf (switch-to-buffer buf)
                       (let ((buf (get-buffer "Makefile")))
                         (if buf (switch-to-buffer buf) (message "no compilation buffer")))))))

(global-set-key (quote [M-f8])
                '(lambda() (interactive)
                   (let ((buf (get-buffer "*compilation*")))
                     (if buf (switch-to-buffer buf) (message "no compilation buffer")))))

(global-set-key (quote [C-f9])
                '(lambda() (interactive)
                   (let ((buf (get-buffer "*gud*")))
                     (if buf (switch-to-buffer buf) (message "no debugger buffer")))))


(global-set-key (quote [C-f10])
                '(lambda() (interactive)
                   (let ((buf (get-buffer "*shell*")))
                     (if buf (switch-to-buffer buf) (shell)))))


;; Tags key mappings
(global-set-key "\M-/" 'tags-apropos)
(global-set-key "\M-g" 'goto-line)

;; Color changing keys
(setq gasColorList '("nothing" "lightblue" "#c0ffc0" "pink" "#FFc0FF" "#FFFF80" "#FFb050" "white"))

;; Create a menu
(setq gasKeymap (make-sparse-keymap))
(define-key global-map [menu-bar gas] (cons "BU" gasKeymap ))

(define-key global-map
   [menu-bar gas lightblue] '("blue" . (lambda() (interactive) (set-background-color (nth 1 gasColorList)))))
(define-key global-map
   [menu-bar gas lightgreen] '("green" . (lambda() (interactive) (set-background-color (nth 2 gasColorList)))))
(define-key global-map
   [menu-bar gas pink] '("pink" . (lambda() (interactive) (set-background-color (nth 3 gasColorList)))))
(define-key global-map
   [menu-bar gas purple] '("purple" . (lambda() (interactive) (set-background-color (nth 4 gasColorList)))))
(define-key global-map
   [menu-bar gas yellow] '("yellow" . (lambda() (interactive) (set-background-color (nth 5 gasColorList)))))
(define-key global-map
   [menu-bar gas orange] '("orange" . (lambda() (interactive) (set-background-color (nth 6 gasColorList)))))
(define-key global-map
   [menu-bar gas white] '("white" . (lambda() (interactive) (set-background-color (nth 7 gasColorList)))))

(define-key global-map
   [menu-bar gas cscope] '("cscope" . (lambda() (interactive) (call-interactively 'cs))))
(define-key global-map
   [menu-bar gas recalc-cscope] '("recalc cscope" . (lambda() (interactive) (call-interactively 'cs-recalc))))

(define-key global-map
   [menu-bar gas title] '("title" . entitle ))



;; (setq gasCurCol 1)

;; (global-unset-key "\M-b")

;; (x-color-values "orange")

;; NOTE: Position cursor one past ending paren and type C-x C-e to evaluate the emacs lisp expression
;; (you can do it in this file)
;;(setq debug-on-error t)

(defun entitle(title)
  "name this window"
  (interactive "Mtitle: ")
  (if (equal "" title)
      (let* (  ;; (dir (file-name-directory (buffer-file-name (current-buffer))))
             (dir default-directory)
             (branch (shell-command-to-string "git symbolic-ref --short HEAD"))
             (br (if (string-prefix-p "fatal" branch) "" (substring branch 0 -1)))
             (remote (shell-command-to-string "git config --get remote.origin.url"))
             (rem (if (string= "" remote) "" (car (cdr (cdr (split-string remote "[:\.]"))))))
             )
        (message rem)
        (setq frame-title-format `(, br, "      ", rem, "    ", dir, "      ", (user-login-name) "@" ,(system-name) ))
          )
    (setq frame-title-format `(,title, "      ", (user-login-name) "@" ,(system-name) ))
    )
  )

(global-set-key "\C-f" 'isearch-forward)

(setq gasKeymapCscope (make-sparse-keymap))
(define-key gasKeymapCscope "s" 'cs-sym)
(define-key gasKeymapCscope "d" 'cs-def)
(define-key gasKeymapCscope "f" 'cs-file)
(define-key gasKeymapCscope "i" 'cs-include)
(define-key gasKeymapCscope "c" 'cs-caller)
(define-key gasKeymapCscope "h" 'cs-hide)
(define-key gasKeymapCscope "t" 'cs-text)
(define-key gasKeymapCscope "q" 'cs)
(define-key gasKeymapCscope "r" 'cs-recalc)
(define-key gasKeymapCscope "p" 'cs-py-recalc)
(define-key gasKeymapCscope "w" 'cs-window)

(global-set-key "\C-q" gasKeymapCscope)

(global-set-key [?\C-q ?1] '(lambda() (interactive) (set-background-color (nth 1 gasColorList))))
(global-set-key [?\C-q ?2] '(lambda() (interactive) (set-background-color (nth 2 gasColorList))))
(global-set-key [?\C-q ?3] '(lambda() (interactive) (set-background-color (nth 3 gasColorList))))
(global-set-key [?\C-q ?4] '(lambda() (interactive) (set-background-color (nth 4 gasColorList))))
(global-set-key [?\C-q ?5] '(lambda() (interactive) (set-background-color (nth 5 gasColorList))))
(global-set-key [?\C-q ?6] '(lambda() (interactive) (set-background-color (nth 6 gasColorList))))

(global-set-key "\M->" '(lambda() (interactive) (if (fboundp 'python-shift-right) (python-shift-right (region-beginning) (region-end)) (python-indent-shift-right (region-beginning) (region-end) ))))
(global-set-key "\M-<" '(lambda() (interactive) (if (fboundp 'python-shift-left) (python-shift-left (region-beginning) (region-end)) (python-indent-shift-left (region-beginning) (region-end) ))))

(defvar python-mode-hook nil "*Hook called by `python-mode'.")

(add-hook 'python-mode-hook (function (lambda ()
  (princ "python-mode-hook worked")
;  (local-set-key "\M->" '(lambda() (interactive) (if (fboundp 'python-shift-right) 'python-shift-right 'python-indent-shift-right)))
;  (local-set-key "\M->" (quote python-shift-right))
  (local-set-key "\M-<" (quote python-shift-left))
  (local-set-key '[f7]  (quote py-gas-run))
  (local-set-key '[f9]  (quote pdb))
  (local-set-key '[M-f7]  '(lambda() (interactive) (let ((buf (get-buffer "*Python*"))) (if buf (switch-to-buffer buf)))))
)))

(add-hook 'inferior-python-mode-hook (function (lambda ()
  (local-set-key (quote [f7]) (quote py-gas-run)))))


;; Windows Style Keys
(global-set-key "\C-z" 'undo)
(global-set-key "\C-v" 'yank)
(global-set-key "\C-c" 'kill-ring-save)
(global-set-key '[end] 'end-of-line)
(global-set-key '[home] 'beginning-of-line)
(global-set-key (quote [C-delete]) (quote kill-word))

(global-set-key "\C-i" 'indent-region)
(global-set-key "\M-i" 'indent-region)

(setq gasKeymapGit (make-sparse-keymap))
(global-set-key "\C-e" gasKeymapGit)
(global-set-key "\C-e b" 'git-blame-mode)

(setq gasKeymapGit (make-sparse-keymap))
(define-key gasKeymapGit "b" 'git-blame-mode)
(define-key gasKeymapGit "m" 'git-menu)
(define-key gasKeymapGit "d" 'git-diff-file)
(define-key gasKeymapGit "a" 'git-add-file)
(define-key gasKeymapGit "s" 'git-status)
(define-key gasKeymapGit "r" 'git-revert-file)
(define-key gasKeymapGit "l" 'git-log-file)

(global-set-key "\C-e" gasKeymapGit)

;; C specific modes

(add-hook 'c-mode-hook (function (lambda ()
  (local-set-key (quote [f7]) (quote compile)))))

(add-hook 'makefile-mode-hook (function (lambda ()
  (local-set-key (quote [f7]) (quote compile)))))
