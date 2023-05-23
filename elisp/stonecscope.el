(load "bufselect.el")

(setq csProjDir default-directory)

(setq gasCsExec (concat "cscope -d -R -l -q -f " (concat csProjDir "cscope.out" )))
(setq gasCsExecRecalc (concat "cscope -R -l -q -s " csProjDir " -f " (concat csProjDir "cscope.out" )))

(setq gasErrCsNotStarted "cscope process not found: restart with M-x cs command.")

(setq gas-cs-end-regexp "^>>\s$")
(setq gas-cs-menu-name "cscope")

(defun full-dir-cleanup(base)
  (if (string= "/" (substring base -1 nil))
                      (substring base 0 -1)
                    base))

(defun gas-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst))
)

(defvar gasCsTq nil)

(defvar gasCsMenuHist nil)
(defvar gasCsMenuPos  0)

(defvar gasCsMenuMap (let ((map (make-sparse-keymap)))
   (define-key map "s" 'cs-sym)
   (define-key map "d" 'cs-def)
   (define-key map "c" 'cs-called)
   (define-key map "t" 'cs-text)
   (define-key map "g" 'cs-grep)
   (define-key map "f" 'cs-file)
   (define-key map "i" 'cs-include)
   (define-key map "\M->" 'cs-hist-next)
   (define-key map "\M-<" 'cs-hist-prev)
   (define-key map "\M-," 'cs-hist-prev)
   (define-key map "\M-." 'cs-hist-next)

   map
))


(defun gasCsReady(q ans)
  (setq gasCsTq q)
  (princ "  Cscope is ready. ")
)

;; Just execute all args & return true
(defun cs-do (&rest args) t)


(defun gasCsStart (cmd)
  (princ cmd)
  (princ "\n")
  (setq gasCsp (start-process-shell-command "cscope" "*cscope*" cmd))
  (set-process-sentinel gasCsp 'gas-sentinel)
  (let ((q (tq-create gasCsp)))
    (princ "Starting cscope...please wait for ready msg.")
    (tq-enqueue q ""  gas-cs-end-regexp  q 'gasCsReady)
))


(defun cs (base)
  "start up cscope"
  (interactive "Dproject base: ")
;  (princ (concat base "\n"))
  (setq csProjDir (full-dir-cleanup base))

  (if (string= "/" (substring base -1 nil))
      (setq gasCsExec (concat "cscope -d -R -l -q -f " base "cscope.out" ))
      (setq gasCsExec (concat "cscope -d -R -l -q -f " base "/cscope.out" ))
      )

  (gasCsStart gasCsExec)
)

(defun cs-recalc (base)
  "start up cscope"
;;  (setq default-directory csProjDir)
  (interactive "Dproject base: ")
;  (princ (concat base "\n"))
  (setq csProjDir (full-dir-cleanup base))

;  (if (string= "/" (substring base -1 nil))
;      (setq gasCsExecRecalc (concat "cscope -R -l -q -s " base " -f " base "cscope.out" ))
;      (setq gasCsExecRecalc (concat "cscope -R -l -q -s " base "/ -f " base "/cscope.out" ))
;      )

  (setq gasCsExecRecalc (concat "cscope -R -l -q -s " csProjDir "/ -f " csProjDir "/cscope.out" ))

  (gasCsStart gasCsExecRecalc)
)

(defun cs-py-recalc (base)
  "recalculate cscope index"
  (interactive "Dpython project base: ")
;  (princ (concat base "\n"))
  (setq csProjDir (full-dir-cleanup base))

  (setq gasCsExecRecalc (concat "(cd " base "; pycscope -R)"))

;  (if (string= "/" (substring base -1 nil))
;      (setq gasCsExec (concat gasCsExecRecalc "; cscope -d -R -l -q -f " base "cscope.out" ))
;      (setq gasCsExec (concat gasCsExecRecalc "; cscope -d -R -l -q -f " base "/cscope.out" ))
;      )
  (setq gasCsExec (concat gasCsExecRecalc "; cscope -d -R -l -q -f " csProjDir "/cscope.out" ))
  (gasCsStart gasCsExec)
)



(defun cs-req()
  "Create a new Queue for cscope.  Only use if old one stopped working."
  (interactive)
  (if (boundp 'gasCsp)
     (setq gasCsTq (tq-create gasCsp))
     (princ "cscope process has stopped. Rerun M-x cs"))
)

(defun cs-window()
  "switch to a new active window"
  (interactive)
  (BsSetCurBuf)
  )

;;(defun cs-sym (sym)
;;  "find c symbol"
;;  (interactive "sSym: ")
;;  (if (boundp 'gasCsp)
;;      (let (( result (process-send-string gasCsp (concat "0" sym "\n"))))
;;        result)
;;        gasErrCsNotStarted
;;))

(setq gas-cs-filename-idx 0)
(setq context 1)
(setq gas-cs-line-idx 2)
(setq blurb 3)

;; Open a file & go to the line passed
(defun gas-cs-access (file line)
  (let ((buf (find-file file)))
    (switch-to-buffer buf)
    (goto-line line)
    (recenter (/ (window-height) 2))

))

(defun gas-cs-invalid() (princ "Invalid Choice"))
(defun gas-cs-notready() (princ "Cscope process is not yet ready."))


(defun gasCsHistAdd (newitem)
  (setq gasCsMenuHist (cons menu gasCsMenuHist))
  (setq gasCsMenuPos 0))

(defun gas-cs-show-menu(parsed why)
;   (princ "gas-cs-show-menu")
;     (format "gas-cs-show-menu ret: %s %s" parsed why))
  (BsInit gas-cs-menu-name gasCsMenuMap)
  (let* ((fmted (mapcar '(lambda (x) (cons (concat (nth gas-cs-filename-idx x) ":" (nth gas-cs-line-idx x) "\n") (list 'lambda '() (list 'gas-cs-access (nth gas-cs-filename-idx x) (string-to-number (nth gas-cs-line-idx x)))))) parsed))
         (menu (cons (cons why 'gas-cs-invalid) (cons (cons "\n" 'gas-cs-invalid) fmted))))

    (gasCsHistAdd menu)
    (BsSetChoices menu))
    (BsChoose)
)

(defun gas-expand-path(item)
  (if (string-prefix-p "./" (car item))
    (cons (concat csProjDir (substring (car item) 1 nil)) (cdr item))
    item)
  )

(defun fullpath-p(item)
  (or (string-prefix-p "/" (car item)) (string-prefix-p "./" (car item)))
  )

(defun gas-cs-parse(cookie answer why)
;   (princ
;     (format "gas-cs-parse Process ret: %s %s" answer why))
   (let* ((resp (split-string answer "\n"))
;                   V remove last elem V   V break up each word V                 V skip first elem (which is "cscope N lines")
          (parsed (reverse (cdr (reverse (mapcar '(lambda (x) (split-string x)) (cdr resp))))))
          (pathExpanded (mapcar 'gas-expand-path parsed))
          (filtered (gas-filter 'fullpath-p pathExpanded))
          )
     (if filtered (gas-cs-show-menu filtered why) (princ "Nothing Found"))
))


;; Interactive functions taken from etags.el found in std emacs dist.

;; Return a default tag to search for, based on the text at point.
(defun gas-cs-find-tag-default ()
  (save-excursion
    (while (looking-at "\\sw\\|\\s_")
      (forward-char 1))
    (if (or (re-search-backward "\\sw\\|\\s_"
                                (save-excursion (beginning-of-line) (point))
                                t)
            (re-search-forward "\\(\\sw\\|\\s_\\)+"
                               (save-excursion (end-of-line) (point))
                               t))
        (progn (goto-char (match-end 0))
               (buffer-substring (point)
                                 (progn (forward-sexp -1)
                                        (while (looking-at "\\s'")
                                          (forward-char 1))
                                        (point))))
      nil)))

;; Read a tag name from the minibuffer with defaulting and completion.
(defun gas-cs-find-tag-tag (string)
  (let* ((default (gas-cs-find-tag-default))
         (spec (if default (format "%s(default %s) " string default) string) ))
;       (princ default)
       (read-string spec nil nil default)


      ))

(defvar last-tag nil
  "Last tag found by \\[find-tag].")


;; Get interactive args for find-tag{-noselect,-other-window,-regexp}.
(defun gas-cs-interactive (prompt &optional no-default)
    (list (if no-default
              (read-string prompt)
            (gas-cs-find-tag-tag prompt))))

(defvar gas-cs-find-tag-history nil)

; Return an anonymous fn which calls the result parser with a string saying what we are looking for
(defun gas-cs-parse-c(why) (list 'lambda (list 'x 'y) (list 'gas-cs-parse 'x 'y why)))

(defun cs-hist-clear() (interactive)
  (setq gasCsMenuHist nil)
  (setq gasCsMenuPos 0)
)

(defun cs-hist-next() (interactive)
  (setq gasCsMenuPos (if (= (safe-length gasCsMenuHist) (+ gasCsMenuPos 1)) 0 (+ gasCsMenuPos 1) ))
  (cs-hist-disp)
)

(defun cs-hist-prev() (interactive)
  (setq gasCsMenuPos (- (if (= 0 gasCsMenuPos) (safe-length gasCsMenuHist) gasCsMenuPos) 1))
  (cs-hist-disp)
)

(defun cs-hist-disp()
    (BsSetChoices (nth gasCsMenuPos gasCsMenuHist))
    (BsChoose))

(defun cs-sym (sym)
  "find c symbol"
  (interactive (gas-cs-interactive "Sym: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
    (tq-enqueue gasCsTq (concat "0" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "References to symbol %s.\n" sym)))
    (princ gasErrCsNotStarted))
)

(defun cs-def (sym)
  "find global def"
  (interactive (gas-cs-interactive "Def: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
    (tq-enqueue gasCsTq (concat "1" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "Global definitions of %s.\n" sym)) )
    (princ gasErrCsNotStarted))
)

(defun cs-called (sym)
  "find functions called by this function"
   (interactive (gas-cs-interactive "Function: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
    (tq-enqueue gasCsTq (concat "2" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "Called by %s.\n" sym)))
    (princ gasErrCsNotStarted))
)


(defun cs-caller (sym)
  "find functions that call this function"
   (interactive (gas-cs-interactive "Function: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
     (tq-enqueue gasCsTq (concat "3" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "Callers of %s.\n" sym)))
      (princ gasErrCsNotStarted))
)

(defun cs-text (sym)
  "find this text"
   (interactive (gas-cs-interactive "text: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
     (tq-enqueue gasCsTq (concat "4" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "Contains the text '%s'.\n" sym)))
      (princ gasErrCsNotStarted))
)

(defun cs-grep (sym)
  "find this pattern"
   (interactive (gas-cs-interactive "egrep: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
      (tq-enqueue gasCsTq (concat "6" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "Contains the pattern '%s'.\n" sym)))
      (princ gasErrCsNotStarted))
)


(defun cs-file (sym)
  "find this file"
   (interactive (gas-cs-interactive "File: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
      (tq-enqueue gasCsTq (concat "7" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "Filenames containing %s.\n" sym))  )
      (princ gasErrCsNotStarted))
)

(defun cs-include (sym)
  "find files including this file"
   (interactive (gas-cs-interactive "File: " nil))
  (BsSetTgtWind (selected-window))
  (if (boundp 'gasCsp)
        (tq-enqueue gasCsTq (concat "8" sym "\n") gas-cs-end-regexp  nil (gas-cs-parse-c (format "Files including %s.\n" sym))  )
      (princ gasErrCsNotStarted))
)


(defun cs-hide()
  "Close the cscope window"
  (interactive)
  (BsHide)
)


(defun gas-sentinel (process event)
  (if (string= (substring event 0 6) "exited")
      (cs-do (makunbound 'gasCsp)
          (if gasCsTq (princ "cscope process exited.  Rerun M-x cs.")
                      (princ (format "  Sorry! cscope exited with:  %s  Cmd: '%s'" event gasCsExec))
            )
       (setq gasCsTq nil))
      (princ
     (format "Process: %s had the event `%s'" process event))


))

