
(defvar bs-choice-list nil)
(defvar bs-current-buffer nil)
(defvar bs-current-window nil)
(defvar bs-my-buffer nil)


(defvar bs-mode-map (make-sparse-keymap))   ; Create a mode-specific keymap.

(define-key bs-mode-map "q" 'bs-quit)
(define-key bs-mode-map "g" 'bs-goto)
(define-key bs-mode-map " " 'bs-view)
(define-key bs-mode-map "\r" 'bs-goto)
(define-key bs-mode-map [double-mouse-1] 'bs-goto)
(define-key bs-mode-map [mouse-2] 'bs-mouse-goto)


;; Just execute all args & return true
(defun bs-do (&rest args) t)


(defun BsInit(name &optional keyMap)
 ; For when we return contexts
 ; (let ((buf (generate-new-buffer name)) (buffer-read-only))
 (let ((buf (get-buffer-create name)) (buffer-read-only))
    (save-current-buffer
      (setq bs-my-buffer buf)
      (set-buffer buf)
      (Bs-mode keyMap))
      (list buf))
  )

(defun BsSetChoices(clist)
;  (princ clist)
  (let ((buf bs-my-buffer) (buffer-read-only))
       (save-current-buffer
    (set-buffer buf)
    (erase-buffer)
    (setq bs-choice-list clist)
    (mapcar '(lambda (x) (princ (car x) buf)) clist)
    (goto-line 1)
    t
))

)

(defun BsSetCurBuf()
  (princ "setting the window")
  (setq bs-current-window (selected-window))
  (setq bs-current-buffer (current-buffer))
)

(defun BsSetTgtWind(wind)
   (let ((buf (window-buffer wind)))
         (if (not (eq bs-my-buffer buf)) (setq bs-current-buffer (current-buffer)) t)))


(defun BsChoose()
;    (bs-do (princ bs-my-buffer) (princ (current-buffer)) (princ bs-current-buffer)
   (if (window-live-p bs-current-window) nil (setq bs-current-window nil))
   (if (eq bs-current-window nil) (BsSetCurBuf) nil)
   (princ (current-buffer))
   (princ bs-my-buffer)
   (if (not (equal (current-buffer) bs-my-buffer)) (BsSetCurBuf) nil)

   (let* ((buf bs-my-buffer) (wind (if (get-buffer-window buf) (get-buffer-window buf) (split-window-horizontally))))
      (set-window-buffer wind buf)
      (select-window wind)
      (enlarge-window-horizontally (- 60 (window-width wind)))
))

(defun BsHide()
  (delete-windows-on bs-my-buffer)
 )

(defvar bs-mode-abbrev-table nil
  "Abbrev table used while in buffer select mode.")

(define-abbrev-table 'bs-mode-abbrev-table ())




;; Create mode-specific tables.
(defvar bs-mode-syntax-table nil
  "Syntax table used while in buffer select mode.")

(if bs-mode-syntax-table
    ()              ; Do not change the table if it is already set up.
  (setq bs-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\" ".   " bs-mode-syntax-table)
  (modify-syntax-entry ?\\ ".   " bs-mode-syntax-table)
  (modify-syntax-entry ?' "w   " bs-mode-syntax-table))

(defun bs-quit()
  (interactive)
  (BsHide)
  (kill-buffer bs-my-buffer)
  (set-buffer bs-current-buffer)
)

(defun bs-goto()
  (interactive)
;  (princ "\nbs-goto:") (princ bs-my-buffer) (princ (current-buffer)) (princ bs-current-buffer)
  (let* ( (line (- (count-lines 1 (+ 1 (point))) 1) ) (choice (nth line bs-choice-list)))
;    (princ (cdr choice))
;    (princ "goto")
;    (set-buffer bs-current-buffer)
    (select-window bs-current-window)
    (apply (cdr choice) nil)
    )
)

(defun bs-view()
  (interactive)
;  (princ "\nbs-goto:") (princ bs-my-buffer) (princ (current-buffer)) (princ bs-current-buffer)
  (let* ( (line (- (count-lines 1 (+ 1 (point))) 1) ) (choice (nth line bs-choice-list))
          (curwin (selected-window))
          )
;    (princ (cdr choice))
;    (princ "goto")
    (select-window bs-current-window)
    (apply (cdr choice) nil)
    (select-window curwin)
    )
)



(defun bs-mouse-goto(click) (interactive "e")
  (set-buffer (window-buffer (posn-window (event-start click))))
  (if (window-live-p bs-current-window) nil (BsSetCurBuf))

  (let* ((posn (event-start click)) (line (- (count-lines 1 (+ 1 (posn-point posn))) 1) ) (choice (nth line bs-choice-list)))
;    (set-buffer bs-current-buffer)
    (select-window bs-current-window)
    (apply (cdr choice) nil)
    )
)


(defun Bs-mode(&optional keyMap)
  "special mode to select an item in a buffer\nq quit\n<cr> select item"
  (interactive)
  (kill-all-local-variables)
  (if keyMap (set-keymap-parent bs-mode-map keyMap))
;  (use-local-map (if keyMap (cons keyMap bs-mode-map) bs-mode-map))
  (use-local-map  bs-mode-map)
  (setq local-abbrev-table bs-mode-abbrev-table)
  (set-syntax-table bs-mode-syntax-table)
;  (make-local-variable 'paragraph-separate)
;  (setq paragraph-separate paragraph-start)
  (setq mode-name "Select")
  (setq major-mode 'Bs-mode)
  (run-hooks 'Bs-mode-hook)

)




;(bt-one)
;(BsTest)


;(BsInit "test")

;(setq alist-of-colors '(("one\n" . 1) ("two" . 2)))
;      '((rose . red) (lily . white)  (buttercup . yellow)))

;(BsSetChoices (list (get-buffer "test")) alist-of-colors)
