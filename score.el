(cua-mode t) ; MIMICKING MODERNISM
(global-set-key (kbd "C-<tab>") 'other-window)
(global-set-key (kbd "C-s") 'save-buffer)
(global-set-key (kbd "S-C-s") 'write-file)
(global-set-key (kbd "C-o") 'find-file)
(global-set-key (kbd "C-w") 'kill-buffer)
(global-set-key (kbd "S-C-z") 'undo-redo)

;; SEARCHING WITH C-f AND CYCLING WITH ARROW KEYS ;;
(global-set-key (kbd "C-f") 'isearch-forward)
(define-key isearch-mode-map (kbd "<right>") 'isearch-repeat-forward)
(define-key isearch-mode-map (kbd "<left>") 'isearch-repeat-backward)

;; DUPLICATE A LINE AND KEEP THE CURSOR POSITION ;;
(set-default 'truncate-lines t)
  (defun duplicate-line (arg)
  "gotta find the link i found this"
  (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point))) eol)
    (save-excursion
      (end-of-line)
      (setq eol (point))
      (let ((line (buffer-substring bol eol)) (buffer-undo-list t) (count arg))
        (while (> count 0) (newline) (insert line) (setq count (1- count))))
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  (next-line arg))

;; COLUMNS!
(setq pointer nil)
(defun aling (BEG END) ; CSOUND SCORE ALIGN ; https://stackoverflow.com/questions/4218099/emacs-how-to-format-a-block-of-text-into-spreadsheet-form
  (interactive "r")
  (align-regexp BEG END "\\(\\s-*\\)\\s-+" 0 1 t)) ; before it was "1 1 t" at the end (i changed GROUP to 0)

(defun csound-score-align ()
   (interactive)
   (save-excursion
     (mark-paragraph)
     (call-interactively 'aling)))

;; START, RECORD AND STOP, WITHOUT EVOKING THE TERMINAL ;;
(defun csound-start ()
  (interactive)
  (save-buffer)
  (call-process "csound" nil 0 nil "-odac" "/home/luqtas/Desktop/projects/qob/Csound/header.orc" (file-name-with-extension buffer-file-name ".sco")))

(defun csound-record-wav ()
   (interactive)
   (save-buffer)
   (call-process "csound" nil 0 nil "-o" (file-name-with-extension buffer-file-name ".wav ") "/home/luqtas/Desktop/projects/qob/Csound/header.orc" (file-name-with-extension buffer-file-name ".sco") "-W"))
; -o noise.wav -W ; for file output any platform

(defun csound-record-ogg ()
   (interactive)
   (save-buffer)
   (call-process "csound" nil 0 nil "-o" (file-name-with-extension buffer-file-name ".ogg ") "--ogg" "/home/luqtas/Desktop/projects/qob/Csound/header.orc" (file-name-with-extension buffer-file-name ".sco")))
; -o noise.ogg --ogg

(defun csound-stop ()
   (interactive)
   (call-process "killall" nil 0 nil "csound"))

;; CUSTOM PLAY FUNCTIONS ;;
(defun play-from-cursor ()
  (interactive)
  (point-to-register 1)
  (move-beginning-of-line 1) (right-word 2) (setq a (thing-at-point 'number 'no-properties))
  (isearch-resume "advance statement" nil nil nil nil nil) (isearch-exit) (next-line) (move-beginning-of-line 1) (right-word 3) (kill-line) (insert " ") (insert (number-to-string a))
  (jump-to-register 1)
  (csound-start))

(defun play-from-zero ()
  (interactive)
  (point-to-register 2)
  (isearch-resume "advance statement" nil nil nil nil nil) (isearch-exit) (next-line) (move-beginning-of-line 1) (right-word 3) (kill-line) (insert " ") (insert "0")
  (jump-to-register 2)
  (csound-start))

(defvar play-from-value nil)
(defun play-from-value ()
  (interactive)
  (point-to-register 3)
  (setq play-from-value (read-string ""))
  (isearch-resume "advance statement" nil nil nil nil nil) (isearch-exit) (next-line) (move-beginning-of-line 1) (right-word 3) (kill-line) (insert " ") (insert play-from-value)
  (jump-to-register 3)
  (csound-start))

(defun which-play-value ()
  (interactive)
  (message play-from-value))

(defun csound-header-edit ()
  (interactive)
  (find-file "/home/luqtas/Desktop/projects/qob/Csound/header.orc"))

;; ORGANIZING THE SCORE ;;
(defun str-sum ()
  (interactive)
  (move-beginning-of-line 0) (right-word 3)
  (setq a (thing-at-point 'number 'no-properties))
  (next-line) (move-beginning-of-line 1) (right-word 2)
  (setq b (thing-at-point 'number 'no-properties))
  (backward-kill-word 1)
  (insert (number-to-string (+ a b))))

(defun quantifier ()
  (interactive)
  (save-excursion
    (move-beginning-of-line 0) (right-word 2) (setq a (thing-at-point 'number 'no-properties))
    (right-word) (setq b (thing-at-point 'number 'no-properties))
    (next-line) (move-beginning-of-line 1) (right-word) (right-word)
    (backward-kill-word 1)
    (insert (number-to-string (+ a b)))))

(defun csound-stab ()
  (interactive)
  (quantifier))

(defun csound-cstab ()
  (interactive)
  (quantifier)
  (next-line))
  (defun csound-duplicate-line (arg) (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point))) eol)
    (save-excursion (end-of-line) (setq eol (point))
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t) (count arg))
        (while (> count 0) (newline) (insert line) (setq count (1- count))))
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  (next-line arg)
   (point-to-register 0)
   (str-sum)
   (mark-paragraph) (call-interactively 'aling) (jump-to-register 0))

(defvar custom-duplicate-line-value nil)
(defun custom-str-sum ()
  (interactive)
  (if custom-duplicate-line-value
      (setq custom-duplicate-line-value (string-to-number(read-string "" (number-to-string custom-duplicate-line-value))))
      (setq custom-duplicate-line-value (string-to-number(read-string ""))))
  (move-beginning-of-line 0) (right-word 3)
  (next-line) (move-beginning-of-line 1) (right-word 2)
  (setq b (thing-at-point 'number 'no-properties))
  (backward-kill-word 1)
  (insert (number-to-string (+ b custom-duplicate-line-value))))

(defun custom-str-sum-repeat ()
  (interactive)
  (move-beginning-of-line 0) (right-word 3)
  (next-line) (move-beginning-of-line 1) (right-word 2)
  (setq b (thing-at-point 'number 'no-properties))
  (backward-kill-word 1)
  (insert (number-to-string (+ b custom-duplicate-line-value))))

(defun csound-custom-duplicate-line (arg) (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point))) eol)
    (save-excursion (end-of-line) (setq eol (point))
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t) (count arg))
        (while (> count 0) (newline) (insert line) (setq count (1- count))))
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  (next-line arg)
  (point-to-register 4)
  (custom-str-sum)
  (mark-paragraph) (call-interactively 'aling) (jump-to-register 4))

(defun csound-custom-duplicate-line-repeat (arg) (interactive "*p")
  (setq buffer-undo-list (cons (point) buffer-undo-list))
  (let ((bol (save-excursion (beginning-of-line) (point))) eol)
    (save-excursion (end-of-line) (setq eol (point))
      (let ((line (buffer-substring bol eol))
            (buffer-undo-list t) (count arg))
        (while (> count 0) (newline) (insert line) (setq count (1- count))))
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list))))
  (next-line arg)
  (point-to-register 4)
  (custom-str-sum-repeat)
  (mark-paragraph) (call-interactively 'aling) (jump-to-register 4))


(define-minor-mode csound-mode
  " compoooosing "
  nil ; INITIAL VALUE 
  " csound" ; INDICATOR
  :keymap (let ((map (make-sparse-keymap)))
			;; > b y o u ' ( d n g v q
			;; 0 1 2 3 4 , . 5 6 7 8 9
			;; ~ ^ # * & - ? @ = + $ /
            (define-key map (kbd "<TAB>") #'csound-score-align)
            (define-key map (kbd "<backtab>") #'csound-stab)
            (define-key map (kbd "C-S-<iso-lefttab>") #'csound-cstab)
            (define-key map (kbd "n") #'csound-start)
			(define-key map (kbd "o") #'csound-stop)
			(define-key map (kbd "N w") #'csound-record-wav)
			(define-key map (kbd "N o") #'csound-record-ogg)
			(define-key map (kbd "q") #'csound-header-edit)
			(define-key map (kbd "d") #'duplicate-line)
			(define-key map (kbd "g") #'csound-duplicate-line)
			(define-key map (kbd "G") #'csound-custom-duplicate-line)
			(define-key map (kbd "C-w") #'csound-custom-duplicate-line-repeat)
			(define-key map (kbd "v") #'play-from-cursor)
			(define-key map (kbd "y") #'play-from-value)
			(define-key map (kbd "Y") #'which-play-value)
			(define-key map (kbd "b") #'play-from-zero)
			(define-key map (kbd "u") (lambda () (interactive) (insert "1.02197503906")))
			(define-key map (kbd "C-c r") (lambda () (interactive) (insert ";12 STR DUR AMP NOTE KC1 KC2 VDEPTH STR VRATE END EDO REPEAT BASE")))
			(define-key map (kbd "C-c g") (lambda () (interactive) (insert ";9 STR DUR AMP NOTE PLK PICK REFL EDO REPEAT BASE")))
			(define-key map (kbd "C-c b") (lambda () (interactive) (insert ";11 STR DUR AMP NOTE PRES RAT STR VIBF END VAMP EDO REPEAT BASE")))
			(define-key map (kbd "C-c y") (lambda () (interactive) (insert ";10 STR DUR AMP NOTE HRD POS STR VIBF END EDO REPEAT BASE")))
            map)
  (if csound-mode
      (progn
        (modify-syntax-entry ?\. "w" (syntax-table))
		(visual-line-mode -1)
		(setq truncate-lines t)
		(harvest-all-columns-to-cycle-list))
    (progn
      (modify-syntax-entry ?\. "." (syntax-table))
	  (visual-line-mode 1)
	  (setq truncate-lines nil))))


(add-hook 'latex-mode-hook 'csound-mode) ; ASSOCIATE MAJOR MODE WITH A MINOR MODE
;; ASSOCIATE FILES WITH A MODE ;;
(add-to-list 'auto-mode-alist '("\\.sco\\'" . latex-mode)) ;; latex with Csound as otherwise the score-align doesn't work
