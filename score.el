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

;; ORGANIZING THE SCORE ;; by google.gemini
(defun csound-score-align-region (beg end)
  "Align Csound score columns in the active region."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\s-+" 1 1 t))
(defun csound-score-align ()
  "Align the current paragraph of Csound code."
  (interactive)
  (save-excursion
    (mark-paragraph)
    (csound-score-align-region (region-beginning) (region-end))))
(defun csound-smart-duplicate (arg)
  "Duplicate line ARG times, using the duration (p3) as the increment.
Jumps to the same position on the last duplicated line."
  (interactive "*p")
  (let* ((col (current-column)) ;; Save cursor position
         (bol (line-beginning-position))
         (eol (line-end-position))
         (line-text (buffer-substring bol eol))
         ;; Get p2 (start) and p3 (duration)
         (p2 (csound--get-number-at-column 2))
         (p3 (csound--get-number-at-column 3)))    
    ;; Create the copies
    (dotimes (i arg)
      (let ((new-p2 (+ p2 (* (1+ i) p3))))
        (end-of-line)
        (newline)
        (insert line-text)
        (csound--replace-number-at-column 2 new-p2)))
    ;; Clean up the formatting
    (csound-score-align)    
    ;; The Jump: Move to the last line and restore the column
    (forward-line 0)
    (move-to-column col)
    (message "Smart-duplicated %d times (incremented by %s)" arg p3)))
(defvar csound-last-increment 0.5
  "Global variable to store the last used increment for duplication.")
(defun csound-custom-duplicate (arg &optional use-last)
  "Duplicate line ARG times and move cursor to the same position on the last copy."
  (interactive "p")
  (let* ((col (current-column)) ;; Save the horizontal position
         (line-text (buffer-substring (line-beginning-position) (line-end-position)))
         ;; 1. Determine the increment
         (inc (if use-last
                  csound-last-increment
                (setq csound-last-increment 
                      (read-number (format "Increment (default %s): " csound-last-increment) 
                                   csound-last-increment))))
         ;; 2. Get current p2 (assumes column 2 / word index 2)
         (p2 (csound--get-number-at-column 2)))
    ;; 3. Perform duplication
    (dotimes (i arg)
      (let ((new-p2 (+ p2 (* (1+ i) inc))))
        (end-of-line)
        (newline)
        (insert line-text)
        (csound--replace-number-at-column 2 new-p2)))
    ;; 4. Clean up alignment
    (csound-score-align)
    ;; 5. The Jump: Move to the last line created and restore the column
    (forward-line 0) ;; Ensure we are at the start of the last duplicated line
    (move-to-column col)
    (message "Duplicated %d times with increment: %s" arg csound-last-increment)))
(defun csound-custom-duplicate-repeat (arg)
  "Duplicate using the existing increment and jump to the last line."
  (interactive "p")
  (csound-custom-duplicate arg t))
(defun csound-recalculate-starts (beg end)
  "Recalculate p2 based on p2+p3 of the previous line.
Works on a region or a single line. Prevents infinite loops."
  (interactive "r")
  (let ((col (current-column))
        (is-region (use-region-p)))
    (save-excursion
      (if is-region
          (progn
            (goto-char beg)
            ;; move-marker ensures 'end' stays at the right place even as we insert text
            (let ((end-marker (copy-marker end)))
              (forward-line 1)
              (while (and (< (point) end-marker) (not (eobp)))
                (csound--apply-start-calc)
                (if (not (zerop (forward-line 1))) ;; Move and check for EOF
                    (goto-char (point-max))))
              (set-marker end-marker nil)))
        ;; Single line logic
        (csound--apply-start-calc)))    
    ;; Only align once at the very end to save CPU
    (csound-score-align)
    (move-to-column col)
    (message "Recalculation complete.")))
(defun csound--apply-start-calc ()
  "Internal helper to apply p2 = prev_p2 + prev_p3 to current line."
  (let (prev-p2 prev-p3 new-p2)
    (save-excursion
      (if (= (forward-line -1) 0)
          (progn
            (setq prev-p2 (csound--get-number-at-column 2))
            (setq prev-p3 (csound--get-number-at-column 3))
            (setq new-p2 (+ prev-p2 prev-p3)))
        (setq new-p2 nil))) ;; No previous line
    (when (and new-p2 (numberp new-p2))
      (csound--replace-number-at-column 2 new-p2))))

;; --- Required Helpers ---
(defun csound--get-number-at-column (col-idx)
  "Get the Nth number-like thing on the line (1-indexed for Csound).
Handles 'i1' and '.5' correctly."
  (save-excursion
    (beginning-of-line)
    ;; Find the Nth occurrence of a number (including leading dots)
    (let ((re "\\(?:\\(?1:[0-9]+\\.[0-9]*\\)\\|\\(?2:\\.[0-9]+\\)\\|\\(?3:[0-9]+\\)\\)"))
      (dotimes (_ col-idx)
        (re-search-forward re (line-end-position) t))
      (let ((val-str (match-string 0)))
        (if val-str (string-to-number val-str) 0.0)))))
(defun csound--replace-number-at-column (col-idx new-val)
  "Replace the Nth number on the line with NEW-VAL.
Works even if the line starts with 'i1'."
  (save-excursion
    (beginning-of-line)
    (let ((re "\\(?:\\(?1:[0-9]+\\.[0-9]*\\)\\|\\(?2:\\.[0-9]+\\)\\|\\(?3:[0-9]+\\)\\)"))
      (dotimes (_ col-idx)
        (re-search-forward re (line-end-position) t))
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (delete-region beg end)
        (goto-char beg)
        (insert (format "%g" new-val))))))


;; workflow for cycling through "scales" (typed notes at the score) ;;
;; 1. VARIABLES (Buffer-local so files don't mix)
(defvar-local my-column-cycles '() "Alist for full-field cycling.")
(defvar-local my-column-decimal-cycles '() "Alist for decimal-only cycling.")
(defvar-local current-field-index 0 "Stores the detected column index.")
;; 2. HELPER: The Smart Splitter (The missing function!)
(defun csound-split-line (line)
  "Split a Csound score line, keeping [...] expressions together."
  (let ((fields '())
        (start 0))
    (while (string-match "\\[[^]]+\\]\\|\\S-+" line start)
      (push (match-string 0 line) fields)
      (setq start (match-end 0)))
    (nreverse fields)))
;; 3. THE HARVEST: Scans the file for all possible values
(defun harvest-all-columns-to-cycle-list ()
  "Scan the whole document and build full-value AND decimal-only cycle lists."
  (interactive)
  (setq my-column-cycles '()
        my-column-decimal-cycles '()) 
  (save-excursion
    (goto-char (point-min))
    (forward-line 5) ;; Skip header
    (while (not (eobp))
      (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (line-fields (csound-split-line line-text)))
        (when (and line-fields (not (string-prefix-p ";" (string-trim (car line-fields)))))
          (cl-loop for val in line-fields
                   for idx from 0
                   do (unless (string= val ".")
                        ;; Full Value Harvest
                        (let ((full-list (alist-get idx my-column-cycles)))
                          (unless (member val full-list)
                            (setf (alist-get idx my-column-cycles) 
                                  (append full-list (list val)))))
                        ;; Decimal Harvest
                        (when (string-match "\\.\\([0-9]+\\)" val)
                          (let ((dec (match-string 1 val))
                                (dec-list (alist-get idx my-column-decimal-cycles)))
                            (setq dec (if (= (length dec) 1) (concat dec "0") dec))
                            (unless (member dec dec-list)
                              (setf (alist-get idx my-column-decimal-cycles) 
                                    (append dec-list (list dec))))))))))
      (forward-line 1)))
  (dolist (cell my-column-decimal-cycles)
    (setcdr cell (sort (cdr cell) 'string-lessp)))
  (message "Harvest Complete: Full fields and Decimals stored."))
;; 4. FIELD CYCLING (# and *)
(defun cycle-current-field (direction)
  "Cycles the whole field at point."
  (interactive "p")
  (let* ((line-beg (line-beginning-position))
         (orig-point (point))
         (found-idx nil))
    (save-excursion
      (beginning-of-line)
      (let ((start 0) (idx 0) (line-text (buffer-substring-no-properties line-beg (line-end-position))))
        (while (string-match "\\[[^]]+\\]\\|\\S-+" line-text start)
          (let ((m-beg (+ line-beg (match-beginning 0))) (m-end (+ line-beg (match-end 0))))
            (when (and (>= orig-point m-beg) (<= orig-point m-end)) (setq found-idx idx))
            (setq start (match-end 0) idx (1+ idx))))
        (setq current-field-index (or found-idx 0))))
    (let* ((column-list (cdr (assoc current-field-index my-column-cycles)))
           (line-fields (csound-split-line (buffer-substring-no-properties line-beg (line-end-position))))
           (current-val (nth current-field-index line-fields)))
      (if (not (and current-val column-list))
          (message "No data for Col %d." (1+ current-field-index))
        (let* ((pos (or (cl-position current-val column-list :test 'string=) -1))
               (new-pos (mod (+ pos direction) (length column-list)))
               (new-val (nth new-pos column-list)))
          (save-excursion
            (beginning-of-line)
            (let ((start 0) (count 0) (rbeg nil) (rend nil) (lt (buffer-substring-no-properties (point-at-bol) (point-at-eol))))
              (while (and (not rbeg) (string-match "\\[[^]]+\\]\\|\\S-+" lt start))
                (if (= count current-field-index)
                    (setq rbeg (+ (point-at-bol) (match-beginning 0)) rend (+ (point-at-bol) (match-end 0)))
                  (setq start (match-end 0) count (1+ count))))
              (when rbeg (delete-region rbeg rend) (goto-char rbeg) (insert new-val)
                    (when (fboundp 'csound-score-align) (csound-score-align))))))))))
;; 5. DECIMAL CYCLING (C-# and C-*)
(defun cycle-decimal-part (direction)
  "Cycles only the decimal part of the value at point."
  (interactive "p")
  (let* ((line-beg (line-beginning-position))
         (orig-point (point))
         (detected-idx 0))
    (save-excursion
      (beginning-of-line)
      (let ((start 0) (idx 0) (lt (buffer-substring-no-properties line-beg (line-end-position))))
        (while (string-match "\\[[^]]+\\]\\|\\S-+" lt start)
          (let ((mb (+ line-beg (match-beginning 0))) (me (+ line-beg (match-end 0))))
            (when (and (>= orig-point mb) (<= orig-point me)) (setq detected-idx idx))
            (setq start (match-end 0) idx (1+ idx))))))
    (let* ((decimal-list (alist-get detected-idx my-column-decimal-cycles))
           (thing (thing-at-point 'symbol)))
      (if (and thing decimal-list (string-match "\\([0-9]+\\)\\.\\([0-9]*\\)" thing))
          (let* ((int-part (match-string 1 thing))
                 (cur-dec (match-string 2 thing))
                 (cur-dec-c (if (string= cur-dec "") "00" (if (= (length cur-dec) 1) (concat cur-dec "0") cur-dec)))
                 (pos (or (cl-position cur-dec-c decimal-list :test 'string=) -1))
                 (new-pos (mod (+ pos direction) (length decimal-list)))
                 (new-dec (nth new-pos decimal-list))
                 (bounds (bounds-of-thing-at-point 'symbol)))
            (when (and new-dec bounds)
              (delete-region (car bounds) (cdr bounds))
              (insert (concat int-part "." new-dec))
              (when (fboundp 'csound-score-align) (csound-score-align))))
        (message "No decimal list for this column or not on a number.")))))


(define-minor-mode csound-mode
  " compoooosing "
  nil ; INITIAL VALUE 
  " csound" ; INDICATOR
  :keymap (let ((map (make-sparse-keymap)))
			;; > b y o u ' ( d n g v q
			;; 0 1 2 3 4 , . 5 6 7 8 9
			;; ~ ^ # * & - ? @ = + $ /
			(define-key map (kbd "&") (lambda () (interactive) (cycle-decimal-part 1)))
			(define-key map (kbd "-") (lambda () (interactive) (cycle-decimal-part -1)))  
			(define-key map (kbd "#") (lambda () (interactive) (cycle-current-field 1)))  
			(define-key map (kbd "*") (lambda () (interactive) (cycle-current-field -1))) 
            (define-key map (kbd "<TAB>") #'csound-score-align)
            (define-key map (kbd "<backtab>") #'csound-recalculate-starts)
            (define-key map (kbd "n") #'csound-start)
			(define-key map (kbd "o") #'csound-stop)
			(define-key map (kbd "N w") #'csound-record-wav)
			(define-key map (kbd "N o") #'csound-record-ogg)
			(define-key map (kbd "q") #'csound-header-edit)
			(define-key map (kbd "d") #'duplicate-line)
			(define-key map (kbd "g") #'csound-smart-duplicate)
			(define-key map (kbd "G") #'csound-custom-duplicate)
			(define-key map (kbd "C-G") #'csound-custom-duplicate-repeat)
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
