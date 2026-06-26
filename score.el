;; next ask for a function that toggles + and ^ to numbers (save the ones that used it)
;; ... but we should be able to return it! maybe it's better to have a
;; print that tells their value instead of anything?

(setq-default indent-tabs-mode nil) ; Global default: use spaces, never tabs
(add-hook 'before-save-hook 'delete-trailing-whitespace)
;; we can cleanup the buffer with
;; M-x untabify
;; M-x delete-trailing-whitespace

;; playing a .csd file ;;
(global-set-key (kbd "C-c p") 'play-csd)
(defun play-csd (&optional show-buffer)
  "Save the current buffer and play it with Csound if it's a .csd file.
With a prefix argument SHOW-BUFFER (e.g., C-u), display the output window."
  (interactive "P")
  (let ((file-path (buffer-file-name)))
    (cond
     ;; 1. Check if a valid file is visiting the buffer
     ((not file-path)
      (message "Error: Buffer is not visiting a file."))
     ;; 2. Ensure it has a .csd extension
     ((not (string-equal (file-name-extension file-path) "csd"))
      (message "Error: Not a .csd file."))
     ;; 3. Save, parse, and execute
     (t
      (when (buffer-modified-p)
        (save-buffer))

      (csound-stop)

      (let* ((buf-name "*Csound Output*")
             (proc (start-process "csound-process" buf-name "csound" "-odac" file-path)))
        (message "Csound started for %s..." (file-name-nondirectory file-path))
        ;; 4. Handle the window display logic
        (if show-buffer
            (display-buffer buf-name)
          (let ((win (get-buffer-window buf-name)))
            (when win (delete-window win)))))))))

;; START, RECORD AND STOP, WITHOUT EVOKING THE TERMINAL ;;
(defvar csound-start-time nil "Stores the epoch time when Csound started.")

;; All three execution functions route through csound--create-resolved-tempfile
;; so that ++N / +-N macros (and all other carry macros) are expanded to plain
;; numbers before Csound ever sees the score.  The original .sco buffer is
;; never touched by this process.

(defun csound-stop ()
  (interactive)
  ;; Changed '0' to 'nil' so Emacs runs this synchronously.
  ;; This prevents 'killall' from accidentally sniping the new process.
  (call-process "killall" nil nil nil "csound")
  (csound--cleanup-tempfile))

(defun csound-start ()
  (interactive)
  (csound-stop) ;; Snuff out the old instance
  (save-buffer)
  (let ((score-file (csound--create-resolved-tempfile)))
    (call-process "csound" nil 0 nil
                  "-odac"
                  "/home/luqtas/Desktop/projects/qob/Csound/header.orc"
                  score-file))
  (setq csound-start-time (float-time))
  (message "Csound started..."))

(defun csound-record-wav ()
  (interactive)
  (csound-stop) ;; Snuff out the old instance
  (save-buffer)
  (let ((score-file (csound--create-resolved-tempfile)))
    (call-process "csound" nil 0 nil
                  "-o" (file-name-with-extension buffer-file-name ".wav")
                  "/home/luqtas/Desktop/projects/qob/Csound/header.orc"
                  score-file "-W")))

(defun csound-record-ogg ()
  (interactive)
  (csound-stop) ;; Snuff out the old instance
  (save-buffer)
  (let ((score-file (csound--create-resolved-tempfile)))
    (call-process "csound" nil 0 nil
                  "-o" (file-name-with-extension buffer-file-name ".ogg")
                  "--ogg"
                  "/home/luqtas/Desktop/projects/qob/Csound/header.orc"
                  score-file)))

(defun csound-show-macro-values ()
  "Print the resolved numeric values of any macros on the current line."
  (interactive)
  (let ((col-idx 0)
        (code-end (csound--code-end-position))
        (results '()))
    (save-excursion
      (beginning-of-line)
      ;; Step through every p-field on the line
      (while (re-search-forward csound-number-regex code-end t)
        (setq col-idx (1+ col-idx))
        (let ((val-str (match-string 0)))
          ;; If it's a macro, resolve it and store the string
          (when (csound-is-macro-p val-str)
            (push (format "p%d[%s] = %g"
                          col-idx
                          val-str
                          (csound--get-number-at-column col-idx))
                  results)))))
    ;; Display the results
    (if results
        (message "Macros: %s" (mapconcat #'identity (nreverse results) "  |  "))
      (message "No macros found on the current line."))))

;; CUSTOM PLAY FUNCTIONS ;;
(defun log-csound-start ()
  "Helper to record the current time when playback begins."
  (setq csound-start-time (float-time)))

(defun csound--set-advance-start (value)
  "Edit the advance-statement line to use VALUE as the start time.
Uses a silent search-forward — no isearch UI, no cursor flicker.
Finds the line below the 'advance statement' comment and rewrites
the fourth field (the start time) in place."
  (save-mark-and-excursion
    (goto-char (point-min))
    (when (search-forward "advance statement" nil t)
      (forward-line 1)
      (let ((bol (line-beginning-position))
            (eol (line-end-position)))
        (replace-regexp-in-region
         "\\(\\(?:\\S-+\\s-+\\)\\{3\\}\\).*$"
         (concat "\\1" (format "%g" value))
         bol eol)))))

(defun csound--get-advance-start ()
  "Return the 4th column value of the advance statement as a float.
Returns 0.0 if the line cannot be found or is malformed."
  (save-mark-and-excursion
    (goto-char (point-min))
    (if (search-forward "advance statement" nil t)
        (progn
          (forward-line 1)
          (let ((cols (split-string (thing-at-point 'line t) "[ \t]+" t)))
            (if (>= (length cols) 4)
                (string-to-number (nth 3 cols))
              0.0)))
      0.0)))

(defun play-from-cursor ()
  (interactive)
  ;; csound--get-number-at-column resolves ++N / +-N as well
  (setq a (csound--get-number-at-column 2))
  (csound--set-advance-start a)
  (log-csound-start)
  (csound-start))

(defun play-from-zero ()
  (interactive)
  (setq a 0)
  (csound--set-advance-start 0)
  (log-csound-start)
  (csound-start))

(defvar play-from-value nil)
(defun play-from-value ()
  (interactive)
  (setq play-from-value (read-string "Start Value (Number or Macro): "))
  (setq a
        (cond
         ;; . or ^ → p2 of the line above
         ((or (string= play-from-value ".") (string= play-from-value "^"))
          (save-excursion
            (if (= (forward-line -1) 0) (csound--get-number-at-column 2) 0.0)))
         ;; + → end of the note on the line above (p2 + p3)
         ((string= play-from-value "+")
          (save-excursion
            (if (= (forward-line -1) 0)
                (+ (csound--get-number-at-column 2)
                   (csound--get-number-at-column 3))
              0.0)))
         ;; ^+N / ^-N → p2 of line above ± N
         ((string-match "^\\^\\([-+]\\)\\([0-9]+\\.[0-9]*\\|\\.[0-9]+\\|[0-9]+\\)$"
                        play-from-value)
          (let ((sign (match-string 1 play-from-value))
                (num  (string-to-number (match-string 2 play-from-value))))
            (save-excursion
              (if (= (forward-line -1) 0)
                  (if (string= sign "+")
                      (+ (csound--get-number-at-column 2) num)
                    (- (csound--get-number-at-column 2) num))
                0.0))))
         ;; ++N / +-N → (p2 + p3) of line above ± N
         ((string-match "^\\+\\([-+]\\)\\([0-9]+\\.[0-9]*\\|\\.[0-9]+\\|[0-9]+\\)$"
                        play-from-value)
          (let ((sign (match-string 1 play-from-value))
                (num  (string-to-number (match-string 2 play-from-value))))
            (save-excursion
              (if (= (forward-line -1) 0)
                  (let ((base (+ (csound--get-number-at-column 2)
                                 (csound--get-number-at-column 3))))
                    (if (string= sign "+") (+ base num) (- base num)))
                0.0))))
         ;; Plain number
         (t (string-to-number play-from-value))))
  (csound--set-advance-start a)
  (log-csound-start)
  (csound-start))

(defun csound-stop-and-log ()
  "Stops the Csound process, calculates elapsed time, and resets the timer."
  (interactive)
  (if (fboundp 'csound-stop)
      (csound-stop)
    (kill-process "csound"))
  (if csound-start-time
      (let* ((stop-time        (float-time))
             (elapsed-seconds  (- stop-time csound-start-time))
             (start-offset     (csound--get-advance-start))
             (total-score-time (+ start-offset elapsed-seconds)))
        (message "Stopped at score time: %.2f seconds (Offset: %.2f + Elapsed: %.2f)"
                 total-score-time start-offset elapsed-seconds)
        (setq csound-start-time nil))
    (message "Csound stopped. No active timer to reset.")))

(defun csound-stop-and-track ()
  "Stops the Csound process, calculates elapsed time, and moves to the playing note."
  (interactive)
  (if (fboundp 'csound-stop)
      (csound-stop)
    (kill-process "csound"))

  (if csound-start-time
      (let* ((stop-time        (float-time))
             (elapsed-seconds  (- stop-time csound-start-time))
             (start-offset     (csound--get-advance-start))
             (total-score-time (+ start-offset elapsed-seconds))
             (saved-col        (current-column)))

        ;; Hunt for the note in the current paragraph
        (let ((target-pos (csound--find-stop-line total-score-time)))
          (if target-pos
              (progn
                ;; Jump to the line and restore horizontal placement
                (goto-char target-pos)
                (move-to-column saved-col)
                (message "Stopped at %.2fs. Jumped to active note." total-score-time))
            (message "Stopped at %.2fs. No matching notes in paragraph." total-score-time)))

        (setq csound-start-time nil))
    (message "Csound stopped. No active timer to reset.")))

(defun print-start-value ()
  "Print the 4th column of the advance statement. (Does not trigger Csound)."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (if (search-forward "; advance statement ;" nil t)
        (progn
          (forward-line 1)
          (let ((cols (split-string (thing-at-point 'line t) "[ \t]+" t)))
            (if (>= (length cols) 4)
                (message "Advance statement value: %s" (nth 3 cols))
              (error "The advance statement does not have 4 columns."))))
      (error "Could not find '; advance statement ;' in the buffer."))))

(defun play-from-cursor ()
  "Modify the advance statement using column 2 of the CURRENT line and start Csound."
  (interactive)
  (let ((a (csound--get-number-at-column 2)))
    (if a
        (progn
          (csound--set-advance-start a)
          (log-csound-start)
          (csound-start)
          (message "Updated advance statement to %s and started Csound!" a))
      (error "Could not harvest a valid value from column 2."))))

(defun play-from-point ()
  "Prompt for N, search for 'pN', and use its line's column 2 to start Csound."
  (interactive)
  (let (a)
    (save-excursion
      (let* ((num (read-number "Enter number: "))
             (target (format "p%d" num)))
        (goto-char (point-min))
        (if (search-forward target nil t)
            (setq a (csound--get-number-at-column 2))
          (error "Could not find '%s' in the buffer." target))))
    (if a
        (progn
          (csound--set-advance-start a)
          (log-csound-start)
          (csound-start)
          (message "Updated advance statement to %s and started Csound!" a))
      (error "Could not harvest a valid value from column 2."))))

(defun csound-header-edit ()
  (interactive)
  (find-file "/home/luqtas/Desktop/projects/qob/Csound/header.orc"))

;; GEMININI STUFF ;;
(defun csound-score-align-region (beg end)
  "Align Csound score columns in the active region."
  (interactive "r")
  (align-regexp beg end "\\(\\s-*\\)\\s-+" 1 1 t))

(defun csound-score-align ()
  "Align the current paragraph of Csound code."
  (interactive)
  (save-mark-and-excursion
    (mark-paragraph)
    (csound-score-align-region (region-beginning) (region-end))
    (untabify (region-beginning) (region-end))
    (delete-trailing-whitespace)))

;; DUPLICATION ;;
(defun csound-smart-duplicate (arg)
  "Duplicate line ARG times, using the duration (p3) as the increment.
Jumps to the same position on the last duplicated line.
Any macro in p2 on the source line is overwritten with a real number on
each copy (force=t), so the duplicates always have explicit start times."
  (interactive "*p")
  (let* ((col       (current-column))
         (bol       (line-beginning-position))
         (eol       (line-end-position))
         (line-text (buffer-substring bol eol))
         (p2        (csound--get-number-at-column 2))
         (p3        (csound--get-number-at-column 3)))
    (dotimes (i arg)
      (let ((new-p2 (+ p2 (* (1+ i) p3))))
        (end-of-line)
        (newline)
        (insert line-text)
        ;; force=t: stamp an explicit number even if source had a macro
        (csound--replace-number-at-column 2 new-p2 t)))
    (csound-score-align)
    (forward-line 0)
    (move-to-column col)
    (message "Smart-duplicated %d times (incremented by %s)" arg p3)))

(defvar csound-last-increment 0.5
  "Global variable to store the last used increment for duplication.")

(defun csound-custom-duplicate (arg &optional use-last)
  "Duplicate line ARG times and move cursor to the same position on the last copy.
Any macro in p2 on the source line is overwritten with a real number on
each copy (force=t), so the duplicates always have explicit start times."
  (interactive "p")
  (let* ((col       (current-column))
         (line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (inc       (if use-last
                        csound-last-increment
                      (setq csound-last-increment
                            (read-number (format "Increment (default %s): "
                                                 csound-last-increment)
                                         csound-last-increment))))
         (p2 (csound--get-number-at-column 2)))
    (dotimes (i arg)
      (let ((new-p2 (+ p2 (* (1+ i) inc))))
        (end-of-line)
        (newline)
        (insert line-text)
        ;; force=t: stamp an explicit number even if source had a macro
        (csound--replace-number-at-column 2 new-p2 t)))
    (csound-score-align)
    (forward-line 0)
    (move-to-column col)
    (message "Duplicated %d times with increment: %s" arg csound-last-increment)))

(defun csound-custom-duplicate-repeat (arg)
  "Duplicate using the existing increment and jump to the last line."
  (interactive "p")
  (csound-custom-duplicate arg t))

;; RECALCULATE STARTS ;;
(defun csound-recalculate-starts ()
  "Recalculate p2 (start times) sequentially.
- If a region is active, process the region.
- Otherwise, process from the current line to the end of the paragraph.

Lines whose p2 is a macro (+, ., ^, ^+N, ^-N, ++N, +-N) are left intact;
their RESOLVED value is still used as the base for the lines that follow."
  (interactive)
  (let* ((use-region (use-region-p))
         (beg (if use-region (region-beginning) (line-beginning-position)))
         (end (if use-region
                  (region-end)
                (save-excursion (forward-paragraph) (point)))))
    (save-excursion
      (goto-char beg)
      (beginning-of-line)
      (let ((prev-p2 (csound--get-number-at-column 2))
            (prev-p3 (csound--get-number-at-column 3)))
        (forward-line 1)
        (while (and (not (eobp)) (< (point) end))
          (when (and prev-p2 prev-p3)
            (let ((expected-start (+ prev-p2 prev-p3)))
              ;; csound--replace-number-at-column shields macros by default,
              ;; so lines with ++N / +-N etc. are not overwritten.
              (csound--replace-number-at-column 2 expected-start)))
          ;; Always use the RESOLVED value for the next iteration,
          ;; regardless of whether this line has a macro or a plain number.
          (setq prev-p2 (csound--get-number-at-column 2))
          (setq prev-p3 (csound--get-number-at-column 3))
          (forward-line 1)))))
  (when (fboundp 'csound-score-align)
    (csound-score-align)))

;; --- Required Helpers ---

(defun csound--find-stop-line (stop-time)
  "Finds the best matching note line in the current paragraph for STOP-TIME.
Returns the buffer position of the line, or nil if none match."
  (let ((para-beg (save-excursion (backward-paragraph) (point)))
        (para-end (save-excursion (forward-paragraph) (point)))
        (best-inside nil)
        (best-fallback nil)
        (max-end -1.0))
    (save-excursion
      (goto-char para-beg)
      (while (< (point) para-end)
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (trimmed (string-trim line)))
          ;; Only parse active i-statements
          (when (and (not (string-empty-p trimmed))
                     (not (string-prefix-p ";" trimmed))
                     (string-match-p "^[iI]" trimmed))
            (let* ((p2 (csound--get-number-at-column 2))
                   (p3 (csound--get-number-at-column 3))
                   (end (+ p2 p3)))
              ;; Only consider notes that have started before or right at the stop-time
              (when (<= p2 stop-time)

                ;; 1. The Active Match: stop-time falls inside this note's duration
                ;; (We use > instead of >= so if a note ended EXACTLY when you stopped,
                ;; it prioritizes the note starting on that exact second instead).
                (when (> end stop-time)
                  (unless best-inside
                    (setq best-inside (line-beginning-position)))) ;; Keep the FIRST one we find

                ;; 2. The Fallback Match: the note that ended closest to the stop-time
                (when (>= end max-end)
                  (setq max-end end)
                  (setq best-fallback (line-beginning-position)))))))
        (forward-line 1)))
    ;; Return the active note if we found one; otherwise, return the best fallback
    (or best-inside best-fallback)))

(defun csound--code-end-position ()
  "Return the buffer position of the first ';' on the line, or `line-end-position` if none."
  (save-excursion
    (let ((eol (line-end-position)))
      (beginning-of-line)
      (if (search-forward ";" eol t)
          (1- (point))
        eol))))

;; The canonical regex for any Csound score "value" field.
;; Alternatives are ordered from most-specific to least-specific so the
;; engine greedily picks the right one:
;;   1. ++N / +-N  (end-of-note offset macro)   [NEW]
;;   2. ^+N / ^-N  (same-column offset macro)
;;   3. float / int with optional leading sign
;;   4. standalone . ^ +  (carry macros)
(defconst csound-number-regex
  (concat "\\(?:"
          ;; 1. ++N or +-N  — note: \\+[+-] matches exactly two chars ++ or +-
          "\\+[+-]\\(?:[0-9]+\\.[0-9]*\\|\\.[0-9]+\\|[0-9]+\\)"
          "\\|"
          ;; 2. ^+N or ^-N
          "\\^[-+]\\(?:[0-9]+\\.[0-9]*\\|\\.[0-9]+\\|[0-9]+\\)"
          "\\|"
          ;; 3. plain number, optional sign
          "-?[0-9]+\\.[0-9]*\\|-?\\.[0-9]+\\|-?[0-9]+"
          "\\|"
          ;; 4. standalone carry macros
          "[+.^]"
          "\\)")
  "Regex matching any Csound score value: numbers and all carry/offset macros.
Covers ++N, +-N, ^+N, ^-N, +, ., ^, signed/unsigned floats and ints.")

(defun csound-is-macro-p (val-str)
  "Returns t if VAL-STR is a Csound carry or offset macro.
Recognized: '.' '^' '+' '^+N' '^-N' '++N' '+-N'."
  (or (string= val-str ".")
      (string= val-str "+")
      (string= val-str "^")
      (string-match-p "^\\^[-+]" val-str)   ; ^+N / ^-N
      (string-match-p "^\\+[+-]" val-str))) ; ++N / +-N  [NEW]

(defun csound-get-nth-val-string (n)
  "Find the Nth field on the current line and return its raw string."
  (save-excursion
    (beginning-of-line)
    (let ((found nil)
          (code-end (csound--code-end-position)))
      (dotimes (_ n)
        (setq found (re-search-forward csound-number-regex code-end t)))
      (if found (match-string 0) nil))))

(defun csound-replace-nth-val (n new-val)
  "Replace the Nth absolute number with NEW-VAL. Macros are shielded."
  (save-excursion
    (beginning-of-line)
    (let ((found nil)
          (code-end (csound--code-end-position)))
      (dotimes (_ n)
        (setq found (re-search-forward csound-number-regex code-end t)))
      (when found
        (let ((beg     (match-beginning 0))
              (end     (match-end 0))
              (val-str (match-string 0)))
          (unless (csound-is-macro-p val-str)
            (delete-region beg end)
            (goto-char beg)
            (insert (format "%g" new-val))))))))

(defun csound--prev-i-statement ()
  "Move point to the start of the nearest previous i-statement, skipping blank
lines, comment lines, and any non-i score opcodes (f, s, t, e, …).
Returns t if one was found, nil if we hit the top of the buffer."
  (let (found)
    (while (and (not found) (= (forward-line -1) 0))
      (let* ((ln (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))
             (tr (string-trim ln)))
        (when (and (not (string-empty-p tr))
                   (not (string-prefix-p ";" tr))
                   (string-match-p "^[iI]" tr))
          (setq found t))))
    found))

(defun csound--get-number-at-column (col-idx)
  "Get the resolved numeric value of the COL-IDX-th field on the current line."
  (save-excursion
    (beginning-of-line)
    (let ((val-str nil)
          (count 0)
          (keep-going t)
          (code-end (csound--code-end-position)))
      (while (and keep-going (< count col-idx))
        (if (re-search-forward csound-number-regex code-end t)
            (progn (setq val-str (match-string 0))
                   (setq count (1+ count)))
          (setq val-str nil
                keep-going nil)))
      (cond
       ((not val-str)
        (if (csound--prev-i-statement)
            (csound--get-number-at-column col-idx)
          0.0))
       ((or (string= val-str ".") (string= val-str "^"))
        (if (csound--prev-i-statement)
            (csound--get-number-at-column col-idx)
          0.0))
       ((string= val-str "+")
        (if (csound--prev-i-statement)
            (+ (csound--get-number-at-column 2)
               (csound--get-number-at-column 3))
          0.0))
       ((string-match "^\\^\\([-+]\\)\\([0-9]+\\.[0-9]*\\|\\.[0-9]+\\|[0-9]+\\)$" val-str)
        (let ((sign (match-string 1 val-str))
              (num  (string-to-number (match-string 2 val-str))))
          (if (csound--prev-i-statement)
              (let ((prev (csound--get-number-at-column col-idx)))
                (if (string= sign "+") (+ prev num) (- prev num)))
            0.0)))
       ((string-match "^\\+\\([-+]\\)\\([0-9]+\\.[0-9]*\\|\\.[0-9]+\\|[0-9]+\\)$" val-str)
        (let ((sign (match-string 1 val-str))
              (num  (string-to-number (match-string 2 val-str))))
          (if (csound--prev-i-statement)
              (let ((base (+ (csound--get-number-at-column 2)
                             (csound--get-number-at-column 3))))
                (if (string= sign "+") (+ base num) (- base num)))
            0.0)))
       (t (string-to-number val-str))))))

(defun csound--replace-number-at-column (col-idx new-val &optional force)
  "Replace the COL-IDX-th number on the current line with NEW-VAL."
  (save-excursion
    (beginning-of-line)
    (let ((found nil)
          (code-end (csound--code-end-position)))
      (dotimes (_ col-idx)
        (setq found (re-search-forward csound-number-regex code-end t)))
      (when found
        (let ((beg     (match-beginning 0))
              (end     (match-end 0))
              (val-str (match-string 0)))
          (when (or force (not (csound-is-macro-p val-str)))
            (delete-region beg end)
            (goto-char beg)
            (insert (format "%g" new-val))))))))

;; --- TEMP FILE RESOLUTION ---
;; Csound does not understand ++N / +-N (or any of the carry macros beyond
;; the built-in ones).  Before handing the score to Csound we write a
;; throwaway temp file where every macro field has been expanded to a plain
;; number.  The original .sco buffer is NEVER modified by this process.
;;
;; Macro syntax recap (all resolved by csound--get-number-at-column):
;;   .  or  ^         carry: repeat same column from line above
;;   +                p2_prev + p3_prev  (note starts right when the last ended)
;;   ^+N  /  ^-N      same-column value ± N
;;   ++N  /  +-N  [NEW]  (p2_prev + p3_prev) ± N  (end-of-note ± offset)

(defvar csound--resolved-tempfile nil
  "Path to the last resolved temp .sco file written for Csound execution.
Cleaned up by `csound--cleanup-tempfile', called from `csound-stop'.")

(defun csound--cleanup-tempfile ()
  "Delete `csound--resolved-tempfile' if it still exists on disk."
  (when (and csound--resolved-tempfile
             (file-exists-p csound--resolved-tempfile))
    (delete-file csound--resolved-tempfile)
    (setq csound--resolved-tempfile nil)))

(defun csound--resolve-line-macros ()
  "Expand every score macro on the current line to its numeric value, in place,
and append any implicitly-carried fields that are absent from the line.
Blank lines, comment lines (starting with ';'), and non-i-statements are skipped."
  (let* ((bol     (line-beginning-position))
         (line    (buffer-substring-no-properties bol (line-end-position)))
         (trimmed (string-trim line)))
    (when (and (not (string-empty-p trimmed))
               (not (string-prefix-p ";" trimmed))
               (string-match-p "^[iI]" trimmed))
      (let (replacements
            (col-idx 0)
            (code-end (csound--code-end-position))) ; <-- Hard limit added
        ;; Pass 1 — scan explicit macros on this line
        (save-excursion
          (beginning-of-line)
          (while (re-search-forward csound-number-regex code-end t)
            (setq col-idx (1+ col-idx))
            (let* ((val-str (match-string 0))
                   (beg     (match-beginning 0))
                   (end     (match-end 0)))
              (when (csound-is-macro-p val-str)
                (push (list beg end (csound--get-number-at-column col-idx))
                      replacements)))))
        ;; Pass 1b — find how many fields the previous i-statement has
        (let (appended)
          (save-excursion
            (when (csound--prev-i-statement)
              (let ((prev-cols 0))
                (save-excursion
                  (beginning-of-line)
                  (while (re-search-forward csound-number-regex (csound--code-end-position) t)
                    (setq prev-cols (1+ prev-cols))))
                (when (> prev-cols col-idx)
                  (dotimes (k (- prev-cols col-idx))
                    (let* ((missing-col (+ col-idx k 1))
                           (val (csound--get-number-at-column missing-col)))
                      (push (format "%g" val) appended)))))))
          ;; Pass 2 — apply in-line replacements right-to-left
          (dolist (rep (sort replacements (lambda (a b) (> (car a) (car b)))))
            (delete-region (nth 0 rep) (nth 1 rep))
            (goto-char (nth 0 rep))
            (insert (format "%g" (nth 2 rep))))
          ;; Append missing fields RIGHT BEFORE the comment
          (when appended
            (goto-char (csound--code-end-position))
            (dolist (v (nreverse appended))
              (insert " " v))))))))

(defun csound--resolve-macros-in-buffer ()
  "Walk every line in the current buffer top-to-bottom and resolve all
score macros in place via `csound--resolve-line-macros'.
Processing top-to-bottom ensures each resolved line is available as
correct context for the carry macros on every subsequent line."
  (goto-char (point-min))
  (while (not (eobp))
    (csound--resolve-line-macros)
    (forward-line 1)))

(defun csound--create-resolved-tempfile ()
  "Return the path of a fresh temp .sco file with all macros expanded.
Reads the current buffer (after any pending save), copies it to a temp
buffer, runs `csound--resolve-macros-in-buffer', then writes the result.
The original buffer is NEVER modified.  Any previous temp file is deleted."
  (csound--cleanup-tempfile)
  (let* ((tmpfile          (make-temp-file "csound-score-" nil ".sco"))
         (original-content (buffer-string)))
    (with-temp-buffer
      (insert original-content)
      (csound--resolve-macros-in-buffer)
      (write-region (point-min) (point-max) tmpfile nil 'quiet))
    (setq csound--resolved-tempfile tmpfile)
    tmpfile))

;; 1. VARIABLES & CORE HELPERS (Buffer-local so files don't mix)
(defvar-local csound-cycle-local-p nil
  "Non-nil means `csound-cycle-column` harvests from the local paragraph instead of globally.")
(defun csound-toggle-cycle-scope ()
  "Toggle column cycling between global and local-only, updating the status bar."
  (interactive)
  (setq csound-cycle-local-p (not csound-cycle-local-p))
  (message "Csound cycle scope: %s" (if csound-cycle-local-p "LOCAL" "GLOBAL"))
  (force-mode-line-update))
(defvar-local my-inst-column-cycles '()
  "Alist mapping Instrument ID -> Column Index -> Full-field cycle lists.")
(defvar-local my-inst-column-decimal-cycles '()
  "Alist mapping Instrument ID -> Column Index -> Decimal-only cycle lists.")
(defvar-local current-field-index 0 "Stores the detected column index.")

(defun csound--sort-numeric (lst)
  "Sort a list of numeric strings numerically."
  (sort (copy-sequence lst)
        (lambda (a b) (< (string-to-number a) (string-to-number b)))))

(defun csound--extract-inst-id (p1)
  "Extract the instrument number from a p1 string (e.g., 'i9' -> '9')."
  (when (and (stringp p1) (string-match "^[iI]\\s-*\\([0-9]+\\)" p1))
    (match-string 1 p1)))

;; 2. HELPER: The Smart Splitter
(defun csound-split-line (line)
  "Split a Csound score line, keeping [...] expressions together. Ignores inline comments."
  (let* ((code-only (if (string-match ";" line)
                        (substring line 0 (match-beginning 0))
                      line))
         (fields '())
         (start 0))
    (while (string-match "\\[[^]]+\\]\\|\\S-+" code-only start)
      (push (match-string 0 code-only) fields)
      (setq start (match-end 0)))
    (nreverse fields)))

;; 3. THE HARVEST (Global & Local)
(defun csound--harvest-include-p (fields)
  "Return t only if this score line should be harvested (excludes instrument 1)."
  (when fields
    (let ((p1 (nth 0 fields))
          (p2 (nth 1 fields)))
      (and (stringp p1)
           (string-match-p "^[iI]" p1)
           (not (string-match-p "^[iI]1$" p1))
           (not (and (string-match-p "^[iI]$" p1)
                     (stringp p2) (string= p2 "1")))))))

(defun harvest-all-columns-to-cycle-list ()
  "Scan the document, harvest raw decimals, and sort them numerically."
  (interactive)
  (setq my-inst-column-cycles '()
        my-inst-column-decimal-cycles '())

  (save-excursion
    (goto-char (point-min))
    ;; REMOVED: (forward-line 5)
    ;; csound--harvest-include-p already ignores non-i statements safely.
    (while (not (eobp))
      (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (line-fields (csound-split-line line-text)))
        (when (and line-fields (csound--harvest-include-p line-fields))
          (let* ((inst-str (csound--extract-inst-id (nth 0 line-fields)))
                 (inst-id (when inst-str (string-to-number inst-str))))
            (when inst-id
              (cl-loop for val in line-fields
                       for idx from 0
                       do (unless (string= val ".")
                            ;; 1. Full Value Harvest (Keep raw)
                            (let ((inst-alist (alist-get inst-id my-inst-column-cycles)))
                              (unless (member val (alist-get idx inst-alist))
                                (push val (alist-get idx inst-alist))
                                (setf (alist-get inst-id my-inst-column-cycles) inst-alist)))

                            ;; 2. Decimal Harvest (Keep raw)
                            (when (string-match "\\.\\([0-9]+\\)" val)
                              (let* ((raw-dec (match-string 1 val))
                                     (inst-alist (alist-get inst-id my-inst-column-decimal-cycles)))
                                (unless (member raw-dec (alist-get idx inst-alist))
                                  (push raw-dec (alist-get idx inst-alist))
                                  (setf (alist-get inst-id my-inst-column-decimal-cycles) inst-alist))))))))))
      (forward-line 1)))

    ;; 3. Sort Numerically
    (dolist (inst-cell my-inst-column-decimal-cycles)
      (dolist (col-cell (cdr inst-cell))
        (setcdr col-cell (sort (cdr col-cell)
                               (lambda (a b)
                                 (< (string-to-number (concat "0." a))
                                    (string-to-number (concat "0." b))))))))

    (message "Harvest Complete: Raw data stored and sorted numerically."))

(defun csound--harvest-local-paragraph (target-inst target-col decimal-only)
  "Harvest values on-the-fly for TARGET-INST at TARGET-COL in the current visual block."
  (let ((results '()))
    (save-excursion
      ;; 1. Move to the very start of the visual block (scan up to blank line)
      (beginning-of-line)
      (while (and (not (bobp))
                  (not (looking-at-p "^\\s-*$")))
        (forward-line -1))
      (when (looking-at-p "^\\s-*$")
        (forward-line 1))

      ;; 2. Process line-by-line until we hit a blank line or end of buffer
      (while (and (not (eobp))
                  (not (looking-at-p "^\\s-*$")))
        (let* ((line (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
               (fields (csound-split-line line)))
          (when (and fields (csound--harvest-include-p fields))
            (let* ((inst-str (csound--extract-inst-id (nth 0 fields)))
                   (inst-id (when inst-str (string-to-number inst-str))))
              (when (and inst-id (= inst-id target-inst) (> (length fields) target-col))
                (let ((val (nth target-col fields)))
                  (unless (string= val ".")
                    (if (= decimal-only 1)
                        (when (string-match "\\.\\([0-9]+\\)" val)
                          (let ((dec (match-string 1 val)))
                            (setq dec (if (= (length dec) 1) (concat dec "0") dec))
                            (unless (member dec results) (push dec results))))
                      (unless (member val results) (push val results)))))))))
        (forward-line 1)))

    ;; 3. Apply the sorted results
    (if (= decimal-only 1)
        (sort results (lambda (a b)
                        (< (string-to-number (concat "0." a))
                           (string-to-number (concat "0." b)))))
      (csound--sort-numeric results))))

;; 4. FIELD CYCLING (Free cursor)
(defun cycle-current-field (direction)
  "Cycles the whole field at point based on the instrument ID."
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
    (let* ((line-fields (csound-split-line (buffer-substring-no-properties line-beg (line-end-position))))
           (inst-str (csound--extract-inst-id (nth 0 line-fields)))
           (inst-id (when inst-str (string-to-number inst-str)))
           (inst-alist (alist-get inst-id my-inst-column-cycles))
           (column-list (alist-get current-field-index inst-alist))
           (current-val (nth current-field-index line-fields)))
      (if (not (and current-val column-list))
          (message "No data for Col %d (Inst %s)." (1+ current-field-index) (or inst-id "None"))
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

;; 5. DECIMAL CYCLING (Free cursor)
(defun cycle-decimal-part (direction)
  "Cycles only the decimal part of the value at point based on the instrument ID."
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
    (let* ((line-fields (csound-split-line (buffer-substring-no-properties line-beg (line-end-position))))
           (inst-str (csound--extract-inst-id (nth 0 line-fields)))
           (inst-id (when inst-str (string-to-number inst-str)))
           (inst-dec-alist (alist-get inst-id my-inst-column-decimal-cycles))
           (decimal-list (alist-get detected-idx inst-dec-alist))
           (thing (thing-at-point 'symbol)))
      ;; Fixed Regex: Allows no-leading-zero decimals like .1825
      (if (and thing decimal-list (string-match "^\\([-+]?[0-9]*\\)\\.\\([0-9]*\\)$" thing))
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

;; 6. P-FIELD NAVIGATION (TAB / SHIFT-TAB)
(defun csound-score-goto-field (direction)
  "Move cursor to the end of the next (DIRECTION = +1) or previous (DIRECTION = -1) p-field."
  (let* ((line-beg  (line-beginning-position))
         (code-end  (csound--code-end-position))
         (line-text (buffer-substring-no-properties line-beg code-end))
         (fields    nil)
         (scan      0))
    (while (string-match "\\[[^]]+\\]\\|\\S-+" line-text scan)
      (push (cons (+ line-beg (match-beginning 0))
                  (+ line-beg (match-end       0)))
            fields)
      (setq scan (match-end 0)))
    (setq fields (nreverse fields))
    (when fields
      (let* ((pos     (point))
             (n       (length fields))
             (cur-idx -1))
        (cl-loop for span in fields
                 for i   from 0
                 when (and (>= pos (car span)) (<= pos (cdr span)))
                 do (setq cur-idx i))
        (when (= cur-idx -1)
          (cl-loop for span in fields
                   for i   from 0
                   when (<= (cdr span) pos)
                   do (setq cur-idx i)))
        (let* ((new-idx (+ cur-idx direction))
               (clamped (max 0 (min (1- n) new-idx))))
          (goto-char (cdr (nth clamped fields))))))))

;; 7. PINNED-COLUMN CYCLING
(defun csound-cycle-column (col-idx direction decimal-only &optional local-only)
  "Cycle a fixed p-field column based the current scope.

COL-IDX      — 0-based field index (so p1=0, p2=1, p3=2, …).
DIRECTION    — +1 forward, -1 backward.
DECIMAL-ONLY — 0 for full-field, 1 for decimal only.
LOCAL-ONLY   — 1 to force local, 0 to force global (current inst), 2 to force ALL insts.
               If omitted, defaults to `csound-cycle-scope` toggle."
  (let* ((cursor-marker (copy-marker (point) t))
         (line-beg  (line-beginning-position))
         (line-text (buffer-substring-no-properties line-beg (line-end-position)))
         (fields    (csound-split-line line-text))
         (cur-val   (nth col-idx fields))
         (inst-str  (csound--extract-inst-id (nth 0 fields)))
         (inst-id   (when inst-str (string-to-number inst-str)))

         ;; 1. Determine effective scope (argument overrides the toggle)
         (scope (cond ((eq local-only 1) 'local)
                      ((eq local-only 2) 'all)
                      ((eq local-only 0) 'global)
                      (t (if (boundp 'csound-cycle-scope) csound-cycle-scope 'global)))))

    (if (not inst-id)
        (message "csound-cycle-column: Could not detect valid Instrument ID.")
      (let ((target-list
             (cond
              ;; SCOPE: LOCAL
              ((eq scope 'local)
               (csound--harvest-local-paragraph inst-id col-idx decimal-only))

              ;; SCOPE: ALL INSTRUMENTS
              ((eq scope 'all)
               (let ((combined '())
                     (source-alist (if (= decimal-only 1)
                                       my-inst-column-decimal-cycles
                                     my-inst-column-cycles)))
                 ;; Harvest from every instrument in the alist
                 (dolist (inst-cell source-alist)
                   (let ((col-list (alist-get col-idx (cdr inst-cell))))
                     ;; CRITICAL FIX: copy-sequence prevents destructive functions
                     ;; from mutating and deleting your global harvest data.
                     (setq combined (append combined (copy-sequence col-list)))))

                 ;; Remove duplicates and sort the safe copy
                 (setq combined (delete-dups combined))
                 (if (= decimal-only 1)
                     (sort combined (lambda (a b) (< (string-to-number (concat "0." a))
                                                     (string-to-number (concat "0." b)))))
                   (csound--sort-numeric combined))))

              ;; SCOPE: GLOBAL (Current Instrument Only)
              (t
               (let ((inst-alist (alist-get inst-id (if (= decimal-only 1)
                                                        my-inst-column-decimal-cycles
                                                      my-inst-column-cycles))))
                 (alist-get col-idx inst-alist))))))

        (if (= decimal-only 0)
            ;; ── FULL-FIELD mode ────────────────────────────────────────────────
            (if (not (and cur-val target-list))
                (message "No harvest data found for field %d." (1+ col-idx))
              (let* ((pos     (or (cl-position cur-val target-list :test #'string=) -1))
                     (new-pos (mod (+ pos direction) (length target-list)))
                     (new-val (nth new-pos target-list))
                     (rbeg nil) (rend nil)
                     (scan-start 0) (count 0)
                     (lt (buffer-substring-no-properties line-beg (line-end-position))))
                (while (and (not rbeg)
                            (string-match "\\[[^]]+\\]\\|\\S-+" lt scan-start))
                  (if (= count col-idx)
                      (setq rbeg (+ line-beg (match-beginning 0))
                            rend (+ line-beg (match-end 0)))
                    (setq scan-start (match-end 0) count (1+ count))))
                (when rbeg
                  (delete-region rbeg rend)
                  (goto-char rbeg)
                  (insert new-val)
                  (when (fboundp 'csound-score-align) (csound-score-align))
                  (message "[Inst %d | %s] p%d → %s" inst-id
                           (upcase (symbol-name scope))
                           (1+ col-idx) new-val))))

          ;; ── DECIMAL-ONLY mode ──────────────────────────────────────────────
          (if (not (and cur-val target-list (string-match "^\\([-+]?[0-9]*\\)\\.\\([0-9]*\\)$" cur-val)))
              (message "No decimal data found for field %d." (1+ col-idx))
            (let* ((int-part  (match-string 1 cur-val))
                   (int-num   (string-to-number int-part))
                   (cur-dec   (match-string 2 cur-val))
                   (cur-dec-c (cond ((string= cur-dec "")  "00")
                                    ((= (length cur-dec) 1) (concat cur-dec "0"))
                                    (t cur-dec)))
                   (list-len  (length target-list))
                   (pos       (or (cl-position cur-dec-c target-list :test #'string=) -1))
                   (wrap-fwd  (and (= direction  1) (= pos (1- list-len))))
                   (wrap-bwd  (and (= direction -1) (= pos 0)))
                   (new-int   (+ int-num (cond (wrap-fwd  1) (wrap-bwd -1) (t 0))))
                   (new-pos   (mod (+ pos direction) list-len))
                   (new-dec   (nth new-pos target-list))
                   (rbeg nil) (rend nil)
                   (scan-start 0) (count 0)
                   (lt (buffer-substring-no-properties line-beg (line-end-position))))
              (while (and (not rbeg)
                          (string-match "\\[[^]]+\\]\\|\\S-+" lt scan-start))
                (if (= count col-idx)
                    (setq rbeg (+ line-beg (match-beginning 0))
                          rend (+ line-beg (match-end 0)))
                  (setq scan-start (match-end 0) count (1+ count))))
              (when (and rbeg new-dec)
                (delete-region rbeg rend)
                (goto-char rbeg)
                (let ((final-int-str (if (or wrap-fwd wrap-bwd)
                                         (number-to-string new-int)
                                       int-part)))
                  (insert (concat final-int-str "." new-dec)))
                (when (fboundp 'csound-score-align) (csound-score-align))
                (message "[Inst %d | %s] p%d -> %s.%s%s" inst-id
                         (upcase (symbol-name scope))
                         (1+ col-idx)
                         (if (or wrap-fwd wrap-bwd) (number-to-string new-int) int-part)
                         new-dec
                         (cond (wrap-fwd " (carry +1)") (wrap-bwd " (borrow -1)") (t "")))))))))

    (goto-char cursor-marker)
    (set-marker cursor-marker nil)))

(defun csound-cycle-current-column (direction decimal-only &optional local-only)
  "Detect the column under the cursor and cycle it using `csound-cycle-column`.
DIRECTION is +1 or -1. DECIMAL-ONLY is 1 (yes) or 0 (no).
LOCAL-ONLY is optional (1 for local, 2 for all, 0 for global)."
  (let* ((line-beg (line-beginning-position))
         (orig-point (point))
         (found-idx nil)
         (line-text (buffer-substring-no-properties line-beg (line-end-position))))

    ;; 1. Calculate which column index the cursor is currently inside
    (save-excursion
      (beginning-of-line)
      (let ((start 0) (idx 0))
        (while (string-match "\\[[^]]+\\]\\|\\S-+" line-text start)
          (let ((m-beg (+ line-beg (match-beginning 0)))
                (m-end (+ line-beg (match-end 0))))
            (when (and (>= orig-point m-beg) (<= orig-point m-end))
              (setq found-idx idx)))
          (setq start (match-end 0) idx (1+ idx)))))

    ;; 2. Default to column 0 if we are somehow outside a token
    (unless found-idx (setq found-idx 0))

    ;; 3. Execute the standard cycle with the dynamic index
    (csound-cycle-column found-idx direction decimal-only local-only)))

(defvar csound-active-layout 'engram
  "The current manual keyboard layout for csound-mode ('engram or 'qwerty).")

;; --- ENGRAM LAYER ---

;; > b y o u '    ( d n g v q
;; 0 1 2 3 4 ,    . 5 6 7 8 9
;; ~ ^ # * & -    ? @ = + $ /

(defvar csound-engram-map
  (let ((map (make-sparse-keymap)))
    ;; general shortcuts ;;
    (define-key map (kbd "u")           #'csound-start)
    (define-key map (kbd "U")           #'csound-stop-and-log)
    (define-key map (kbd "M-u")         #'csound-stop-and-track)
    (define-key map (kbd "C-c u w")     #'csound-record-wav)
    (define-key map (kbd "C-c u o")     #'csound-record-ogg)
    (define-key map (kbd "o")           #'play-from-value)
    (define-key map (kbd "y")           #'play-from-cursor)
    (define-key map (kbd "b")           #'play-from-zero)
    (define-key map (kbd "B")           #'play-from-point)
    (define-key map (kbd "M-b")         #'print-start-value)

    (define-key map (kbd "M-.")         #'csound-toggle-cycle-scope)

    (define-key map (kbd "d")           #'duplicate-line)
    (define-key map (kbd "n")           #'csound-smart-duplicate)
    (define-key map (kbd "N")           #'csound-custom-duplicate)
    (define-key map (kbd "C-N")         #'csound-custom-duplicate-repeat)

    (define-key map (kbd "Q")           #'csound-header-edit)
    (define-key map (kbd "C-c i")       #'csound-show-column-info)
    (define-key map (kbd "C-c I")       #'csound-edit-column-info)
    (define-key map (kbd "M-q") (lambda () (interactive) (insert "1.02197503906")))

    ;; p-field cycle ;;
    (define-key map (kbd "&") (lambda () (interactive) (csound-cycle-column 2  1 0))) ; dur
    (define-key map (kbd "*") (lambda () (interactive) (csound-cycle-column 2 -1 0)))
    (define-key map (kbd "@") (lambda () (interactive) (csound-cycle-column 4  1 1))) ; note global
    (define-key map (kbd "=") (lambda () (interactive) (csound-cycle-column 4 -1 1)))
    (define-key map (kbd "+") (lambda () (interactive) (csound-cycle-column 4  1 1 1))) ; note local
    (define-key map (kbd "$") (lambda () (interactive) (csound-cycle-column 4 -1 1 1)))

    ;; Dynamic column cycling (Column Agnostic)
    ;; Full-field cycling (+1 / -1)
    ;;(define-key map (kbd "C-c >") (lambda () (interactive) (csound-cycle-current-column  1 0)))
    ;;(define-key map (kbd "C-c <") (lambda () (interactive) (csound-cycle-current-column -1 0)))
    ;; Decimal-only cycling (+1 / -1)
    ;;(define-key map (kbd "C-c .") (lambda () (interactive) (csound-cycle-current-column  1 1)))
    ;;(define-key map (kbd "C-c ,") (lambda () (interactive) (csound-cycle-current-column -1 1)))
    ;; Force-local dynamic cycling
    ;;(define-key map (kbd "C-c }") (lambda () (interactive) (csound-cycle-current-column  1 0 1)))
    ;;(define-key map (kbd "C-c {") (lambda () (interactive) (csound-cycle-current-column -1 0 1)))

    map)
  "Engram-specific shortcuts for csound-mode.")

;; --- QWERTY LAYER ---
(defvar csound-qwerty-map
  (let ((map (make-sparse-keymap)))
    ;; general shortcuts ;;
    (define-key map (kbd "f") #'csound-start)
    (define-key map (kbd "F") #'csound-stop-and-log)
    (define-key map (kbd "M-f") #'csound-stop-and-track)
    (define-key map (kbd "C-c f w") #'csound-record-wav)
    (define-key map (kbd "C-c f o") #'csound-record-ogg)
    (define-key map (kbd "d") #'duplicate-line)
    (define-key map (kbd "s") #'csound-smart-duplicate)
    (define-key map (kbd "S") #'csound-custom-duplicate)
    (define-key map (kbd "C-S") #'csound-custom-duplicate-repeat)
    (define-key map (kbd "r") #'play-from-value)
    (define-key map (kbd "e") #'which-play-value-modify)
    (define-key map (kbd "E") #'which-play-value-show)
    (define-key map (kbd "M-e") #'csound-show-macro-values)
    (define-key map (kbd "w") #'play-from-zero)
    (define-key map (kbd "Q") #'csound-header-edit)
    (define-key map (kbd "A") (lambda () (interactive) (insert "1.02197503906")))

    ;; p-fields cycle ;;
    (define-key map (kbd "t") (lambda () (interactive) (csound-cycle-column 2  1 0))) ; dur p-field
    (define-key map (kbd "y") (lambda () (interactive) (csound-cycle-column 2 -1 0)))
    (define-key map (kbd "g") (lambda () (interactive) (csound-cycle-column 3  1 0))) ; amp p-field
    (define-key map (kbd "h") (lambda () (interactive) (csound-cycle-column 3 -1 0)))
    (define-key map (kbd "b") (lambda () (interactive) (csound-cycle-column 4  1 1))) ; note p-field
    (define-key map (kbd "n") (lambda () (interactive) (csound-cycle-column 4 -1 1)))
    (define-key map (kbd "u") (lambda () (interactive) (csound-cycle-column 5  1 0))) ; p6
    (define-key map (kbd "i") (lambda () (interactive) (csound-cycle-column 5 -1 0)))
    (define-key map (kbd "j") (lambda () (interactive) (csound-cycle-column 6  1 0))) ; p7
    (define-key map (kbd "k") (lambda () (interactive) (csound-cycle-column 6 -1 0)))
    (define-key map (kbd "m") (lambda () (interactive) (csound-cycle-column 7  1 0))) ; p8
    (define-key map (kbd ",") (lambda () (interactive) (csound-cycle-column 7 -1 0)))
    (define-key map (kbd "o") (lambda () (interactive) (csound-cycle-column 8  1 0))) ; p9
	(define-key map (kbd "p") (lambda () (interactive) (csound-cycle-column 8 -1 0)))
    (define-key map (kbd "l") (lambda () (interactive) (csound-cycle-column 9  1 0))) ; p10
	(define-key map (kbd ";") (lambda () (interactive) (csound-cycle-column 9 -1 0)))
    (define-key map (kbd "[") (lambda () (interactive) (csound-cycle-column 10	1 0))) ; p11
	(define-key map (kbd "]") (lambda () (interactive) (csound-cycle-column 10 -1 0)))
    (define-key map (kbd "'") (lambda () (interactive) (csound-cycle-column 11	1 0))) ; p12
	(define-key map (kbd "\\") (lambda () (interactive) (csound-cycle-column 11 -1 0)))
    map)
  "QWERTY-specific shortcuts for csound-mode.")

;; --- BASE MAP ---
(defvar csound-mode-map
  (let ((map (make-sparse-keymap)))
    ;; Layout-Agnostic Shortcuts (Same on both layouts)
    (define-key map (kbd "<TAB>")     (lambda () (interactive) (csound-score-goto-field  1)))
    (define-key map (kbd "<backtab>") (lambda () (interactive) (csound-score-goto-field -1)))
    (define-key map (kbd "M-<tab>")   #'csound-score-align)
    (define-key map (kbd "C-<iso-lefttab>") 'csound-recalculate-starts)

    ;; Instruments shortcuts
    (define-key map (kbd "C-c r") (lambda () (interactive) (insert "; bf0000's keyboard\n\n;12 STR DUR AMP NOTE KC1 KC2 VDEPTH STR VRATE END EDO REPEAT BASE")))
    (define-key map (kbd "C-c g") (lambda () (interactive) (insert "; 00b100's lyre\n\n;9 STR DUR AMP NOTE PLK PICK REFL EDO REPEAT BASE")))
    (define-key map (kbd "C-c b") (lambda () (interactive) (insert "; 3044eb's violin\n\n;11 STR DUR AMP NOTE PRES RAT STR VIBF END VAMP EDO REPEAT BASE")))
    (define-key map (kbd "C-c y") (lambda () (interactive) (insert "; 9e9b00's vibraphone\n\n;10 STR DUR AMP NOTE HRD POS STR VIBF END EDO REPEAT BASE")))

    ;; Set the initial parent to Engram
    (set-keymap-parent map csound-engram-map)
    map)
  "Base keymap for csound-mode. Inherits from active layout.")

;; 1. Upgrade variable from boolean to symbol
(defvar-local csound-cycle-scope 'global
  "Current scope for `csound-cycle-column`. Can be 'global, 'local, or 'all.")

;; 2. Upgrade the lighter to check all 3 states
(defvar csound-mode-lighter '(:eval (pcase csound-cycle-scope
                                      ('local " cycle[L]")
                                      ('all   " cycle[A]")
                                      (_      " cycle[G]")))
  "Dynamic mode-line indicator for `csound-mode`.")
(put 'csound-mode-lighter 'risky-local-variable t)

;; 3. Make the toggle function cycle in a 3-way loop
(defun csound-toggle-cycle-scope ()
  "Toggle column cycling between global, local, and all instruments."
  (interactive)
  (setq csound-cycle-scope
        (pcase csound-cycle-scope
          ('global 'local)
          ('local  'all)
          ('all    'global)))
  (message "Csound cycle scope: %s" (upcase (symbol-name csound-cycle-scope)))
  (force-mode-line-update t))

;; --- INSTRUMENT COLUMN DEFINITIONS ---

(defvar csound-instrument-info-alist nil
  "Alist storing instrument column definitions.
Format: ((12 . (\"column 0 info\" \"column 1 info\")) ...)")

(defun csound-parse-instrument-info ()
  "Parse the *Csound Col Info* buffer and update `csound-instrument-info-alist`."
  (interactive)
  (setq csound-instrument-info-alist nil)
  (save-excursion
    (with-current-buffer "*Csound Col Info*"
      (goto-char (point-min))
      (while (not (eobp))
        ;; Skip to the start of the next paragraph
        (skip-chars-forward " \t\n")
        (unless (eobp)
          (let* ((para-end (save-excursion (forward-paragraph) (point)))
                 (para-text (buffer-substring-no-properties (point) para-end))
                 ;; Split by newline, dropping empty lines
                 (lines (split-string para-text "\n" t)))
            (when lines
              (let ((first-line (car lines))
                    (info-lines (cdr lines)))
                ;; Look for i[number] or I[number]
                (when (string-match "^[iI]\\s-*\\([0-9]+\\)" (string-trim first-line))
                  (let ((inst-id (string-to-number (match-string 1 (string-trim first-line)))))
                    (push (cons inst-id info-lines) csound-instrument-info-alist)))))
            (goto-char para-end))))))
  (message "Instrument column definitions updated!"))

(defun csound-edit-column-info ()
  "Open a buffer to define instrument column information.
Press C-q to save the definitions and close."
  (interactive)
  (switch-to-buffer "*Csound Col Info*")
  (text-mode)
  ;; Insert a helpful header if the buffer is empty
  (when (= (point-min) (point-max))
    (insert ";; Define instrument columns here.\n")
    (insert ";; Separate instruments with a blank line.\n")
    (insert ";; Press C-c C-c to save and exit.\n\n")
    (insert "i12\ntext for col 0 (p1)\ntext for col 1 (p2)\n\n"))
  (local-set-key (kbd "C-q") (lambda ()
                                   (interactive)
                                   (csound-parse-instrument-info)
                                   (quit-window))))

(defun csound-show-column-info ()
  "Identify the instrument and column at point, and display its definition."
  (interactive)
  (let* ((line-beg (line-beginning-position))
         (orig-point (point))
         (found-idx nil)
         (line-text (buffer-substring-no-properties line-beg (line-end-position)))
         (fields (csound-split-line line-text)))

    ;; 1. Find which column index the cursor is currently inside
    (save-excursion
      (beginning-of-line)
      (let ((start 0) (idx 0))
        (while (string-match "\\[[^]]+\\]\\|\\S-+" line-text start)
          (let ((m-beg (+ line-beg (match-beginning 0)))
                (m-end (+ line-beg (match-end 0))))
            (when (and (>= orig-point m-beg) (<= orig-point m-end))
              (setq found-idx idx)))
          (setq start (match-end 0) idx (1+ idx)))))

    ;; Default to column 0 if we are somehow outside a token but on the line
    (unless found-idx (setq found-idx 0))

    ;; 2. Extract instrument ID and look it up
    (let* ((inst-str (csound--extract-inst-id (nth 0 fields)))
           (inst-id (when inst-str (string-to-number inst-str))))
      (if (not inst-id)
          (message "No instrument detected on this line.")
        (let* ((inst-info (alist-get inst-id csound-instrument-info-alist))
               (col-desc (nth found-idx inst-info)))
          (if col-desc
              (message "[Inst %d / Col %d]: %s" inst-id found-idx col-desc)
            (message "[Inst %d / Col %d]: No definition provided." inst-id found-idx)))))))

;; --- MINOR MODE ---
(define-minor-mode csound-mode
  " compoooosing "
  :init-value nil
  :lighter csound-mode-lighter  ; Pass the variable symbol directly here
  :keymap csound-mode-map       ; Explicitly point to our base map
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

;; 3. FAILSAFE: Forcefully update Emacs' internal alist for the active session
(let ((cell (assq 'csound-mode minor-mode-alist)))
  (if cell
      (setcdr cell '(csound-mode-lighter))
    (push '(csound-mode csound-mode-lighter) minor-mode-alist)))

(defun csound-toggle-layout ()
  "Manually toggle csound-mode shortcuts between Engram and QWERTY."
  (interactive)
  (if (eq csound-active-layout 'engram)
      (progn
        (setq csound-active-layout 'qwerty)
        (set-keymap-parent csound-mode-map csound-qwerty-map)
        (message "Keyboard Layout: QWERTY"))
    (progn
      (setq csound-active-layout 'engram)
      (set-keymap-parent csound-mode-map csound-engram-map)
      (message "Keyboard Layout: ENGRAM"))))

;; Optional: Bind the toggle to a global key so you can hit it anytime
;; (global-set-key (kbd "C-c l") 'csound-toggle-layout)

;; gotta change this to text-mode only!
(global-set-key (kbd "C-c t") 'csound-mode) ; global shortcut as we need to return from text-mode

;; Create a lightweight major mode derived from text-mode
(define-derived-mode csound-score-mode text-mode "Csound Score"
  "Major mode for editing Csound files, triggering csound-mode automatically."
  (csound-mode 1)         ; Activate your custom minor mode
  (setq line-spacing -1)) ; Apply your tight line spacing

;; Associate .sco files with this new major mode
(add-to-list 'auto-mode-alist '("\\.sco\\'" . csound-score-mode))

(provide 'csound-score)
