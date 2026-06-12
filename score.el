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

(defun csound-start ()
  (interactive)
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
  (save-buffer)
  (let ((score-file (csound--create-resolved-tempfile)))
    (call-process "csound" nil 0 nil
                  "-o" (file-name-with-extension buffer-file-name ".wav")
                  "/home/luqtas/Desktop/projects/qob/Csound/header.orc"
                  score-file "-W")))
; -o noise.wav -W ; for file output any platform

(defun csound-record-ogg ()
  (interactive)
  (save-buffer)
  (let ((score-file (csound--create-resolved-tempfile)))
    (call-process "csound" nil 0 nil
                  "-o" (file-name-with-extension buffer-file-name ".ogg")
                  "--ogg"
                  "/home/luqtas/Desktop/projects/qob/Csound/header.orc"
                  score-file)))
; -o noise.ogg --ogg

(defun csound-stop ()
  (interactive)
  (call-process "killall" nil 0 nil "csound")
  (csound--cleanup-tempfile))

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
      (let* ((stop-time       (float-time))
             (elapsed-seconds (- stop-time csound-start-time))
             (start-offset    (if (boundp 'a) (or a 0) 0))
             (total-score-time (+ start-offset elapsed-seconds)))
        (message "Stopped at score time: %.2f seconds (Offset: %.2f + Elapsed: %.2f)"
                 total-score-time start-offset elapsed-seconds)
        (setq csound-start-time nil))
    (message "Csound stopped. No active timer to reset.")))

(defun which-play-value-show ()
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

(defun which-play-value-modify (arg)
  "Modify the advance statement's 4th column and start Csound.
With no prefix (y), harvests the 2nd column of the current line.
With a prefix (C-u y), prompts for N and searches for 'pN'."
  (interactive "P")
  (let (a)
    (if arg
        ;; --- Pathway 1: C-u y (Search for pN) ---
        (save-excursion
          ;; Use read-number to accept multi-digit inputs and %d to format the integer
          (let* ((num (read-number "Enter number: "))
                 (target (format "p%d" num)))
            (goto-char (point-min))
            (if (search-forward target nil t)
                ;; Harvest using your custom function on the found line
                (setq a (csound--get-number-at-column 2))
              (error "Could not find '%s' in the buffer." target))))

      ;; --- Pathway 2: Plain y (Harvest current line) ---
      (setq a (csound--get-number-at-column 2)))

    ;; --- Apply the modification using your custom functions ---
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

;; workflow for cycling through "scales" (typed notes at the score) ;;
;; 1. VARIABLES (Buffer-local so files don't mix)
(defvar-local my-column-cycles '() "Alist for full-field cycling.")
(defvar-local my-column-decimal-cycles '() "Alist for decimal-only cycling.")
(defvar-local current-field-index 0 "Stores the detected column index.")
;; 2. HELPER: The Smart Splitter (The missing function!)
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
;; 3. THE HARVEST: Scans the file for all possible values

(defun csound--harvest-include-p (fields)
  "Return t only if this score line should be harvested.
Only i-statements for instruments other than 1 qualify.
All other opcodes (a b B C d e f m n q r s t v x y, …) are excluded —
they carry no musically cyclable p-field data."
  (when fields
    (let ((p1 (nth 0 fields))
          (p2 (nth 1 fields)))
      (and (stringp p1)
           ;; Must be an i-statement (bare 'i'/'I' or compact 'i2', 'i3', …)
           (string-match-p "^[iI]" p1)
           ;; Exclude instrument 1  (i1 / i 1)
           (not (string-match-p "^[iI]1$" p1))
           (not (and (string-match-p "^[iI]$" p1)
                     (stringp p2) (string= p2 "1")))))))

(defun harvest-all-columns-to-cycle-list ()
  "Scan the whole document and build full-value AND decimal-only cycle lists.
Only i-statements for instruments other than 1 are harvested."
  (interactive)
  (setq my-column-cycles '()
        my-column-decimal-cycles '())
  (save-excursion
    (goto-char (point-min))
    (forward-line 5) ;; Skip header
    (while (not (eobp))
      (let* ((line-text (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
             (line-fields (csound-split-line line-text)))
        (when (and line-fields
                   (csound--harvest-include-p line-fields))
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

;; 6. PINNED-COLUMN CYCLING
(defun csound-cycle-column (col-idx direction decimal-only)
  "Cycle a fixed p-field column regardless of where the cursor is.

COL-IDX    — 0-based field index (so p1=0, p2=1, p3=2, …).
DIRECTION  — +1 forward, -1 backward through the harvested list.
DECIMAL-ONLY — 0 cycles the whole field value (uses `my-column-cycles');
               1 cycles only the decimal part  (uses `my-column-decimal-cycles').

The cursor is restored to its exact pre-call position after the edit and
after the alignment pass, using a buffer marker so character-position
shifts from text replacement don't corrupt the saved location.

Typical keymap usage:
  (define-key map (kbd \"'\") (lambda () (interactive) (csound-cycle-column 3 1  0)))
  (define-key map (kbd \"(\") (lambda () (interactive) (csound-cycle-column 3 -1 0)))
  (define-key map (kbd \"'\") (lambda () (interactive) (csound-cycle-column 5 1  1)))"
  (let* (;; Pin the cursor with a marker so alignment can't lose it.
         (cursor-marker (copy-marker (point) t))
         (line-beg  (line-beginning-position))
         (line-text (buffer-substring-no-properties line-beg (line-end-position)))
         (fields    (csound-split-line line-text))
         (cur-val   (nth col-idx fields)))
    (if (= decimal-only 0)
        ;; ── FULL-FIELD mode ────────────────────────────────────────────────
        (let* ((column-list (cdr (assoc col-idx my-column-cycles))))
          (if (not (and cur-val column-list))
              (message "csound-cycle-column: no harvest data for field %d." (1+ col-idx))
            (let* ((pos     (or (cl-position cur-val column-list :test #'string=) -1))
                   (new-pos (mod (+ pos direction) (length column-list)))
                   (new-val (nth new-pos column-list))
                   ;; Locate the token boundaries inside the live buffer.
                   (rbeg nil) (rend nil)
                   (scan-start 0) (count 0)
                   (lt (buffer-substring-no-properties line-beg (line-end-position))))
              (while (and (not rbeg)
                          (string-match "\\[[^]]+\\]\\|\\S-+" lt scan-start))
                (if (= count col-idx)
                    (setq rbeg (+ line-beg (match-beginning 0))
                          rend (+ line-beg (match-end 0)))
                  (setq scan-start (match-end 0)
                        count      (1+ count))))
              (when rbeg
                (delete-region rbeg rend)
                (goto-char rbeg)
                (insert new-val)
                (when (fboundp 'csound-score-align) (csound-score-align))
                (message "p%d → %s" (1+ col-idx) new-val)))))
      ;; ── DECIMAL-ONLY mode ──────────────────────────────────────────────
      (let* ((decimal-list (alist-get col-idx my-column-decimal-cycles)))
        (if (not (and cur-val decimal-list
                      (string-match "\\([0-9]+\\)\\.\\([0-9]*\\)" cur-val)))
            (message "csound-cycle-column: no decimal data for field %d (value: %s)."
                     (1+ col-idx) (or cur-val "nil"))
          (let* ((int-part  (match-string 1 cur-val))
                 (int-num   (string-to-number int-part))
                 (cur-dec   (match-string 2 cur-val))
                 (cur-dec-c (cond ((string= cur-dec "")  "00")
                                  ((= (length cur-dec) 1) (concat cur-dec "0"))
                                  (t cur-dec)))
                 (list-len  (length decimal-list))
                 (pos       (or (cl-position cur-dec-c decimal-list :test #'string=) -1))
                 ;; Detect wrap-around only when the current decimal is known.
                 ;; Forward past the last  -> carry  +1 to the integer part.
                 ;; Backward past the first -> borrow -1 from the integer part.
                 (wrap-fwd  (and (= direction  1) (= pos (1- list-len))))
                 (wrap-bwd  (and (= direction -1) (= pos 0)))
                 (new-int   (+ int-num (cond (wrap-fwd  1)
                                             (wrap-bwd -1)
                                             (t         0))))
                 (new-pos   (mod (+ pos direction) list-len))
                 (new-dec   (nth new-pos decimal-list))
                 ;; Find the token in the live buffer (same scan as full-field).
                 (rbeg nil) (rend nil)
                 (scan-start 0) (count 0)
                 (lt (buffer-substring-no-properties line-beg (line-end-position))))
            (while (and (not rbeg)
                        (string-match "\\[[^]]+\\]\\|\\S-+" lt scan-start))
              (if (= count col-idx)
                  (setq rbeg (+ line-beg (match-beginning 0))
                        rend (+ line-beg (match-end 0)))
                (setq scan-start (match-end 0)
                      count      (1+ count))))
            (when (and rbeg new-dec)
              (delete-region rbeg rend)
              (goto-char rbeg)
              (insert (concat (number-to-string new-int) "." new-dec))
              (when (fboundp 'csound-score-align) (csound-score-align))
              (message "p%d -> %d.%s%s" (1+ col-idx) new-int new-dec
                       (cond (wrap-fwd " (carry +1)") (wrap-bwd " (borrow -1)") (t ""))))))))
    ;; Restore cursor unconditionally, after both branches and after alignment.
    (goto-char cursor-marker)
    (set-marker cursor-marker nil)))

(global-set-key (kbd "%") 'csound-mode)

(define-minor-mode csound-mode
  " compoooosing "
  nil ; INITIAL VALUE
  " csound" ; INDICATOR
  :keymap (let ((map (make-sparse-keymap)))
			;; > b y o u ' ( d n g v q
			;; 0 1 2 3 4 , . 5 6 7 8 9
			;; ~ ^ # * & - ? @ = + $ /
			;; TODO: a shortcut to toggle between csound-mode for typing comments!
			(define-key map (kbd "&") (lambda () (interactive) (csound-cycle-column 2  1 0))) ; p3 forward full
			(define-key map (kbd "*") (lambda () (interactive) (csound-cycle-column 2 -1 0))) ; p3 backward full
			(define-key map (kbd "@") (lambda () (interactive) (csound-cycle-column 4  1 1))) ; p5 forward decimal
			(define-key map (kbd "=") (lambda () (interactive) (csound-cycle-column 4 -1 1))) ; p5 backward decimal
            (define-key map (kbd "<TAB>") #'csound-score-align)
            (define-key map (kbd "<backtab>") 'csound-recalculate-starts)
            (define-key map (kbd "n") #'csound-start)
			(define-key map (kbd "o") #'csound-stop-and-log)
			(define-key map (kbd "N w") #'csound-record-wav)
			(define-key map (kbd "N o") #'csound-record-ogg)
			(define-key map (kbd "q") #'csound-header-edit)
			(define-key map (kbd "d") #'duplicate-line)
			(define-key map (kbd "g") #'csound-smart-duplicate)
			(define-key map (kbd "G") #'csound-custom-duplicate)
			(define-key map (kbd "C-G") #'csound-custom-duplicate-repeat)
			(define-key map (kbd "y") #'play-from-value)
			(define-key map (kbd "Y") #'which-play-value-show)
			(define-key map (kbd "v") #'which-play-value-modify)
			(define-key map (kbd "b") #'play-from-zero)
			(define-key map (kbd "u") (lambda () (interactive) (insert "1.02197503906")))
			;; instruments shortcut ;;
			(define-key map (kbd "C-c r") (lambda () (interactive) (insert "; bf0000's keyboard\n\n;12 STR DUR AMP NOTE KC1 KC2 VDEPTH STR VRATE END EDO REPEAT BASE")))
			(define-key map (kbd "C-c g") (lambda () (interactive) (insert "; 00b100's lyre\n\n;9 STR DUR AMP NOTE PLK PICK REFL EDO REPEAT BASE")))
			(define-key map (kbd "C-c b") (lambda () (interactive) (insert "; 3044eb's violin\n\n;11 STR DUR AMP NOTE PRES RAT STR VIBF END VAMP EDO REPEAT BASE")))
			(define-key map (kbd "C-c y") (lambda () (interactive) (insert "; 9e9b00's vibraphone\n\n;10 STR DUR AMP NOTE HRD POS STR VIBF END EDO REPEAT BASE")))
            map)
  (if csound-mode
      (progn
        (modify-syntax-entry ?\. "w" (syntax-table))
		(visual-line-mode -1)
		(setq truncate-lines t)
		(harvest-all-columns-to-cycle-list))
    (progn
      (text-mode)
      (modify-syntax-entry ?\. "." (syntax-table))
	  (visual-line-mode 1)
	  (setq truncate-lines nil))))

(add-hook 'fundamental-mode-hook 'csound-mode) ; ASSOCIATE MAJOR MODE WITH A MINOR MODE

;; tight spacing when we arrange the score ;;
(add-hook 'fundamental-mode-hook
          (lambda ()
            ;; Adjust this number (-1, 0, 1, 2) to get the exact compactness you like
            (setq line-spacing -1)))

;; ASSOCIATE FILES WITH A MODE ;;
(add-to-list 'auto-mode-alist '("\\.sco\\'" . fundamental-mode))
