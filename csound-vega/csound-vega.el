(require 'json)
(require 'csound-score)

(defun csound-vega-harvest (target-inst col-idx)
  "Harvest data. TARGET-INST can be 'all' or a number. COL-IDX is the p-field.
COL-IDX is 0-based: 0 = instrument field, 1 = p2, 2 = p3, etc.
All Csound carry/offset macros (+, ., ^, ^+N, ^-N, ++N, +-N) are fully
resolved via `csound--get-number-at-column' from csound-score."
  (let ((data-list '()))
    (with-current-buffer (current-buffer)
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "^i\\([0-9]+\\)" nil t)
          (let ((inst-id (string-to-number (match-string 1))))
            (when (or (eq target-inst 'all) (= inst-id target-inst))
              ;; csound--get-number-at-column counts regex matches 1-based:
              ;; match 1 = the digit inside "i9", match 2 = p2, match 3 = p3…
              ;; So we add 1 to bridge from our 0-based col-idx convention.
              (let ((val (csound--get-number-at-column (1+ col-idx))))
                (push val data-list)))))))
    (reverse data-list)))

(defun csound-vega-harvest-xy (inst-list x-col y-col)
  "Harvests two columns for X and Y coordinates.
X-COL and Y-COL are 0-based (0 = instrument field, 1 = p2, …).
Macros are resolved via `csound--get-number-at-column'."
  (let ((data-combined '()))
    (dolist (inst (if (equal inst-list '("all"))
                      (csound-vega-get-all-unique-instruments)
                    inst-list))
      (with-current-buffer (current-buffer)
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward (format "^i%s" inst) nil t)
            (let* ((x-val (csound--get-number-at-column (1+ x-col)))
                   (y-val (csound--get-number-at-column (1+ y-col))))
              (push `((instrument . ,(format "Inst %s" inst))
                      (x . ,x-val)
                      (y . ,y-val)) data-combined))))))
    (reverse data-combined)))

(defun csound-vega-get-all-unique-instruments ()
  "Scans the buffer for all 'i' numbers."
  (let ((instruments '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^i\\([0-9]+\\)" nil t)
        (add-to-list 'instruments (match-string 1))))
    (reverse instruments)))

(defun csound-vega-harvest-with-ids (inst-list col-idx)
  "Harvests multiple instruments. If INST-LIST contains 'all', it scans everything."
  (let ((data-combined '()))
    (dolist (inst (if (equal inst-list '("all"))
                      (csound-vega-get-all-unique-instruments)
                    inst-list))
      (let ((data-list (csound-vega-harvest (string-to-number inst) col-idx)))
        (dolist (val data-list)
          (push `((instrument . ,(format "Inst %s" inst)) (value . ,val)) data-combined))))
    (reverse data-combined)))

(defun csound-vega-bar (target-inst col-idx x-name)
  "Harvests data and triggers the dedicated bar chart visualizer with a custom X-axis label."
  (interactive "sInstrument (number or 'all'): \nsColumn index: \nsX-Axis Name: ")
  (let* ((inst (if (string= target-inst "all") 'all (string-to-number target-inst)))
         (data (csound-vega-harvest inst (string-to-number col-idx))))
    (if (null data) (message "No data found.")
      (let ((tmp-file "/tmp/csound-data.json"))
        (with-temp-file tmp-file (insert (json-encode (mapcar (lambda (val) `((value . ,val))) data))))
        (start-process "vega-bar" "*Vega Bar*" "/usr/bin/python3"
                       (expand-file-name "~/.emacs.d/lisp/csound-vega/bar_chart.py")
                       tmp-file x-name)))))

(defun csound-vega-stacked-bar (inst-list-string col-idx x-name)
  "Visualizes multiple instruments as a stacked bar chart with custom X-axis label."
  (interactive "sInstruments (comma separated or 'all'): \nsColumn index: \nsX-Axis Name: ")
  (let* ((inst-list (if (string= inst-list-string "all")
                        '("all")
                      (split-string inst-list-string ",")))
         (data (csound-vega-harvest-with-ids inst-list (string-to-number col-idx))))
    (if (null data) (message "No data found.")
      (let ((tmp-file "/tmp/csound-stacked-data.json"))
        (with-temp-file tmp-file (insert (json-encode data)))
        (start-process "vega-stacked" "*Vega Stacked*" "/usr/bin/python3"
                       (expand-file-name "~/.emacs.d/lisp/csound-vega/stacked_bar.py")
                       tmp-file x-name)))))

(defun csound-vega-stacked-none-bar (inst-list-string col-idx x-name)
  "Visualizes multiple instruments as overlapping bars (stack=None) with custom X-axis label."
  (interactive "sInstruments (comma separated or 'all'): \nsColumn index: \nsX-Axis Name: ")
  (let* ((inst-list (if (string= inst-list-string "all")
                        '("all")
                      (split-string inst-list-string ",")))
         (data (csound-vega-harvest-with-ids inst-list (string-to-number col-idx))))
    (if (null data) (message "No data found.")
      (let ((tmp-file "/tmp/csound-stacked-data.json"))
        (with-temp-file tmp-file (insert (json-encode data)))
        (start-process "vega-stacked" "*Vega Stacked*" "/usr/bin/python3"
                       (expand-file-name "~/.emacs.d/lisp/csound-vega/stacked_none_bar.py")
                       tmp-file x-name)))))

(defun csound-vega-grouped-bar (inst-list-string col-idx value-name)
  "Visualizes multiple instruments grouped on the X-axis with side-by-side value bars."
  (interactive "sInstruments (comma separated or 'all'): \nsColumn index: \nsValue Name: ")
  (let* ((inst-list (if (string= inst-list-string "all")
                        (csound-vega-get-all-unique-instruments)
                      (split-string inst-list-string ",")))
         (data (csound-vega-harvest-with-ids inst-list (string-to-number col-idx))))

    (if (null data) (message "No data found.")
      (let ((tmp-file "/tmp/csound-grouped-data.json"))
        (with-temp-file tmp-file (insert (json-encode data)))
        (start-process "vega-grouped" "*Vega Grouped*" "/usr/bin/python3"
                       (expand-file-name "~/.emacs.d/lisp/csound-vega/grouped_bar.py")
                       tmp-file value-name)))))

(defun csound-vega-line (inst-list-string x-col x-name y-col y-name)
  "Visualizes two p-fields as a line chart with custom axis labels."
  (interactive "sInstruments (comma separated or 'all'): \nnX Column index: \nsX-Axis Name: \nnY Column index: \nsY-Axis Name: ")
  (let* ((inst-list (if (string= inst-list-string "all")
                        '("all")
                      (split-string inst-list-string ",")))
         (data (csound-vega-harvest-xy inst-list x-col y-col)))
    (if (null data) (message "No data found.")
      (let ((tmp-file "/tmp/csound-line-data.json"))
        (with-temp-file tmp-file (insert (json-encode data)))
        (start-process "vega-line" "*Vega Line*" "/usr/bin/python3"
                       (expand-file-name "~/.emacs.d/lisp/csound-vega/line_chart.py")
                       tmp-file x-name y-name)))))

(provide 'csound-vega)
