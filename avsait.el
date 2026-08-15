;;; avsait.el --- A very simple AI query tool -*- lexical-binding: t; -*-

;; URL: https://github.com/andreas-roehler/avsait
;; Keywords: lisp, convenience

;; This program is free software" you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary: Get responses from AI-bots into an Emacs buffer.

;; Output gets an unique name related to a buffer, that way subsequent answers don't get lost.
;; By default output is stored in customizable ‘avsait-output-dir’

;; Usage:

;; Rename ‘avsait-secrets-example.el’ to ‘avsait-secrets.el’ and edit
;; the required token according to your provider.

;; Define your query-commands providing arguments as string like this:

;; (defun my_query (arg)
;;   "With \\[universal-argument] read from input-file, not from minibuffer.
;; See also customizable ‘avsait-read-from-input-file-p’ and
;; ‘avsait-toggle-read-from-input-file’, which would reverse that behavior.

;; API: which endpoint to access.
;; KEY: the token provided by the API
;; MODEL: the LLM"
;;   (interactive "P")
;;   (let ((api YOUR_API)
;;         (key YOUR_ACCESS-TOKEN)
;;         (model YOUR_LLM-MODEL-TO-USE))
;;     (avsait arg api key model text)))

;; According to customizable value of ‘avsait-read-from-input-file-p’
;; user gets prompted or input-file is taken. There is a command
;; ‘avsait-toggle-read-from-input-file’ switching that the fly.

;;; Code:

;; (require 'avsait-api)
(require 'table)
(require 'avsait-config)
(require 'avsait-secrets)

(when (and (getenv "IFLOCAL") (eq 0 (getenv "IFLOCAL")))
  (require 'avsait-secrets))

(defgroup avsait nil
  "Question LLMs" ;; generic mark
  :group 'convenience
  :prefix "avsait-")

(defcustom avsait-allow-special-edits-p nil
 "If true, admonitions by the model are deleted from the response.

Default is nil"
 :type 'boolean
 :group 'avsait)

(defcustom avsait-format-paragraphs-p t
  "Disable formatting paragraphs if convenient.

Default is ‘t’"
  :type 'boolean
  :group 'avsait)

(defcustom avsait-cell-length 9
  "Maximal length of cells: or transforms into multi-line"
  :type 'int
  :group 'avsait)

(defun avsait--table-narrow-cell (n)
  "Narrow the current cell by N columns and shrink the cell horizontally.
Some other cells in the same table are narrowed as well to keep the
table's rectangle structure."
  (interactive "*p")
  (if (< n 0) (setq n 1))
  (table--finish-delayed-tasks)
  (let* ((coord-list (table--cell-list-to-coord-list (table--vertical-cell-list)))
	 (current-cell (table--cell-to-coord (table--probe-cell)))
	 (current-coordinate (table--get-coordinate))
	 tmp-list)
    (message "Narrowing...");; this operation may be lengthy
    ;; determine the doable n by try narrowing each cell.
    (setq tmp-list coord-list)
    (while tmp-list
      (let ((cell (prog1 (car tmp-list) (setq tmp-list (cdr tmp-list))))
	    (table-inhibit-update t)
	    cell-n)
	(table--goto-coordinate (car cell))
	(table-recognize-cell 'force)
	(table-with-cache-buffer
          ;; (switch-to-buffer (current-buffer))
	  (table--fill-region (point-min) (point-max) (- table-cell-info-width n))
	  (if (< (setq cell-n (- table-cell-info-width (table--measure-max-width))) n)
	      (setq n cell-n))
	  (erase-buffer)
	  (setq table-inhibit-auto-fill-paragraph t))))
    (if (< n 1) nil
      ;; narrow only the contents of each cell but leave the cell frame as is because
      ;; we need to have valid frame structure in order for table-with-cache-buffer
      ;; to work correctly.
      (setq tmp-list coord-list)
      (while tmp-list
	(let* ((cell (prog1 (car tmp-list) (setq tmp-list (cdr tmp-list))))
	       (table-inhibit-update t)
	       (currentp (equal cell current-cell))
	       old-height)
	  (if currentp (table--goto-coordinate current-coordinate)
	    (table--goto-coordinate (car cell)))
	  (table-recognize-cell 'force)
	  (setq old-height table-cell-info-height)
	  (table-with-cache-buffer
            ;; (switch-to-buffer (current-buffer))
	    (let ((out-of-bound (>= (- (car current-coordinate) (car table-cell-info-lu-coordinate))
				    (- table-cell-info-width n)))
		  (sticky (and currentp
			       (save-excursion
				 (unless (bolp) (forward-char -1))
				 (looking-at ".*\\S ")))))
	      (table--fill-region (point-min) (point-max) (- table-cell-info-width n))
	      (if (or sticky (and currentp (looking-at ".*\\S ")))
		  (setq current-coordinate (table--transcoord-cache-to-table))
		(if out-of-bound (setcar current-coordinate
					 (+ (car table-cell-info-lu-coordinate) (- table-cell-info-width n 1))))))
	    (setq table-inhibit-auto-fill-paragraph t))
	  (table--update-cell 'now)
	  ;; if this cell heightens and pushes the current cell below, move
	  ;; the current-coordinate (point location) down accordingly.
	  (if currentp (setq current-coordinate (table--get-coordinate))
	    (if (and (> table-cell-info-height old-height)
		     (> (cdr current-coordinate) (cdr table-cell-info-lu-coordinate)))
		(setcdr current-coordinate (+ (cdr current-coordinate)
					      (- table-cell-info-height old-height)))))
	  ))
      ;; coord-list is now possibly invalid since some cells may have already
      ;; been heightened so recompute them by table--vertical-cell-list.
      (table--goto-coordinate current-coordinate)
      (setq coord-list (table--cell-list-to-coord-list (table--vertical-cell-list)))
      ;; push in the affected area above and below this table so that things
      ;; on the right side of the table are shifted horizontally neatly.
      (table--horizontally-shift-above-and-below (- n) (reverse coord-list))
      ;; finally narrow the frames for each cell.
      (let* ((below-list nil)
	     (this-list coord-list)
	     (above-list (cdr coord-list)))
	(while this-list
	  (let* ((below (prog1 (car below-list) (setq below-list (if below-list (cdr below-list) coord-list))))
		 (this (prog1 (car this-list) (setq this-list (cdr this-list))))
		 (above (prog1 (car above-list) (setq above-list (cdr above-list)))))
	    (delete-rectangle
	     (table--goto-coordinate
	      (cons (- (cadr this) n)
		    (if (or (null above) (<= (cadr this) (cadr above)))
			(1- (cdar this))
		      (cdar this))))
	     (table--goto-coordinate
	      (cons (cadr this)
		    (if (or (null below) (< (cadr this) (cadr below)))
			(1+ (cddr this))
		      (cddr this)))))))))
    (table--goto-coordinate current-coordinate)
    ;; re-recognize the current cell's new dimension
    (setq erg (table-recognize-cell 'force))
    (message "%s" erg)
    (message "")))

(defun avsait--org-table-convert-and-align ()
  ""
  (interactive "*")
  (let ((orig (copy-marker (point)))
        done)
    (goto-char (point-min))
    (while (re-search-forward "^ *|" nil t 1)
      (beginning-of-line)
      (setq orig (point))
      (save-restriction
        (narrow-to-region
         (or (and (bobp) (point))(- (line-beginning-position) 1))
         (progn (while (and (forward-line 1) (looking-at "^ *|")))(point)))
        (goto-char orig)
        (while (re-search-forward "^ *|" nil t 1)
          (skip-chars-forward "^|")
          (backward-char)
          ;; (search-forward "UI)")
          (when (org-at-table-p)
            (org-table-align)
            (org-table-convert)
            )
          ;; (search-forward "Model")
          (skip-chars-forward "^|")
          (when (eq (char-after) ?|)
            (forward-char 1))
          ;; (setq orig (point))
          (while (and (not done)(not (eobp)))
            (when (string-match "|" (buffer-substring-no-properties (point) (line-end-position)))
              (avsait--table-narrow-cell avsait-cell-length))
            (skip-chars-forward "^|")
            (when (eq (char-after) ?|)
              (forward-char 1)))
          ;; (unless (< (progn (skip-chars-forward "^|")(point)) orig)
          ;;   (setq done t))
          )))
    ))



(defun avsait--leerzeile-org-kapitel ()
  (interactive "*")
  (goto-char (point-min))
  (while (re-search-forward "^\\*" nil t 1)
    (save-excursion
      (forward-line -1)
      (beginning-of-line)
      (unless (or (ar-empty-line-p)(bobp))
        (end-of-line)
        (newline 1)))))

(defun just-one-empty-line (&optional beg end)
  "Delete consecutive empty lines, retain just one.

Works on region if active.
Accepts optional arguments BEG END to specify a region"
  (interactive "*")
  (let ((beg (cond (beg)
		   ((region-active-p)
		    (region-beginning))
		   (t (point-min))))
	(end (copy-marker
              (cond (end)
		    ((region-active-p)
		     (region-end))
		    (t (point-max)))))
        previous-line-was-empty)
    (save-excursion
      (save-restriction
        (narrow-to-region beg end)
        (goto-char beg)
        (while (not (eobp))
          (if (looking-at "\\([ \t]*\\)$")
              (if previous-line-was-empty
                  (delete-char 1)
                (setq previous-line-was-empty t)
                (forward-line 1))
            (setq previous-line-was-empty nil)
            (forward-line 1)))
        ;; (je-ein-leerzeichen-im-bereich beg end)
	(widen))))
  (when (eq major-mode 'org-mode)
    (save-excursion
      (avsait--leerzeile-org-kapitel))))

(defun avsait--format-paragraphs-intern (at-program fill-command)
  (cond (at-program
         (when (looking-at comment-start)
           (funcall fill-command)))
        (t
         (funcall fill-command))))

(defun avsait-format-paragraphs (&optional at-program)
  "With optional AT-PROGRAM format comments only. "
  (interactive "*")
  (goto-char (point-min))
  (let ((paragraph-start (concat paragraph-start "\\|" "# =+[ \t]*$"))
        (paragraph-separate (concat paragraph-separate "\\|" "# [=-]+[ \t]*$"))
        (orig (point))
        (fill-command (cond ((and (eq major-mode 'python-mode)
                                  ;; check for python-mode.el
                                  (functionp 'py-fill-paragraph))
                             'py-fill-paragraph)
                            (t 'fill-paragraph)))
        ;; (plain (unless at-program (save-excursion (search-forward "```plain" nil t))))
        done)
    (while (progn
             (skip-chars-forward " \t\r\n\f")
             (save-restriction
               (narrow-to-region (point) (point-max))
               ;; (if plain
               ;; don't format a paragraph after ```plain
               (cond (done
                      ;; dont (format when set
                      (setq done nil)
                      (forward-paragraph)
                      (skip-chars-forward " \t\r\n\f"))
                     ((looking-at "^|\\|\\+-")
                      ;; table, dont't (format
                      (forward-paragraph)
                      (skip-chars-forward " \t\r\n\f"))
                     ((looking-at "```plain")
                      (setq done t)
                      (avsait--format-paragraphs-intern at-program fill-command))
                     (t (avsait--format-paragraphs-intern at-program fill-command)
                        (forward-paragraph)))
               ;; (avsait--format-paragraphs-intern at-program fill-command)
               ;; )
               (and (<  orig (point))
                    (setq orig (point))))))
    (goto-char (point-min))
    (while (re-search-forward "^*" nil t 1)
      (when (search-forward ":" (line-end-position) 1)
        (newline 2)
        (skip-chars-forward " \t\r\n\f")
        (indent-according-to-mode)
        (avsait--format-paragraphs-intern at-program fill-command)))
    (goto-char (point-min))
    (while (re-search-forward "^-" nil t 1)
      (avsait--format-paragraphs-intern at-program fill-command))
    (just-one-empty-line)))

(defun avsait-just-one-empty-line (&optional beg end)
  "Delete consecutive empty lines, retain just one.

Works on region if active.
Accepts optional arguments BEG END to specify a region"
  (interactive "*")
  (let ((beg (cond (beg)
		   ((region-active-p)
		    (region-beginning))
		   (t (point-min))))
	(end (copy-marker
              (cond (end)
		    ((region-active-p)
		     (region-end))
		    (t (point-max)))))
        previous-line-was-empty)
    (goto-char beg)
    (while (not (eobp))
      (if (looking-at "\\([ \t]*\\)$")
          (if previous-line-was-empty
              (delete-char 1)
            (setq previous-line-was-empty t)
            (forward-line 1))
        (setq previous-line-was-empty nil)
        (forward-line 1)))))

(defun avsait-pretty-start-end-spaces ()
  ""
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (let ((orig (point)))
      (when (< 0 (abs (skip-chars-forward " \t\r\n\f")))
        (delete-region orig (point) ))
      (goto-char (point-max))
      (when (< 0 (abs (skip-chars-backward " \t\r\n\f")))
        (delete-region (point-max) (point))))))

(defun avsait-toggle-pretty-print ()
  "Toggle use of electric colon for Python code."
  (interactive)
  (setq avsait-pretty-print-p (not avsait-pretty-print-p))
  (when (and avsait-verbose-p (called-interactively-p 'interactive)) (message "avsait-pretty-print-p: %s" avsait-pretty-print-p)))

(defalias 'avsait-open-input-file 'avsait-input-file)
(defun avsait-find-input-file()
  "Open the input file"
  (interactive)
  (find-file avsait-input-file))

(defun avsait-toggle-read-from-input-file ()
  "Avsait toggle ‘read-from-input-file-p’ value. "
  (interactive)
  (setq avsait-read-from-input-file-p (not avsait-read-from-input-file-p))
  (when (called-interactively-p 'interactive) (message "avsait-read-from-input-file-p: %s" avsait-read-from-input-file-p)))

(defalias 'avsait-read-input-from-current-file 'avsait-read-current-as-input-file)
(defun avsait-read-current-as-input-file ()
  "Next avsait will read the current file as input-file.

Sets ‘avsait-read-from-input-file-p’ and ‘avsait-input-file’"
  (interactive)
  (setq avsait-read-from-input-file-p t)
  (setq avsait-input-file (buffer-file-name))
  (when avsait-verbose-p (message "avsait reads from current file: %s" (file-name-nondirectory avsait-input-file)))
  )

(defun avsait-toggle-debug-p ()
  "Toggle ‘avsait-debug-p’ value."
  (interactive)
  (setq avsait-debug-p (not avsait-debug-p))
  (when (called-interactively-p 'interactive) (message "avsait-debug-p: %s" avsait-debug-p)))

(defun avsait-toggle-verbose-p ()
  "Toggle ‘avsait-verbose-p’ value."
  (interactive)
  (setq avsait-verbose-p (not avsait-verbose-p))
  (when (and avsait-verbose-p (called-interactively-p 'interactive)) (message "avsait-verbose-p: %s" avsait-verbose-p)))

(defun avsait--highlight-match ()
  "Used inside avsait-current2output-dir."
  (push-mark)
  (back-to-indentation)
  (exchange-point-and-mark))

(defalias 'avsait-switch-output-dir 'avsait-current2output-dir)
(defun avsait-current2output-dir ()
  "Make the current dired directory the ‘avsait-output-dir’.

Writes value of new ‘avsait-output-dir’ into ‘custom-file’
An alternative to ‘M-x customize-variable ...’ "
  (interactive)
  (let ((exchange-point-and-mark-highlight-region t)
        (erg (expand-file-name default-directory)))
    (load custom-file)
    (find-file custom-file)
    (goto-char (point-min))
    (if (search-forward avsait-output-dir nil t)
        (if (string= erg (match-string-no-properties 0))
            (progn
              (message "%s already current value" erg)
              (avsait--highlight-match))
          (replace-match erg)
          (avsait--highlight-match)
          (when (yes-or-no-p "Write custom-file?") (write-file custom-file)))
      (error (concat "Can't see " custom-file)))))

(defvar known-emacs-modes (list "ada-mode" "asm-mode" "awk-mode" "cc-mode" "clojure-mode" "css-mode" "elisp" "emacs-lisp" "erlang-mode" "forth-mode" "fortran-mode" "go-mode" "haskell-mode" "html-mode" "java-mode" "js-mode" "js-json-mode" "julia-mode" "latex-mode" "lisp-mode" "lua-mode" "makefile-mode" "matlab-mode" "perl-mode" "php-mode" "python" "python-mode" "r-mode" "ruby-mode" "rust-mode" "scala-mode" "scheme-mode" "sh-mode" "shell-mode" "sql-mode" "swift-mode" "tcl-mode" "tex-mode" "tuareg-mode" "verilog-mode" "vhdl-mode" "web-mode"
)
  "Known Emacs modes")

(defun avsait--determine-language-mode ()
  "Returns the corresponding Emacs mode, if existing."
  (interactive)
  (and (search-forward "```" nil t 1)
       (looking-at "[[:graph:]]+")
       (member (concat (match-string-no-properties 0) "-mode") known-emacs-modes))
  (match-string-no-properties 0) "-mode")

(defun avsait--adjust-templates ()
  ""
  (interactive "*")
  (goto-char (point-min))
  (while (re-search-forward "^| *\\([[:print:]]+\\) *" nil t 1)
    (message "%s" (match-string 1))

    ))

(defun avsait--result-in-language-mode (res &optional orig this-mode beg end)
  "If some code was delivered, store the result in the respective mode.

ARG RES: the first match of some code section"
  (interactive "*")
  (unless (eobp)
    (save-restriction
      (let* ((orig (copy-marker (or orig (point-min))))
             (this-mode-raw (or this-mode (car res)))
             (this-mode (pcase this-mode-raw
                          ("bash" "sh")
                          ("elisp" "emacs-lisp")
                          ("emacs" "emacs-lisp")
                          ("json" "js-json")
                          ("scheme" "scheme")
                          ("yml" (or
                                  (and
                                   (featurep (car (read-from-string (concat "yaml" "-ts-mode"))))
                                   (concat "yaml" "-ts-mode"))
                                  (concat "yaml" "-mode")))
                          (_
                           (cond
                            ((featurep (car (read-from-string (concat this-mode-raw "-ts-mode"))))
                             (concat this-mode-raw "-ts-mode"))
                            ((featurep (car (read-from-string (concat this-mode-raw "-mode"))))
                             this-mode-raw)
                            (t "text")))))
             (beg (or beg (point-min)))
             (end (copy-marker (or end (point-max)))))
        ;; if not at BOB, the previous section doesn't belong to a specific mode
        ;; so let's apply text
        (unless (bobp)
          (save-excursion
            (goto-char (point-min))
            (insert "#+begin_src text")
            (newline 1))
          (beginning-of-line)
          (end-of-line)
          (newline 1)
          (insert "#+end_src")
          (newline 1)
          (save-excursion
            (indent-region (progn (forward-line -2)(end-of-line)(point))(progn (search-backward "#+begin_src text")(+ (line-end-position) 1)))))
        (save-restriction
          (newline 1)
          (narrow-to-region (point) end)
          ;; at the first mode match
          (insert (concat "#+begin_src " this-mode))
          (when (re-search-forward "```" nil 'move 1)
            (replace-match "#+end_src"))
          (newline 1)
          (save-restriction
            (narrow-to-region (point) (point-max))
            (if (re-search-forward "```\\([[:alpha:]]+\\)" nil t 1)
                (avsait--result-in-language-mode (avsait--ending-according-to-language (current-buffer)) orig this-mode beg end)
              (unless
                  (eobp)
                ;;  just plain text below
                (insert "#+begin_src text")
                (newline 1)
                (goto-char (point-max))
                 (newline 1)
                (insert "#+end_src")
                ))))))))

(defun avsait--special-edits ()
  (when (looking-at "{\"id\":.+\"content\":\"")
    (delete-region (match-beginning 0) (match-end 0)))
  (save-excursion (when (search-forward "\"},\"logprobs\"" nil t 1)
                    (delete-region (match-beginning 0) (point-max))))
  (save-excursion (when (re-search-forward "^Es ist wichtig, dass " nil t 1)
                    (delete-region (match-beginning 0) (progn (goto-char (match-end 0))(skip-chars-forward "^.")(+ (point) 1)))))
  )

(defun avsait-pretty-print--newlines-when-nest ()
  (interactive "*")
  (let ((orig (point)))
    (save-excursion
      (while (progn (ignore-errors (down-list))
                    (< orig (point)))
        (save-excursion
          (backward-char)
          (forward-sexp)
          (newline 1))
        (newline 1)
        (setq orig (point))))))

(defun avsait-pretty-print--newlines ()
  (save-excursion
    (while (search-forward "\\n" nil t 1)
      (replace-match "")
      (newline 1))))

(defun avsait-pretty-print--star-after-newline ()
  (save-excursion
    (while (search-forward "\\n*" nil t 1)
      (replace-match "\n-"))))

(defun avsait-pretty-print--bash-prompt ()
  (save-excursion
    (while (search-forward "bash\\n#" nil t 1)

      (let ((beg (match-beginning 0))
            (end (save-excursion
                   (search-forward "```" nil t))))
        (when end
          (goto-char beg)
          (while (search-forward "\\n#" end t 1)
            (replace-match "\n>"))
)))))

(defun avsait-pretty-print--org-fill-paragraph ()
  (save-excursion
    (while (re-search-forward "^- " nil t 1)
      (org-fill-paragraph))))

(defun avsait-pretty-print--triple-backtics ()
  (save-excursion
    (while (re-search-forward "```" nil t 1)
      (beginning-of-line)
      (newline 1)
      (end-of-line)
      (newline 1))))

(defun avsait-pretty-print--tabs ()
  (save-excursion
    (while (search-forward "\\t" nil t 1)
      (replace-match "  "))))

;; not ready yet
(defun avsait-pretty-print--table ()
  ""
  (interactive "*")
  (save-excursion
    (let ((counter 1))
    (while (re-search-forward (concat comment-start "|") nil t 1)
      (save-excursion
      (forward-line -1)
      (insert (concat "#+name: table_" (prin1-to-string counter)))
      (setq counter (+ 1 counter)))
      (forward-word 1)
      (indent-for-tab-command)
      ;; (call-interactively (kbd "TAB"))
      ))))

(defun avsait-pretty-print--backticks (lang)
  ""
  (interactive "*")
  (save-excursion
    (while (re-search-forward (concat "```"lang) nil t 1)
      (replace-match lang)))
  (save-excursion
    (while (re-search-forward (concat "^" comment-start-skip "```$") nil t 1)
      (replace-match "")))
  )

(defun avsait-pretty-print--greater-than ()
  (save-excursion
    (while (re-search-forward "\\\\u003[ce]" nil t 1)
      (replace-match ">"))))

(defun avsait-pretty-print--enclosing-braces ()
  ""
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (when (eq (char-after) ?{)
      (delete-char 1))
    ;; (when (eq (char-after) ?\")
    ;; (delete-char 1)
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (when (eq (char-before) ?})
      (delete-char -1))
    ;; (when (eq (char-before) ?\")
    ;; (delete-char -1)
    ))

(defun avsait-pretty-print--start ()
  ""
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "content\":" nil t)
      (save-excursion
      (goto-char (nth 1 (parse-partial-sexp (point-min) (point))))
        (forward-sexp)
        (delete-region (point) (point-max)))
      (delete-region (point) (point-min)))))


(defun avsait-pretty-print--tldr ()
  ""
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "TL;DR" nil t 1)
      (delete-region (point-min) (match-end 0)))))

(defun avsait-pretty-print--i-hope ()
  (save-excursion
    (while (re-search-forward "I hope that helps!.+" nil t 1)
      (replace-match ""))))

(defun avsait-pretty-print--delete-backlashes ()
  (save-excursion
    (while (search-forward "\\\\" nil t 1)
      (delete-char -1)(forward-char 1))))

(defun avsait-pretty-print--unescape-doublequotes ()
  (save-excursion (while (search-forward "\\\""nil t 1)
                    (replace-match "\""))))

(defun avsait-pretty-print--enumerations ()
  (save-excursion (while (re-search-forward "^#? ?[2-9]+\\." nil t 1)
                    (beginning-of-line)
                    (newline 1)
                    (end-of-line))))

(defun avsait-pretty-print--remove-doublestars ()
  (save-excursion (while (search-forward "**"nil t 1)
                    (replace-match ""))))

(defun avsait-pretty-print--content ()
  (interactive "*")
  (save-excursion (when (search-forward "\"content\":" nil t 1)
                    (newline 2))))

(defun avsait-pretty-print--end ()
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    (when (search-forward "queue_time\":" nil t)
                    (save-excursion
                      (goto-char (nth 1 (parse-partial-sexp (point-min) (point)) ))
                      (backward-sexp)
                      (delete-region (point) (point-max))))))

(defun avsait-pretty-print--single-paren ()
  (interactive "*")
  (save-excursion (while (re-search-forward "^[\\[{]" nil t 1)
                    (backward-char)
                    (save-excursion
                      (forward-sexp)
                      (backward-delete-char-untabify 1))
                    (delete-char 1))))

(defun avsait-pretty-print--remove-backslash-at-EOL ()
  (save-excursion (while (and (not (eobp)) (re-search-forward "$" nil t 1)(eolp))
                    (when (eq (char-before) 92)
                      (delete-char -1))
                    (unless (eobp) (forward-line 1)))))

(defun avsait--adjust-newlines ()
  (save-excursion
    (while (re-search-forward "^ *[0-9]+\\." nil t 1)
      (beginning-of-line)
      (split-line)
      (forward-line 2))))

(defun avsait--adjust-paragraphs ()
  (save-excursion
    (while (re-search-forward "^ *[0-9]+\\." nil t 1)
      (fill-paragraph)
      (forward-line 2))))

(defun avsait--delete-consecutive-empty-lines ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      ;; delete consecutive empty lines
      (if (looking-at "\\([ \t]*\\)$")
          (if previous-line-was-empty
              (delete-char 1)
            (setq previous-line-was-empty t)
            (forward-line 1))
        (setq previous-line-was-empty nil)
        (forward-line 1)))))

(defun avsait--fix-end-of-lines ()
  (save-excursion
    (goto-char (point-min))
    ;; line ends with opening paren
    (while (re-search-forward "($" nil t 1)
      (delete-char 1))))

(defun avsait--fix-ampersand ()
  "\u0026"
  (interactive "*")
  (save-excursion
    (goto-char (point-min))
    ;; line ends with opening paren
    (while (search-forward "\\u0026" nil t 1)
      (replace-match "&"))))

(defun avsait-pretty-print ()
  "Cleanup the output-buffer."
  (interactive "*")
  (let (erg previous-line-was-empty)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    ;; (avsait-pretty-print--enclosing-braces)
    ;; (avsait-pretty-print--tldr)
    (avsait-pretty-print--start)
    (avsait-pretty-print--bash-prompt)
    (avsait-pretty-print--star-after-newline)
    (avsait--fix-ampersand)
    (avsait-pretty-print--newlines-when-nest)
    (avsait-pretty-print--newlines)
    (avsait-pretty-print--triple-backtics)
    (avsait-pretty-print--tabs)
    (avsait-pretty-print--greater-than)
    ;; (avsait-pretty-print--backticks)
    (avsait-pretty-print--i-hope)
    (avsait-pretty-print--delete-backlashes)
    (avsait-pretty-print--unescape-doublequotes)
    (avsait-pretty-print--remove-doublestars)
    (avsait-pretty-print--remove-backslash-at-EOL)
    (avsait-pretty-print--content)
    (avsait-pretty-print--single-paren)
    (avsait-pretty-print--enumerations)
    (avsait-just-one-empty-line)
    (avsait-pretty-start-end-spaces)
    (when avsait-allow-special-edits-p (avsait--special-edits))
    ;; (avsait--adjust-templates)
    (avsait--adjust-newlines)
    (avsait--adjust-paragraphs)
    (avsait--delete-consecutive-empty-lines)
    (avsait--fix-end-of-lines)
    (avsait-pretty-print--end)
))

(defun avsait--ending-according-to-language (output-buffer)
  ""
  (interactive
   (list (current-buffer)))
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (when (re-search-forward "```\\([[:alpha:]]+\\)" nil t 1)
      (list (match-string-no-properties 1)
            (pcase (match-string-no-properties 1)
              ("ada" ".ada")
              ("assembly" ".asm")
              ("awk" ".awk")
              ("bash" ".sh")
              ("c" ".c")
              ("c++" ".cpp")
              ("clojure" ".clj")
              ("common" ".lisp")
              ("css" ".css")
              ("elisp" ".el")
              ("emacs" ".el")
              ("erlang" ".erl")
              ("forth" ".f")
              ("fortran" ".f90")
              ("go" ".go")
              ("haskell" ".hs")
              ("html" ".html")
              ("java" ".java")
              ("javascript" ".js")
              ("json" ".json")
              ("julia" ".jl")
              ("latex" ".tex")
              ("lisp" ".el")
              ("lua" ".lua")
              ("makefile" ".mak")
              ("matlab" ".m")
              ("objective-c" ".h")
              ("ocaml" ".ml")
              ("php" ".php")
              ("perl" ".pl")
              ("python" ".py")
              ("r" ".r")
              ("ruby" ".rb")
              ("rust" ".rs")
              ("scala" ".scala")
              ("scheme" ".scm")
              ("shell" ".sh")
              ("sql" ".zsh")
              ("swift" ".sql")
              ("tcl" ".swift")
              ("tex" ".tcl")
              ("vh-----dl" ".tex")
              ("verilog" ".vhd")
              ("vue.js" ".v")
              ("yaml"  ".yml")
              ("yml"  ".yml")
              )))))

(defun avsait--write-debug-output (output-buffer)
  ""
  (save-excursion
        (with-current-buffer
            (set-buffer (get-buffer-create (concat "/debug_" output-buffer)))
          (switch-to-buffer (current-buffer))
          (insert-buffer output-buffer)
          (write-file (expand-file-name (concat avsait-output-dir "/debug_" output-buffer))))
        ))

(defun avsait--pp-and-language (output-buffer &optional test)
  ""
  (interactive
   (list (current-buffer)))
  (when avsait-pretty-print-p
    (avsait-pretty-print)
    (let ((lang-and-ending (avsait--ending-according-to-language output-buffer)))
      (unless test
        (write-file (expand-file-name
                     (concat avsait-output-dir "/" (replace-regexp-in-string "^debug_" ""
                                                                             (buffer-name (current-buffer)))
                             ".org"))))
      (avsait--org-table-convert-and-align)
      (when lang-and-ending
        ;; first match of ``` is reached
        (avsait--result-in-language-mode lang-and-ending))
      (when avsait-format-paragraphs-p
        ;; (unless lang-and-ending
        (goto-char (point-min))

        (if (member major-mode (list 'fundamental-mode 'org-mode" 'text-mode"))
            (progn
              (save-excursion
              (avsait-pretty-print--org-fill-paragraph))
              (save-excursion (avsait-format-paragraphs)))
          (save-excursion (avsait-format-paragraphs t))
          (save-excursion (avsait-pretty-print--backticks (car lang-and-ending))))
        (avsait-just-one-empty-line)))
    (unless test
      (write-file (buffer-file-name)))
    ;; (expand-file-name
    ;;  (concat avsait-output-dir "/" (replace-regexp-in-string "^debug_" ""
    ;;                                                          (buffer-name (current-buffer)))
    ;;                     ".org"))
    ))
                           ;; (if lang-and-ending
                           ;;     (cadr lang-and-ending)
                           ;;   (pcase major-mode
                           ;;     (`python-mode ".py")
                           ;;     (_ ".org")))
                           ;; ))))))

(defun avsait--read-input-file (file)
  ""
  (interactive
   (list (current-buffer)))
  (find-file (expand-file-name file))
  (when avsait-verbose-p (message "%s" (concat "loading " file)))
  (goto-char (point-min))
  (save-excursion
  (while (re-search-forward "\\\n\\|\\\t" nil t 1)
    (replace-match " ")))
  (save-excursion
    (while (re-search-forward "\"\\([^\"]+\\)\"" nil t 1)
      (replace-match (concat "‘" (match-string-no-properties 1) "’")))))

(defun avsait (arg api key &optional model text test role)
  "Query LLM.
Argument ARG With \\[universal-argument] read from input-file, not from minibuffer.

See also ‘avsait-read-input-from-current-file.’

API: which endpoint to access.
KEY: the token provided by the API
MODEL: the LLM
TEXT: the query when called from a program"
  (interactive "P")
  ;; (unless (eq 4 (prefix-numeric-value arg))
  ;; (find-file avsait-input-file))
  (let* ((text (cond ((and (stringp arg)
                           arg))
                     (test)
                     (text)
                     (;; current-buffer
                      (eq 4 (prefix-numeric-value arg))
                      (replace-regexp-in-string "\\\n\\|\\\t" "" (buffer-substring-no-properties (point-min) (point-max))))
                     ((and (or avsait-read-from-input-file-p (eq 4 (prefix-numeric-value arg)))
                           (not (string= "" avsait-input-file)))
                      (avsait--read-input-file avsait-input-file))
                     ;; (progn (find-file (expand-file-name avsait-input-file))
                     ;;        (with-current-buffer (get-file-buffer avsait-input-file)
                     ;;          (message "%s" (get-file-buffer avsait-input-file))
                     ;;          ;; (message "%s" (buffer-name avsait-input-file)))
                     ;;          (replace-regexp-in-string "\\\n\\|\\\t" " " (buffer-substring-no-properties (point-min) (point-max))))))
                     (t (read-from-minibuffer "Eingabe: " (car kill-ring)))))
         (neutext text)
         (model (or model "openai/gpt-oss-120b"))
         (start (point-min)
                ;; (if (string-match " " text)
                ;;           (+ 1 (string-match " " text))
                ;;         0)
                )
         (outbut-buffer-init-text (or test (capitalize (substring text 0 (and (string-match "[^ ]+ +[^ ]+" text start) (match-end 0))))))
         (output-buffer (or test (if (not (string= "" avsait-output-buffer))
                                     avsait-output-buffer
                                   ;; (concat (replace-regexp-in-string "[^[:alnum:]_]" "" (concat outbut-buffer-init-text (make-temp-name "_"))) ".text")
                                   (replace-regexp-in-string "[^[:alnum:]_]" "" (concat outbut-buffer-init-text (make-temp-name "_"))))))
         )
    (or test (shell-command (concat "curl " api " \
-H \"Content-Type: application/json\" \
-H \"Authorization: Bearer " key "\" \
-d '{
\"model\": \"" model "\",
\"messages\": [
               {\"role\": \"user\",\"content\": \"" text ".\"}

              ]
    }'
") output-buffer))
    ;; (sit-for 1)
    (with-current-buffer output-buffer
      (delete-other-windows)
      (when
          avsait-debug-p
        (avsait--write-debug-output output-buffer))
      (avsait--pp-and-language output-buffer)
      ;; (avsait-format-paragraphs)

      ;; (when (buffer-live-p (concat output-buffer (or erg ".org")))
        ;; (switch-to-buffer (concat output-buffer (or erg ".org"))))
      )))

(provide 'avsait)
;;; avsait.el ends here
