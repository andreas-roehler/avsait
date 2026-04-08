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

(require 'avsait-api)
(require 'avsait-config)

(defgroup avsait nil
  "Question LLMs" ;; generic mark
  :group 'convenience
  :prefix "avsait-")

(defcustom avsait-allow-special-edits-p nil
 "If true, admonitions by the model are deleted from the response.

Default is nil"
 :type 'boolean
 :group 'avsait)

(when (and (getenv "IFLOCAL") (eq 0 (getenv "IFLOCAL")))
  (require 'avsait-secrets))

(defun avsait-format-paragraphs ()
  (interactive "*")
  (goto-char (point-min))
  (while (re-search-forward "^*" nil t 1)
    (when (search-forward ":" (line-end-position) 1)
      (newline 2)
      (skip-chars-forward " \t\r\n\f")
      (indent-according-to-mode)
      (fill-paragraph)))
  (goto-char (point-min))
  (while (prog1 (not (eobp)) (forward-paragraph))
      (save-excursion
	(skip-chars-backward " \t\r\n\f")
	(fill-paragraph)))
  )

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

(defun avsait--ajust-templates ()
  ""
  (interactive "*")
  (goto-char (point-min))
  (while (re-search-forward "^| *\\([[:alnum:]]+\\) *" nil t 1)
    (message "%s" (match-string 1))

    ))

(defun avsait--result-in-language-mode (&optional orig this-mode first second)
  "If some code was request, store the result in the respective mode."
  (interactive "*")
  (unless (eobp)
    (let ((orig (or orig (point-min)))
          (this-mode this-mode)
          (first first)
          (second second)
          erg)
      (goto-char orig)
      (cond (first
             (goto-char (point-min))
             (comment-region
              (and (setq erg (re-search-forward "^```" nil 'move 1))
                   ;; match the closing triple
                   ;; (not (looking-at "\\([[:graph:]]+\\)\\(.*\\)"))
                   (match-beginning 0))
              ;; at the closing
              (and (or (and (re-search-forward "^```" nil 'move 1)
                            ;; match possible new opening triple
                            ;; (not (looking-at "\\([[:graph:]]+\\)\\(.*\\)"))
                            ;; (match-end 0)
                            (line-end-position)
                            )
                       (point-max)))
              )
             (avsait--result-in-language-mode (point) this-mode first second))
            ;; (t (comment-region orig (point-max)))
            ((and (re-search-forward "^```" nil 'move 1)
                  (looking-at "\\([[:graph:]]+\\)\\(.*\\)")
                  (setq erg (match-end 1))
                  (or
                   (member (concat (match-string-no-properties 1) "-mode") known-emacs-modes)
                   (member (concat "js-" (match-string-no-properties 1) "-mode") known-emacs-modes)
                   (member (match-string-no-properties 1) known-emacs-modes)
             (setq this-mode (pcase (match-string-no-properties 1)
                               ("bash" "sh-mode")
                               ("elisp" "emacs-lisp-mode")
                               ("emacs" "emacs-lisp-mode")
                               ("json" "js-json-mode")
                               (_ (concat (match-string-no-properties 1) "-mode"))))))
             (when (and first (string= this-mode "emacs-lisp-mode"))
               (goto-char (point-min))
               (insert "-*- lexical-binding: t; -*-")(newline 1) (setq first t))
             (funcall (car (read-from-string this-mode)))
             (goto-char erg) (skip-chars-forward " \t\r\n\f") (newline 1)
             (comment-region orig (point))
             (setq first t)
             (avsait--result-in-language-mode (point) this-mode first second))
            ;; ((and (re-search-forward "^```" nil 'move 1)
            ;;       (not (looking-at "\\([[:graph:]]+\\)\\(.*\\)")))
            ;;  (comment-region (match-beginning 0) (match-end 0))
            ;;  (avsait--result-in-language-mode (point) this-mode))
            ;; (t (comment-region orig (point-max)))
            ))))

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

(defun avsait-pretty-print--tabs ()
  (save-excursion
    (while (search-forward "\\t" nil t 1)
      (replace-match "	"))))

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
    (when (eq (char-after) ?\")
      (delete-char 1)
      )
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (when (eq (char-before) ?})
      (delete-char -1)
      )
    (when (eq (char-before) ?\")
      (delete-char -1)
      )
    ))

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

(defun avsait-pretty-print--remove-doublestars ()
  (save-excursion (while (search-forward "**"nil t 1)
                    (replace-match ""))))

(defun avsait-pretty-print--content ()
  (interactive "*")
  (save-excursion (when (search-forward "\"content\":" nil t 1)
                    (newline 2))))

(defun avsait-pretty-print--keywords ()
  (interactive "*")
  (save-excursion (while (re-search-forward (regexp-opt (list
                                                         "id"
                                                         "index"
                                                         "logprobs"
                                                         "role"
                                                         "usage"
                                                         )
                                                        'symbols)
                                            nil t 1)
                    (delete-region (line-beginning-position) (line-end-position)))))

(defun avsait-pretty-print--single-paren ()
  (interactive "*")
  (save-excursion (while (re-search-forward "^[]\\[{}]$" nil t 1)
                    (delete-region (line-beginning-position) (line-end-position)))))

(defun avsait-pretty-print--remove-backslash-at-EOL ()
  (save-excursion (while (and (not (eobp)) (re-search-forward "$" nil t 1)(eolp))
                    (when (eq (char-before) 92)
                      (delete-char -1))
                    (unless (eobp) (forward-line 1)))))

(defun avsait-pretty-print ()
  "Cleanup the output-buffer."
  (interactive "*")
  (let (erg previous-line-was-empty)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (avsait-pretty-print--newlines-when-nest)
    (avsait-pretty-print--newlines)
    (avsait-pretty-print--tabs)
    ;; (save-excursion
    ;;   (while (re-search-forward "```\\([[:alpha:]]+\\)" nil t 1)
    ;;     (setq erg (match-string-no-properties 1))))
    (avsait-pretty-print--greater-than)
    (avsait-pretty-print--i-hope)
    (avsait-pretty-print--delete-backlashes)
    (avsait-pretty-print--unescape-doublequotes)
    (avsait-pretty-print--remove-doublestars)
    (avsait-pretty-print--remove-backslash-at-EOL)
    (avsait-pretty-print--content)
    (avsait-pretty-print--keywords)
    (avsait-pretty-print--single-paren)
    (avsait-just-one-empty-line)
    (avsait-pretty-start-end-spaces)
    (avsait-pretty-print--enclosing-braces)
    (when avsait-allow-special-edits-p (avsait--special-edits))
    (avsait--ajust-templates)
    (save-excursion
      (while (re-search-forward "^ *[0-9]+\\." nil t 1)
        (beginning-of-line)
        (split-line)
        (forward-line 2)))
    (save-excursion
      (while (re-search-forward "^ *[0-9]+\\." nil t 1)
        (fill-paragraph)
        (forward-line 2)))
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
          (forward-line 1))))
    (save-excursion
      (goto-char (point-min))
      ;; line ends with opening paren
      (while (re-search-forward "($" nil t 1)
        (delete-char 1)))

    ))

(defun ending-according-to-language (output-buffer)
  ""
  (interactive
   (list (current-buffer)))
  (with-current-buffer output-buffer
    (goto-char (point-min))
    (when (re-search-forward "```\\([[:alpha:]]+\\)" nil t 1)
        (pcase  (match-string-no-properties 1)
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
          ))))

(defun avsait--write-debug-output (output-buffer)
  ""
  (save-excursion
        (with-current-buffer
            (set-buffer (get-buffer-create (concat "/debug_" output-buffer)))
          (switch-to-buffer (current-buffer))
          (insert-buffer output-buffer)
          (write-file (expand-file-name (concat avsait-output-dir "/debug_" output-buffer))))
        ))

;; (defun avsait-pretty-print-current-buffer ()
;;   "Helper command."
;;   (interactive "*")
;;   (let ((debugfile (buffer-name (current-buffer)))
;;         erg)
;;     (avsait-pretty-print)
;;     (when (setq erg (ending-according-to-language (current-buffer)))
;;       (avsait--result-in-language-mode))
;;     (write-file (expand-file-name (concat avsait-output-dir "/" (replace-regexp-in-string "^debug_" "" (buffer-name (current-buffer))) (or erg ".text"))))
;;     (unless avsait-debug-p (shell-command (concat "rm " debugfile)))))

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
  (let* ((text (cond (test)
                     (text)
                     (;; current-buffer
                      (eq 4 (prefix-numeric-value arg))
                      (replace-regexp-in-string "\\\n\\|\\\t" "" (buffer-substring-no-properties (point-min) (point-max))))
                     ((and (or avsait-read-from-input-file-p (eq 4 (prefix-numeric-value arg)))
                           (not (string= "" avsait-input-file)))
                      (progn (find-file (expand-file-name avsait-input-file))
                             (with-current-buffer (get-file-buffer avsait-input-file)
                               (message "%s" (get-file-buffer avsait-input-file))
                               ;; (message "%s" (buffer-name avsait-input-file)))
                               (replace-regexp-in-string "\\\n\\|\\\t" "" (buffer-substring-no-properties (point-min) (point-max))))))
                     (t (read-from-minibuffer "Eingabe: " (car kill-ring)))))
         (model (or model "llama-3.3-70b-versatile"))
         (start (if (string-match " " text)
                    (+ 1 (string-match " " text))
                  0))
         (outbut-buffer-init-text (or test (capitalize (substring text 0 (and (string-match "[^ ]+ +[^ ]+" text start) (match-end 0))))))
         (output-buffer (or test (if (not (string= "" avsait-output-buffer))
                                     avsait-output-buffer
                                   ;; (concat (replace-regexp-in-string "[^[:alnum:]_]" "" (concat outbut-buffer-init-text (make-temp-name "_"))) ".text")
                                   (replace-regexp-in-string "[^[:alnum:]_]" "" (concat outbut-buffer-init-text (make-temp-name "_"))))))
         erg)
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
    (set-buffer output-buffer)
    (delete-other-windows)
    (when
        avsait-debug-p
      (avsait--write-debug-output output-buffer))
    (when avsait-pretty-print-p
      (avsait-pretty-print)
      (when (setq erg (ending-according-to-language output-buffer))
        (avsait--result-in-language-mode)))
    (when (member major-mode (list 'text-mode 'org-mdoe))
      (avsait-format-paragraphs))
    (write-file (expand-file-name (concat avsait-output-dir "/" output-buffer (or erg ".org"))))
    (switch-to-buffer (concat output-buffer (or erg ".org")))))

(provide 'avsait)
;;; avsait.el ends here
