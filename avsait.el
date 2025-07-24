;;; avsait.el --- A very simple AI query tool -*- lexical-binding: t; -*-

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

;; Usage: Define your query-commands providing arguments as string like this:

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

;;; Code:

(require 'avsait-api)
(require 'avsait-config)
(require 'avsait-secrets)

(defun avsait-toggle-no_cleanup ()
  "Toggle use of electric colon for Python code."
  (interactive)
  (setq avsait-no-cleanup-p (not avsait-no-cleanup-p))
  (when (and avsait-verbose-p (called-interactively-p 'interactive)) (message "avsait-no-cleanup-p: %s" avsait-no-cleanup-p)))

(defalias 'avsait-open-input-file 'avsait-input-file)
(defun avsait-input-file()
  "Open the input file"
  (interactive)
  (find-file avsait-input-file))

(defun avsait-toggle-read-from-input-file ()
  "Avsait toggle ‘read-from-input-file-p’ value. "
  (interactive)
  (setq avsait-read-from-input-file-p (not avsait-read-from-input-file-p))
  (when (called-interactively-p 'interactive) (message "avsait-read-from-input-file-p: %s" avsait-read-from-input-file-p)))

(defun avsait-toggle-debug-p ()
  "Toggle ‘avsait-debug-p’ value."
  (interactive)
  (setq-local avsait-debug-p (not avsait-debug-p))
  (when (called-interactively-p 'interactive) (message "avsait-debug-p: %s" avsait-debug-p)))

(defun avsait-toggle-verbose-p ()
  "Toggle ‘avsait-verbose-p’ value."
  (interactive)
  (setq-local avsait-verbose-p (not avsait-verbose-p))
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

(defun avsait-cleanup()
  "Cleanup the output-buffer."
  (interactive "*")
  (let (erg previous-line-was-empty)
    (switch-to-buffer (current-buffer))
    (goto-char (point-min))
    (save-excursion
      (while (search-forward "\\n" nil t 1)
        (replace-match "")
        (newline 1)))
    (save-excursion
      (while (search-forward "\\t" nil t 1)
        (replace-match "	")))
    (save-excursion
      (while (re-search-forward "```\\([[:alpha:]]+\\)" nil t 1)
        (setq erg (match-string-no-properties 1))
        (when (member erg (list "haskell"))
          (replace-match "```")
          (delete-horizontal-space)
          (newline 1)
          (insert erg)
          (newline 1)
          (setq erg nil))))
    (save-excursion
      (while (re-search-forward "``` " nil t 1)
        (replace-match "")
        (newline 1)
        (insert "```")
        (newline 1)))
    (save-excursion
      (while (re-search-forward "\\\\u003[ce]" nil t 1)
        (replace-match ">")))
    (save-excursion
      (while (re-search-forward "I hope that helps!.+" nil t 1)
        (replace-match "")))
    (save-excursion
      (while (search-forward "\\\\" nil t 1)
        (delete-char -1)(forward-char 1)))
    (when (looking-at "{\"id\":.+\"content\":\"")
      (delete-region (match-beginning 0) (match-end 0)))
    (save-excursion (while (search-forward "\\\""nil t 1)
                      (replace-match "\"")))
    (save-excursion (while (search-forward "**"nil t 1)
                      (replace-match "")))
    (save-excursion (while (re-search-forward "\\/\$"nil t 1)
                      (replace-match "")))
    ;; (save-excursion (while (re-search-forward "\\\\n\\|\\\\t"nil t 1)
    ;;                   (delete-char -2)
    ;;                   (newline 1)))
    (save-excursion (when (search-forward "\"},\"logprobs\"" nil t 1)
                      (delete-region (match-beginning 0) (point-max))))
    (save-excursion (while (and (not (eobp)) (re-search-forward "$" nil t 1)(eolp))
                      ;; (sit-for 0.1)
                      (when (eq (char-before) 92)
                        ;; (sit-for 0.1)
                        (delete-char -1))
                      (unless (eobp) (forward-line 1))))
    (save-excursion
      (while (re-search-forward "^ *[0-9]+\\." nil t 1)
        (beginning-of-line)
        (split-line)
        (forward-line 2)))
    (save-excursion
      (while (re-search-forward "^ *[0-9]+\\." nil t 1)
        (fill-paragraph)
        (forward-line 2)))
    ;; (save-excursion (while (not (eobp))
    ;;                   (unless (eq (char-after) ?*)(fill-paragraph))
    ;;                   (forward-paragraph)
    ;;                   (skip-chars-forward " \t\r\n\f")))
    (save-excursion
      (goto-char (point-min))
      (while (not (eobp))
        (if (looking-at "\\([ \t]*\\)$")
            (if previous-line-was-empty
                (delete-char 1)
              (setq previous-line-was-empty t)
              (forward-line 1))
          (setq previous-line-was-empty nil)
          (forward-line 1))))))

(defun avsait (arg api key &optional model text)
  "Query LLM.
Argument ARG With \\[universal-argument] read from input-file, not from minibuffer.

See also customizable ‘avsait-read-from-input-file-p’ and
‘avsait-toggle-read-from-input-file’, which would reverse that behavior.

API: which endpoint to access.
KEY: the token provided by the API
MODEL: the LLM
TEXT: the query when called from a program"
  (interactive "P")
  ;; (unless (eq 4 (prefix-numeric-value arg))
  ;; (find-file avsait-input-file))
  (let* ((text (cond (text)
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
         (modes (or model "llama-3.3-70b-versatile"))
         (start (if (string-match " " text)
                    (+ 1 (string-match " " text))
                  0))
         (outbut-buffer-init-text (capitalize (substring text 0 (and (string-match "[^ ]+ +[^ ]+" text start) (match-end 0)))))
         (output-buffer (if (not (string= "" avsait-output-buffer))
                            avsait-output-buffer
                          (concat (replace-regexp-in-string "[^[:alnum:]_]" "" (concat outbut-buffer-init-text (make-temp-name "_"))) ".text"))))
    (shell-command (concat "curl " api " \
-H \"Content-Type: application/json\" \
-H \"Authorization: Bearer " key "\" \
-d '{
\"model\": \"" model "\",
\"messages\": [{
    \"role\": \"user\",
    \"content\": \"" text ".\"
}]
}'
") output-buffer)
    ;; (sit-for 1)
    (set-buffer output-buffer)
    (delete-other-windows)
    (when
        avsait-debug-p
      (write-file (expand-file-name (concat avsait-output-dir "/debug_" output-buffer))))
    (unless avsait-no-cleanup-p
      (avsait-cleanup))
    (write-file (expand-file-name (concat avsait-output-dir "/" output-buffer)))
    (switch-to-buffer output-buffer)))

(provide 'avsait)
;;; avsait.el ends here
