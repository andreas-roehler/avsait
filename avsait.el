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
;; See also customizable ‘avsait_read-from-input-file-p’ and
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

(require 'avsait-config)
(require 'avsait-secrets)

(defun avsait-toggle-no_cleanup ()
  "Toggle use of electric colon for Python code."
  (interactive)
  (setq avsait_no_cleanup-p (not avsait_no_cleanup-p))
  (when (and ar-verbose-p (called-interactively-p 'interactive)) (message "avsait_no_cleanup-p: %s" avsait_no_cleanup-p)))

(defalias 'avsait-open-input-file 'avsait-input-file)
(defun avsait-input-file()
  "Open the input file"
  (interactive)
  (find-file avsait_input-file))

(defun avsait-toggle-read-from-input-file ()
  "Avsait toggle ‘read-from-input-file-p’ value .

If off, with \\[universal-argument] gets prompted for a file"
  (interactive)
  (setq avsait_read-from-input-file-p (not avsait_read-from-input-file-p))
  (when (and ar-verbose-p (called-interactively-p 'interactive)) (message "avsait_read-from-input-file-p: %s" avsait_read-from-input-file-p)))

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
  (switch-to-buffer (current-buffer))
  (goto-char (point-min))
  (save-excursion
    (while (re-search-forward "\\\\u003[ce]" nil t 1)
      (replace-match ">")))
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
  (save-excursion (while (re-search-forward "\\\\n\\|\\\\t"nil t 1)
                    (delete-char -2)
                    (newline 1)))
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
  (save-excursion (while (not (eobp))
                    (unless (eq (char-after) ?*)(fill-paragraph))
                    (forward-paragraph)
                    (skip-chars-forward " \t\r\n\f"))))

(defun avsait (arg api key &optional model text)
  "Query LLM.
Argument ARG With \\[universal-argument] read from input-file, not from minibuffer.

See also customizable ‘avsait_read-from-input-file-p’ and
‘avsait-toggle-read-from-input-file’, which would reverse that behavior.

API: which endpoint to access.
KEY: the token provided by the API
MODEL: the LLM
TEXT: the query when called from a program"
  (interactive "P")
  ;; (unless (eq 4 (prefix-numeric-value arg))
  ;; (find-file avsait_input-file))
  (let* ((text (or text
                   (if (and (or avsait_read-from-input-file-p (eq 4 (prefix-numeric-value arg)))
                            (not (string= "" avsait_input-file)))
                       (progn (find-file avsait_input-file)
                              (replace-regexp-in-string "\\\n\\|\\\t" "" (buffer-substring-no-properties (point-min) (point-max))))
                     (read-from-minibuffer "Eingabe: " (car kill-ring)))))
         (modes (or model "llama-3.3-70b-versatile"))
         (start (if (string-match " " text)
                    (+ 1 (string-match " " text))
                  0))
         (outbut-buffer-init-text (substring text 0 (and (string-match "[^ ]+ +[^ ]+" text start) (match-end 0))))
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
    (unless avsait_no_cleanup-p
      (avsait-cleanup))
    (write-file (expand-file-name (concat avsait-output-dir "/" output-buffer)))
    (switch-to-buffer output-buffer)))

(defalias 'gq 'groq)
(defun groq (&optional arg)
  (interactive "P")
  (avsait arg "https://api.groq.com/openai/v1/chat/completions -s" groq_pw "llama-3.3-70b-versatile"))

(defalias 'ds 'groqDeepSeek)
(defun groqDeepSeek (&optional arg)
  (interactive "P")
  (avsait arg "https://api.groq.com/openai/v1/chat/completions -s" deepSeek_pw "DeepSeek-R1-Distill-Llama-70b"))

;; DeepSeek-R1-Distill-Llama-70b
(defun chatGPT (&optional arg)
  (interactive "P")
  (avsait arg "https://chatgpt.com/" chatGPT_pw))

(defun groqDeepSeedDatei()
  "Call Groq with DeepSeek and read prompts from file."
  (interactive)
  (avsait '(4) "https://api.groq.com/openai/v1/chat/completions -s" deepSeek_pw "DeepSeek-R1-Distill-Llama-70b"))

(provide 'avsait)
;;; avsait.el ends here
