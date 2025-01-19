;;; avsait.el --- A very simple AI query tool

;" This program is free software" you can redistribute it and/or modify
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
;; By default output is stored in customizable ‘avsait-ml-output-dir’

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

(defcustom avsait-ml-output-dir ""
  "If set, the result will be stored in that directory. If left empty, no storage."
  :type 'string
  :tag "avsait-ml-output-dir"
  )

(defcustom avsait_input-file ""
  "In case, input should be read from a file, define its location here.
  If left empty, input is read from minibuffers prompt.

See also ‘avsait_read-from-input-file-p’"
  :type 'string
  :tag "avsait_input-file")

(defcustom avsait-output-buffer ""
  "By default, avsait will create a new dedicated buffer for each query.
If you wish a default buffer-name instead, specify it here."
  :type 'string
  :tag "avsait-output-buffer")

(defcustom avsait_read-from-input-file-p nil
  "If prompt-texts should be read from an input-file instead from minibuffer.
Default is nil.
"
  :type 'boolean
  :tag "avsait_read-from-input-file-p")

(defcustom avsait_no_cleanup-p nil
  "Leave output as received, no cleanup.
Default is nil.
"
  :type 'boolean
  :tag "avsait_no_cleanup-p")

(defun avsait-toggle-read-from-input-file ()
  "Toggle use of electric colon for Python code."
  (interactive)
  (setq avsait_read-from-input-file-p (not avsait_read-from-input-file-p))
  (when (and ar-verbose-p (called-interactively-p 'interactive)) (message "avsait_read-from-input-file-p: %s" avsait_read-from-input-file-p)))

(unless (symbolp 'ar-verbose-p)
  (defcustom ar-verbose-p nil
    "Verbosity."
    :type 'boolean
    :tag "ar-verbose-p"))

(defun avsait-cleanup()
  "Cleanup the output-buffer."
  (interactive "*")
  (goto-char (point-min))
  (save-excursion
    (while (search-forward "\\\\" nil t 1)
      (delete-char -1)(forward-char 1)))
  (when (looking-at "{\"id\":.+\"content\":\"")
    (delete-region (match-beginning 0) (match-end 0)))
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

(defun avsait (arg api key model &optional text)
  "Query LLM.
Argument ARG With \\[universal-argument] read from input-file, not from minibuffer.

See also customizable ‘avsait_read-from-input-file-p’ and
‘avsait-toggle-read-from-input-file’, which would reverse that behavior.

API: which endpoint to access.
KEY: the token provided by the API
MODEL: the LLM"
  (interactive "P")
  ;; (unless (eq 4 (prefix-numeric-value arg))
  ;; (find-file avsait_input-file))
  (let* ((text (or text
                   (if (and (or avsait_read-from-input-file-p (eq 4 (prefix-numeric-value arg)))
                            (not (string= "" avsait_input-file)))
                       (progn (find-file avsait_input-file)
                              (replace-regexp-in-string "\\\n\\|\\\t" "" (buffer-substring-no-properties (point-min) (point-max))))
                     (read-from-minibuffer "Eingabe: " (car kill-ring)))))
         (start (if (string-match " " text)
                    (+ 1 (string-match " " text))
                  0))
         (outbut-buffer-init-text (substring text 0 (string-match " " text start)))
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
    (switch-to-buffer output-buffer)
    (write-file (expand-file-name (concat avsait-ml-output-dir "/" output-buffer)))
    (switch-to-buffer output-buffer)
    (delete-other-windows)
    (unless avsait_no_cleanup-p
      (avsait-cleanup))))

(provide 'avsait)
;;; avsait.el ends here
