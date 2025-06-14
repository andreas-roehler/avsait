;;; avsait-config.el --- Configuring avsait -*- lexical-binding: t; -*-

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

;;; Comment: Some example configuration

;;; Code:

(defcustom avsait-output-dir ""
  "If set, the result will be stored in that directory. If left empty, no storage."
  :type 'string
  :tag "avsait-output-dir"
  )

(defcustom avsait_input-file ""
  "In case, input should be read from a file, define its location here.
If left empty, input is read from minibuffers prompt.

See also ‘avsait_read-from-input-file-p’"
  :type 'string
  :tag "avsait_input-file")

(defcustom avsait-output-buffer ""
  "By default, avsait will create a new dedicated buffer for each query.
If you wish a default `buffer-name' instead, specify it here."
  :type 'string
  :tag "avsait-output-buffer")

(defcustom avsait_read-from-input-file-p nil
  "If prompt-texts should be read from an input-file instead from minibuffer.
Default is nil."
  :type 'boolean
  :tag "avsait_read-from-input-file-p")

(defcustom avsait_no_cleanup-p nil
  "Leave output as received, no cleanup.
Default is nil."
  :type 'boolean
  :tag "avsait_no_cleanup-p")

(defalias 'gq 'groq)
(defun groq (&optional arg)
  (interactive "P")
  (avsait arg "https://api.groq.com/openai/v1/chat/completions -s" groq_pw "llama-3.3-70b-versatile"))

(defalias 'gq4 'groq4)
(defun groq4 (&optional arg)
  (interactive "P")
  (avsait arg "https://api.groq.com/openai/v1/chat/completions -s" groq_pw "meta-llama/llama-4-scout-17b-16e-instruct"))

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

(provide 'avsait-config)
;;; avsait-config.el ends here
