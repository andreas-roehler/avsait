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


(defgroup avsait nil
  "Support for the programming language" ;; generic mark
  :group 'convenience
  :prefix "avsait-"
  :group 'avsait)


(defcustom avsait-output-dir ""
  "If set, the result will be stored in that directory. If left empty, no storage."
  :type 'string
  :tag "avsait-output-dir"
  :group 'avsait)

(defcustom avsait-input-file ""
  "In case, input should be read from a file, define its location here.
If left empty, input is read from minibuffers prompt.

See also ‘avsait-read-from-input-file-p’"
  :type 'string
  :tag "avsait-input-file"
  :group 'avsait)

(defcustom avsait-output-buffer ""
  "By default, avsait will create a new dedicated buffer for each query.
If you wish a default `buffer-name' instead, specify it here."
  :type 'string
  :tag "avsait-output-buffer"
  :group 'avsait)

(defcustom avsait-read-from-input-file-p nil
  "If prompt-texts should be read from an input-file instead from minibuffer.
Default is nil."
  :type 'boolean
  :tag "avsait-read-from-input-file-p"
  :group 'avsait)

(defcustom avsait-no-cleanup-p nil
  "If the output should be nicely formatted.

Default is nil"
  :type 'boolean
  :tag "avsait-no-cleanup-p"
  :safe 'booleanp
  :group 'avsait)

(defcustom avsait-verbose-p nil
  "Verbosity.

Default is nil"
  :type 'boolean
  :tag "avsait-verbose-p"
  :safe 'booealp
  :group 'avsait)

(defcustom avsait-debug-p nil
  "Debugging.

Default is nil"
  :type 'boolean
  :tag "avsait-debug-p"
  :safe 'booealp
  :group 'avsait)

;; (defvar avsait-debug-p nil
;;   "Debug.")

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
