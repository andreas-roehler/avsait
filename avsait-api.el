;;; avsait-api.el --- API's for a very simple AI query tool -*- lexical-binding: t; -*-

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

;;; Commentary: API's to get responses from AI-bots into an Emacs buffer.

;;; Code

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

;; (defun groqDeepSeedDatei (&optional arg)
;;   "Call Groq with DeepSeek and read prompts from file."
;;   (interactive)
;;   (avsait arg  "https://api.groq.com/openai/v1/chat/completions -s" deepSeek_pw "DeepSeek-R1-Distill-Llama-70b"))


;;; avsait-api.el ends here
(provide 'avsait-api)
