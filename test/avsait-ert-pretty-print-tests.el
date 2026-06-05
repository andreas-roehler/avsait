;;; avsait-ert-pretty-print-tests.el --- python-mode navigation tests  -*- lexical-binding: t; -*-

;;
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(require 'avsait-setup-ert-tests)

;; (setq avsait-verbose-p t)

(ert-deftest avsait-ert-bash-hello-test-BuxN7t ()
  ""
  (avsait-test-point-min
   "{\"id\":\"chatcmpl-f0553702-3767-4ff2-a32c-91570ba02640\",\"object\":\"chat.completion\",\"created\":1779171238,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"```bash\necho \"Hello, World!\"\n```\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.047034077,\"prompt_tokens\":40,\"prompt_time\":0.001684574,\"completion_tokens\":11,\"completion_time\":0.03987366,\"total_tokens\":51,\"total_time\":0.041558234},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_ce7bc1685b\",\"x_groq\":{\"id\":\"req_01krzdx8w4et6s9csnfv2gdrdt\",\"seed\":29370525},\"service_tier\":\"on_demand\"}
"
   'sh-mode
   'avsait-verbose-p
   (avsait--pp-and-language (current-buffer) t)
   (goto-char (point-min))
   (should (looking-at comment-start))))

(ert-deftest avsait-ert-bash-hello-test-74Az7v ()
  ""
  (avsait-test-point-min
   "{\"id\":\"chatcmpl-f0553702-3767-4ff2-a32c-91570ba02640\",\"object\":\"chat.completion\",\"created\":1779171238,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"```bash\necho \"Hello, World!\"\n```\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.047034077,\"prompt_tokens\":40,\"prompt_time\":0.001684574,\"completion_tokens\":11,\"completion_time\":0.03987366,\"total_tokens\":51,\"total_time\":0.041558234},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_ce7bc1685b\",\"x_groq\":{\"id\":\"req_01krzdx8w4et6s9csnfv2gdrdt\",\"seed\":29370525},\"service_tier\":\"on_demand\"}
"
   'sh-mode
   'avsait-verbose-p
   (avsait--pp-and-language (current-buffer) t)
   (goto-char (point-min))
   (search-forward "echo")
   (end-of-line)
   (should-not (nth 4 (parse-partial-sexp (point-min) (point))))))

(ert-deftest avsait-ert-bash-hello-test-qlRNUw ()
  ""
  (avsait-test-point-min
   "{\"id\":\"chatcmpl-f0553702-3767-4ff2-a32c-91570ba02640\",\"object\":\"chat.completion\",\"created\":1779171238,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"```bash\necho \"Hello, World!\"\n```\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.047034077,\"prompt_tokens\":40,\"prompt_time\":0.001684574,\"completion_tokens\":11,\"completion_time\":0.03987366,\"total_tokens\":51,\"total_time\":0.041558234},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_ce7bc1685b\",\"x_groq\":{\"id\":\"req_01krzdx8w4et6s9csnfv2gdrdt\",\"seed\":29370525},\"service_tier\":\"on_demand\"}
"
   'sh-mode
   'avsait-verbose-p
   (avsait--pp-and-language (current-buffer) t)
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (beginning-of-line)
   (should (looking-at comment-start))))

(ert-deftest avsait-ert-bash-test-8p8mGk ()
  ""
  (avsait-test-point-min
   "{\"id\":\"chatcmpl-f0553702-3767-4ff2-a32c-91570ba02640\",\"object\":\"chat.completion\",\"created\":1779171238,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"```bash\necho \"Hello, World!\"\n```\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.047034077,\"prompt_tokens\":40,\"prompt_time\":0.001684574,\"completion_tokens\":11,\"completion_time\":0.03987366,\"total_tokens\":51,\"total_time\":0.041558234},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_ce7bc1685b\",\"x_groq\":{\"id\":\"req_01krzdx8w4et6s9csnfv2gdrdt\",\"seed\":29370525},\"service_tier\":\"on_demand\"}
"
   'sh-mode
   'avsait-verbose-p
   (avsait--pp-and-language (current-buffer) t)
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (beginning-of-line)
   (sit-for 0.1)
   (should (looking-at comment-start))))

(provide 'avsait-ert-pretty-print-tests)
;;; avsait-ert-pretty-print-tests.el ends here
