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

(ert-deftest avsait-ert-haskell-u4t728 ()
  ""
  (avsait-test-point-min
   "{\"id\":\"chatcmpl-68f6c7cf-226e-47f1-a61c-287946f989a0\",\"object\":\"chat.completion\",\"created\":1760036752,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"## Unit Testing for Fibonacci Function in Haskell\n\nTo write unit tests for a Fibonacci function in Haskell, you would typically use the HUnit testing framework. Below is an example of how you can define a Fibonacci function and write unit tests for it.\n\n### Fibonacci Function\n\nFirst, let's define a simple recursive Fibonacci function in Haskell:\n\n```haskell\n-- Fibonacci.hs\nmodule Fibonacci where\n\nfib :: Int -\u003e Int\nfib 0 = 0\nfib 1 = 1\nfib n = fib (n-1) + fib (n-2)\n```\n\n### Unit Tests\n\nNow, let's write some unit tests for this function using HUnit. We will create a separate file for tests.\n\n```haskell\n-- FibonacciTests.hs\nmodule Main where\n\nimport Test.HUnit\nimport Fibonacci\n\n-- Test cases\ntestFib0 = TestCase $ assertEqual \"Fib 0\" 0 (fib 0)\ntestFib1 = TestCase $ assertEqual \"Fib 1\" 1 (fib 1)\ntestFib2 = TestCase $ assertEqual \"Fib 2\" 1 (fib 2)\ntestFib3 = TestCase $ assertEqual \"Fib 3\" 2 (fib 3)\ntestFib4 = TestCase $ assertEqual \"Fib 4\" 3 (fib 4)\ntestFib5 = TestCase $ assertEqual \"Fib 5\" 5 (fib 5)\n\n-- Test suite\ntests = TestList [testFib0, testFib1, testFib2, testFib3, testFib4, testFib5]\n\n-- Run tests\nmain = runTestTT tests\n```\n\n### Explanation\n\n1. **Fibonacci Function (`Fibonacci.hs`):** Defines a simple Fibonacci function.\n2. **Test Module (`FibonacciTests.hs`):**\n   - Imports the `fib` function from `Fibonacci`.\n   - Defines test cases using `TestCase` and `assertEqual`. Each test case checks the result of `fib n` against the expected result for `n = 0, 1, 2, 3, 4, 5`.\n   - Combines test cases into a test suite (`tests`).\n   - Defines a `main` function to run the test suite using `runTestTT`.\n\n### Running the Tests\n\nTo run these tests, you'll need to have GHC (the Glasgow Haskell Compiler) and cabal (the package manager for Haskell) installed on your system. Here’s a brief overview of how to run the tests:\n\n1. Install HUnit if you haven't already: `cabal install HUnit`\n2. Save both `Fibonacci.hs` and `FibonacciTests.hs` in the same directory.\n3. Compile the test file: `ghc --make FibonacciTests.hs`\n4. Run the tests: `./FibonacciTests`\n\nThis will execute the tests and report any failures or errors.\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.135657857,\"prompt_tokens\":42,\"prompt_time\":0.001850732,\"completion_tokens\":621,\"completion_time\":1.465685326,\"total_tokens\":663,\"total_time\":1.4675360579999999},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_34\",\"x_foo\":{\"id\":\"req_01k755v1a0f91r58jsgg5en0y6\"},\"service_tier\":\"on_demand\"}
"
   'emacs-lisp-mode
   'avsait-verbose-p
   (goto-char (point-min))
   (avsait-pretty-print)
   (should (looking-at ".+Haskell$"))))

(ert-deftest avsait-ert-haskell-files-end-BuxN7t ()
  ""
  (avsait-test-point-min
"{\"id\":\"chatcmpl-68f6c7cf-226e-47f1-a61c-287946f989a0\",\"object\":\"chat.completion\",\"created\":1760036752,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"## Unit Testing for Fibonacci Function in Haskell\n\nTo write unit tests for a Fibonacci function in Haskell, you would typically use the HUnit testing framework. Below is an example of how you can define a Fibonacci function and write unit tests for it.\n\n### Fibonacci Function\n\nFirst, let's define a simple recursive Fibonacci function in Haskell:\n\n```haskell\n-- Fibonacci.hs\nmodule Fibonacci where\n\nfib :: Int -\u003e Int\nfib 0 = 0\nfib 1 = 1\nfib n = fib (n-1) + fib (n-2)\n```\n\n### Unit Tests\n\nNow, let's write some unit tests for this function using HUnit. We will create a separate file for tests.\n\n```haskell\n-- FibonacciTests.hs\nmodule Main where\n\nimport Test.HUnit\nimport Fibonacci\n\n-- Test cases\ntestFib0 = TestCase $ assertEqual \"Fib 0\" 0 (fib 0)\ntestFib1 = TestCase $ assertEqual \"Fib 1\" 1 (fib 1)\ntestFib2 = TestCase $ assertEqual \"Fib 2\" 1 (fib 2)\ntestFib3 = TestCase $ assertEqual \"Fib 3\" 2 (fib 3)\ntestFib4 = TestCase $ assertEqual \"Fib 4\" 3 (fib 4)\ntestFib5 = TestCase $ assertEqual \"Fib 5\" 5 (fib 5)\n\n-- Test suite\ntests = TestList [testFib0, testFib1, testFib2, testFib3, testFib4, testFib5]\n\n-- Run tests\nmain = runTestTT tests\n```\n\n### Explanation\n\n1. **Fibonacci Function (`Fibonacci.hs`):** Defines a simple Fibonacci function.\n2. **Test Module (`FibonacciTests.hs`):**\n   - Imports the `fib` function from `Fibonacci`.\n   - Defines test cases using `TestCase` and `assertEqual`. Each test case checks the result of `fib n` against the expected result for `n = 0, 1, 2, 3, 4, 5`.\n   - Combines test cases into a test suite (`tests`).\n   - Defines a `main` function to run the test suite using `runTestTT`.\n\n### Running the Tests\n\nTo run these tests, you'll need to have GHC (the Glasgow Haskell Compiler) and cabal (the package manager for Haskell) installed on your system. Here’s a brief overview of how to run the tests:\n\n1. Install HUnit if you haven't already: `cabal install HUnit`\n2. Save both `Fibonacci.hs` and `FibonacciTests.hs` in the same directory.\n3. Compile the test file: `ghc --make FibonacciTests.hs`\n4. Run the tests: `./FibonacciTests`\n\nThis will execute the tests and report any failures or errors.\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.135657857,\"prompt_tokens\":42,\"prompt_time\":0.001850732,\"completion_tokens\":621,\"completion_time\":1.465685326,\"total_tokens\":663,\"total_time\":1.4675360579999999},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_34\",\"x_foo\":{\"id\":\"req_01k755v1a0f91r58jsgg5en0y6\"},\"service_tier\":\"on_demand\"}
"
   'emacs-lisp-mode
   'avsait-verbose-p
   (message "%s" (current-buffer))
   (goto-char (point-min))
   (should (string= (cadr (avsait--ending-according-to-language (current-buffer))) ".hs" ))))

(ert-deftest avsait-ert-bash-hello-test-BuxN7t ()
  ""
  (avsait-test-point-min
   "{\"id\":\"chatcmpl-f0553702-3767-4ff2-a32c-91570ba02640\",\"object\":\"chat.completion\",\"created\":1779171238,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"```bash\necho \"Hello, World!\"\n```\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.047034077,\"prompt_tokens\":40,\"prompt_time\":0.001684574,\"completion_tokens\":11,\"completion_time\":0.03987366,\"total_tokens\":51,\"total_time\":0.041558234},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_ce7bc1685b\",\"x_groq\":{\"id\":\"req_01krzdx8w4et6s9csnfv2gdrdt\",\"seed\":29370525},\"service_tier\":\"on_demand\"}
"
   'sh-mode
   'avsait-verbose-p
   (avsait--pp-and-language (current-buffer))
   (goto-char (point-min))
   (should (looking-at comment-start))))

(ert-deftest avsait-ert-bash-hello-test-74Az7v ()
  ""
  (avsait-test-point-min
   "{\"id\":\"chatcmpl-f0553702-3767-4ff2-a32c-91570ba02640\",\"object\":\"chat.completion\",\"created\":1779171238,\"model\":\"llama-3.3-70b-versatile\",\"choices\":[{\"index\":0,\"message\":{\"role\":\"assistant\",\"content\":\"```bash\necho \"Hello, World!\"\n```\"},\"logprobs\":null,\"finish_reason\":\"stop\"}],\"usage\":{\"queue_time\":0.047034077,\"prompt_tokens\":40,\"prompt_time\":0.001684574,\"completion_tokens\":11,\"completion_time\":0.03987366,\"total_tokens\":51,\"total_time\":0.041558234},\"usage_breakdown\":null,\"system_fingerprint\":\"fp_ce7bc1685b\",\"x_groq\":{\"id\":\"req_01krzdx8w4et6s9csnfv2gdrdt\",\"seed\":29370525},\"service_tier\":\"on_demand\"}
"
   'sh-mode
   'avsait-verbose-p
   (avsait--pp-and-language (current-buffer))
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
   (avsait--pp-and-language (current-buffer))
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
   (avsait--pp-and-language (current-buffer))
   (goto-char (point-max))
   (skip-chars-backward " \t\r\n\f")
   (beginning-of-line)
   (sit-for 0.1) 
   (should (looking-at comment-start))))


(provide 'avsait-ert-pretty-print-tests)
;;; avsait-ert-pretty-print-tests.el ends here
