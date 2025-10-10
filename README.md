Querying LLMs from Emacs

Check for command ‘avsait’:

Reads prompts from file or minibuffer, according to configuration or calling argument.

Writes output-buffer into directory according to configuable ‘avsait-output-dir’

Rename ‘avsait-secrets-example.el’ to ‘avsait-secrets.el’ and edit the required token according to your provider.

According to customizable value of ‘avsait-read-from-input-file-p’ user gets prompted or input-file is taken.

There is a command ‘avsait-toggle-read-from-input-file’ for switching on the fly.

‘avsait-read-current-as-input-file’ would send the current file to the LLM as given in avsait-config.el 

There it may read:

(defun groq (&optional arg)
  (interactive \"P\")
  (avsait arg \"https://api.groq.com/openai/v1/chat/completions -s\" GROQ_TOKEN \"meta-llama/llama-4-scout-17b-16e-instruct\"))

This is Beta and susceptible of any changes.
