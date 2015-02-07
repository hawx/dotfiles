(define-generic-mode 'vodka-mode
  '(";")                                ; Comments start with a ';'
  '()                                   ; List of keywords
  '((":[^\s]+" . 'font-lock-constant-face)
    ("\".*?\"" . 'font-lock-string-face)
    ("'.*?'" . 'font-lock-string-face)
    ("true" . 'font-lock-variable-name-face)
    ("false" . 'font-lock-variable-name-face)
    ("nil" . 'font-lock-variable-name-face))
  '("\\.vk$")                           ; Files to activate for
  nil                                   ; Other functions to call
  "A mode for vodka files"              ; Doc string
  )

(provide 'vodka-mode)
