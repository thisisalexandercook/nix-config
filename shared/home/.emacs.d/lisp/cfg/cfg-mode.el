(define-generic-mode 'cfg-mode
  '("//" "#")                       ;; 1. Comment starters (C++ or Shell style)
  '("GOAL" "epsilon")               ;; 2. Keywords (Special grammar markers)
  '(
    ;; The Arrow "->" (High visibility)
    ("->" . 'font-lock-builtin-face)

    ;; LHS Definitions: Any word at the start of a line
    ("^[a-zA-Z0-9]+" . 'font-lock-function-name-face)

    ;; TERMINALS: Strictly ALL CAPS (e.g. CLASS, PUBLIC, LBRACE)
    ;; Regex: Word boundary, Uppercase/Numbers/Underscore only, Word boundary
    ("\\b[A-Z_][A-Z0-9_]*\\b" . 'font-lock-constant-face)

    ;; Non-Terminals: MixedCase words in the RHS
    ;; Regex: Starts with Upper, contains lower (e.g. CompilationUnit)
    ("\\b[A-Z][a-zA-Z0-9]*[a-z][a-zA-Z0-9]*\\b" . 'font-lock-variable-name-face)
   )
  (list "\\.cfg\\'")               ;; 3. Auto-load for .cfg files
  nil                               ;; 4. No special function hook needed
  "Major mode for editing Joos CFG files.")
