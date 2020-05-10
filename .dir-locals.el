; Use with this in your Emacs config:
;
; (use-package reformatter)
; (reformatter-define ormolu-format-0.0.5.0
;   :program "ormolu-0.0.5.0"
;   :args '("--ghc-opt" "-XBangPatterns"
;           "--ghc-opt" "-XTypeApplications"))

((haskell-mode
   (mode . ormolu-format-0.0.5.0-on-save)))
