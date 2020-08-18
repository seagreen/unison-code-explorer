; Use with this in your Emacs config:
;
; (use-package reformatter)
; (reformatter-define ormolu-format
;   :program "ormolu")

((haskell-mode
   (mode . ormolu-format-on-save)))
