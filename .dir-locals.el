((elm-mode
  (elm-interactive-command . ("elm" "repl"))
  (elm-compile-command . ("elm" "make"))
  (elm-compile-arguments . ("--output=priv/static/main.js" "--debug"))
  (elm-package-command . ("elm" "package"))
  (elm-package-json . "elm.json")))
