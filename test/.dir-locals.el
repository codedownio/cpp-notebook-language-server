(
 (haskell-mode
  .
  (
   (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans"
                                       "--no-build"
                                       "--no-load"
                                       "rust-notebook-language-server:lib"
                                       "rust-notebook-language-server:exe:rust-notebook-language-server"
                                       "rust-notebook-language-server:test:rust-notebook-language-server-test"
                                       )))
  )
 )
