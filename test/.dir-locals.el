(
 (haskell-mode
  .
  (
   (haskell-process-args-stack-ghci . ("--ghci-options=-ferror-spans"
                                       "--no-build"
                                       "--no-load"
                                       "cpp-notebook-language-server:lib"
                                       "cpp-notebook-language-server:exe:cpp-notebook-language-server"
                                       "cpp-notebook-language-server:test:cpp-notebook-language-server-test"
                                       )))
  )
 )
