;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((haskell-mode
  (intero-targets "emacs-diary-convert:lib" "emacs-diary-convert:test:emacs-diary-convert-test")
  (compile-command . "make -j 4 test")
  (default-directory . "~/Projects/Emacs/emacs-diary-convert/"))
 (literate-haskell-mode
  (default-directory . "~/Projects/Emacs/emacs-diary-convert/")))


