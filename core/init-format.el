;;; init-format.el --- format setting -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

(leaf editorconfig
  :blackout t
  :hook (prog-mode-hook . editorconfig-mode))

(leaf format-all
  :doc "deps: inheritenv language-id"
  :hook (prog-mode-hook . format-all-ensure-formatter)
  :commands format-all-buffer
  :init
  ;; silent ensure message
  (advice-add #'format-all-ensure-formatter :around #'silent-message-advice)

  (setq format-all-debug t)
  (setq format-all-default-formatters
        '(("Assembly" asmfmt)
          ("ATS" atsfmt)
          ("Bazel" buildifier)
          ("BibTeX" bibtex-mode)
          ("C" clang-format)
          ("C#" clang-format)
          ("C++" clang-format)
          ("Cabal Config" cabal-fmt)
          ("Clojure" cljfmt)
          ("CMake" cmake-format)
          ("Crystal" crystal)
          ("CSS" prettier)
          ("D" dfmt)
          ("Dart" dartfmt)
          ("Dhall" dhall)
          ("Dockerfile" dockfmt)
          ("Elixir" mix-format)
          ("Elm" elm-format)
          ("Emacs Lisp" emacs-lisp)
          ("Fish" fish-indent)
          ("GLSL" clang-format)
          ("Go" gofmt)
          ("GraphQL" prettier)
          ("Haskell" brittany)
          ("HTML" prettier)
          ("Java" clang-format)
          ("JavaScript" prettier)
          ("JSON" prettier)
          ("Jsonnet" jsonnetfmt)
          ("JSX" prettier)
          ("Kotlin" ktlint)
          ("LaTeX" latexindent)
          ("Less" prettier)
          ("Literate Haskell" brittany)
          ("Lua" lua-fmt)
          ("Markdown" prettier)
          ("Nix" nixpkgs-fmt)
          ("Objective-C" clang-format)
          ("OCaml" ocp-indent)
          ("Perl" perltidy)
          ("PHP" prettier)
          ("Protocol Buffer" clang-format)
          ("PureScript" purty)
          ("Python" black)
          ("R" styler)
          ("Reason" bsrefmt)
          ("ReScript" resfmt)
          ("Ruby" rufo)
          ("Rust" rustfmt)
          ("Scala" scalafmt)
          ("SCSS" prettier)
          ("Shell" shfmt)
          ("Solidity" prettier)
          ("SQL" sqlformat)
          ("Swift" swiftformat)
          ("Terraform" terraform-fmt)
          ("TOML" prettier)
          ("TSX" prettier)
          ("TypeScript" prettier)
          ("Verilog" istyle-verilog)
          ("Vue" prettier)
          ("XML" html-tidy)
          ("YAML" prettier)

          ("_Angular" prettier)
          ("_Flow" prettier)
          ("_Fortran 90" fprettify)
          ("_Gleam" gleam)
          ("_Ledger" ledger-mode)
          ("_Snakemake" snakefmt))))

(defun my-format ()
  "Formating files."
  (interactive)
  (whitespace-cleanup)
  ;; (cond
  ;;  ((member major-mode '(web-mode
  ;;                        css-mode
  ;;                        html-mode
  ;;                        js-mode
  ;;                        js2-mode
  ;;                        json-mode
  ;;                        sgml-mode
  ;;                        yaml-mode)) (prettier-prettify))
  ;;  (t (format-all-buffer)))
  (format-all-buffer))


(provide 'init-format)
;;; init-format.el ends here
