;;; init-ide.el --- IDE setting for code -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; Code Check
;; @https://www.flycheck.org/en/latest/
(leaf flycheck
  :hook (prog-mode-hook . flycheck-mode)
  :init
  (setq flycheck-stylelintrc "~/.stylelintrc.json"
        flycheck-tidyrc "~/.tidyrc"
        flycheck-emacs-lisp-load-path 'inherit
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode 'right-fringe
        ;; flycheck-global-modes
        ;; '(not text-mode outline-mode fundamental-mode org-mode
        ;;       diff-mode shell-mode eshell-mode term-mode vterm-mode)
        )
  :config
  ;; Prettify fringe style
  (when (fboundp 'define-fringe-bitmap)
    (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
      [16 48 112 240 112 48 16] nil nil 'center)))


;; Code Running
;; @https://github.com/emacsorphanage/quickrun#customize
(leaf quickrun
  :defun (imp-visit-buffer . impatient-mode)
  :commands quickrun quickrun-region
  :init
  (setq quickrun-focus-p nil
        quickrun-timeout-seconds 20))

(defun my-run (&optional start end)
  "Running for whole or parts."
  (interactive "r")
  (cond
   ((member major-mode '(html-mode web-mode)) (imp-visit-buffer))
   (t (if (evil-visual-state-p)
          (quickrun-region start end)
        (quickrun)))))

(defun my-repl ()
  "Runinig for interactive."
  (interactive)
  (cond
   ((eq major-mode 'emacs-lisp-mode) (ielm))
   ((eq major-mode 'python-mode) (run-python))
   ;; ((eq major-mode 'lua-mode) (run-lua))
   ((member major-mode '(js-mode js2-mode)) (nodejs-repl))
   (t (message "no repl for selected mode"))))

(defun quickrun-vterm ()
  "Quickrun command in vterm."
  (interactive)
  (let ((buffer buffer-file-name))
    (vterm-toggle-cd-show)
    (vterm-send-string (format "node %s" buffer))
    (vterm-send-return)))

;; Code Formating
(leaf editorconfig
  :hook (shell-mode-hook . editorconfig-mode))

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


(provide 'init-ide)
;;; init-ide.el ends here
