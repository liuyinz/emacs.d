#!/usr/bin/env bash

readonly EMACE_USER_DIR="$HOME/.config/emacs"
readonly TREESIT_TAR="tree-sitter-grammars.aarch64-apple-darwin.*.tar.gz"
TREESIT_TEMP_DIR="$EMACE_USER_DIR/.cache/treesit-temp"

download_tar() {
  cd "$EMACE_USER_DIR/.cache" || exit
  if ! command -v gh; then
    echo "can not find gh"
    exit 1
  else
    gh release download --repo emacs-tree-sitter/tree-sitter-langs \
      --pattern "$TREESIT_TAR" --skip-existing
  fi
}

uncompress_dylib() {
  [[ -d "$TREESIT_TEMP_DIR" ]] && rm -rf "$TREESIT_TEMP_DIR"
  mkdir "$TREESIT_TEMP_DIR" && find . -name "$TREESIT_TAR" -mtime -1 -exec tar xf {} \
    --directory "$TREESIT_TEMP_DIR" \
    --wildcards "*.dylib" \;
}

rename_dylib() {
  cd "$TREESIT_TEMP_DIR" && find -- * -maxdepth 0 -exec mv {} libtree-sitter-{} \;
}

replace_origin() {
  cd "$EMACE_USER_DIR" || exit
  [[ -d "tree-sitter" ]] \
    && mv tree-sitter tree-sitter.old \
    && mv "$TREESIT_TEMP_DIR" tree-sitter
}

download_tar && uncompress_dylib && rename_dylib && replace_origin
