# Changelog for cpp-notebook-language-server

## 0.2.4.0

* Transform textDocument/selectionRange, textDocument/semanticTokens and the goto family, which reached the client in shadow coordinates
* Transform textDocument/rename, prepareRename, references, signatureHelp and declaration, which reached the server naming the notebook

## 0.2.3.2

* Untransform workspace/applyEdit, which named the shadow document and so was refused by the client

## 0.2.3.1

* Fix textDocument/formatting, which silently did nothing in 0.2.3.0. Formatting edits are now
  passed through untouched, so plain files format and notebooks are left alone.

## 0.2.3.0

* Support textDocument/inlayHint, textDocument/onTypeFormatting and textDocument/formatting
* Map code action edits back to the notebook instead of the shadow file
* Transform codeLens/resolve, so lens titles that the server defers resolve correctly

## 0.2.2.0

* Add debounced textDocument/didSave command and --did-save-period-ms argument

## 0.2.1.0

* Fix an issue with `untransformPosition`
