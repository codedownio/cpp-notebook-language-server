# Changelog for cpp-notebook-language-server

## 0.2.3.0

* Support textDocument/inlayHint, textDocument/onTypeFormatting and textDocument/formatting
* Map code action edits back to the notebook instead of the shadow file
* Transform codeLens/resolve, so lens titles that the server defers resolve correctly

## 0.2.2.0

* Add debounced textDocument/didSave command and --did-save-period-ms argument

## 0.2.1.0

* Fix an issue with `untransformPosition`
