# Changelog

## 0.6.0 (2026-11-24)

- Split into one file per viewer: text, EPUB (nov.el), PDF (pdf-tools),
  pdf-tools internals, and pdf-canvas.  `multi-region-pdf-tools-test.el`
  checks what multi-region assumes about pdf-tools; run
  `make test-pdf-tools` after updating pdf-tools.
- A manual: `C-h R multi-region` or `C-c M h`.
- New option `multi-region-prefix-key` (`C-c M` by default).
- `C-c M /` also works after kill and replace: after a replace it
  selects the replacements, after a kill and `undo` the text put back.
- `C-c M /` keeps the last five deselected sets, not just one: press it
  again right away (just `/` with `repeat-mode`) for the set before, or
  `C-u C-c M /` to pick one from a list.  New option
  `multi-region-restore-history`.
- New option `multi-region-ignored-modes`: `global-multi-region-mode`
  now stays off in Dired, Magit, Ibuffer and terminal buffers, so C-drag
  keeps its own meaning there.
- Turning the mode off removes everything it set up: the EPUB advice
  (once no EPUB buffer uses it) and the pending PDF redraw.
- The help of `multi-region-kill` and `multi-region-replace` names the
  keys again instead of showing "[undo]".
- Makefile and GitHub Actions CI (Emacs 27.2, 29.4, 30.1, snapshot).

## 0.5.0 (2026-11-11)

- Works in pdf-canvas buffers.
- Words hyphenated with ¬ are joined again in copied text.

## 0.4.0 (2026-10-14)

- Pictures are selected together with text (eww, EPUB, Org, Markdown);
  M-drag selects a picture area in a PDF.

## 0.3.0 (2026-10-13)

- `C-c M /` brings back what was last deselected.
- C-g deselects everything in EPUB buffers too.
- C-drag on a link in eww or an EPUB selects.
- README.

## 0.2.1 (2026-10-09)

- Works with pdf-tools 1.3, which forgets earlier regions on C-drag.
- A plain click keeps PDF selections; C-g deselects on all pages.

## 0.2.0 (2026-10-08)

- First release.
