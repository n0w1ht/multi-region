# multi-region

Select several separate pieces of text at once in Emacs, then copy, kill,
replace or run a command on all of them together.

In *"I love ClaudeAI very much"* you can have **love** and **much** selected,
and nothing else. It works the same way in a PDF, where you can select
passages on several pages and copy them in one go.

It works in plain text, Org, Markdown and code buffers, in read-only buffers
such as eww, in EPUB books opened with [nov.el](https://depp.brause.cc/nov.el/),
and in PDFs opened with [pdf-tools](https://github.com/vedang/pdf-tools).

Pictures can be selected along with the text: drag across a picture in eww,
an EPUB, Org or Markdown, or mark a figure in a PDF with `M-drag`. Other
packages, such as [readq](https://github.com/n0w1ht/readq), get the text and
the pictures together, so one extract can hold both.

## Installation

multi-region is a single file, `multi-region.el`, and needs Emacs 27.1 or
later. Neither nov.el nor pdf-tools is required; multi-region uses them only
when they are there.

**With `package-vc-install`** (Emacs 29 or later):

```elisp
(package-vc-install "https://github.com/n0w1ht/multi-region")
```

**With `use-package` and a local copy:**

```elisp
(use-package multi-region
  :load-path "~/.emacs.d/lisp/multi-region"
  :config (global-multi-region-mode 1))
```

**By hand:** put `multi-region.el` in a folder on your `load-path`, then add
this to your init file:

```elisp
(require 'multi-region)
(global-multi-region-mode 1)   ; or M-x multi-region-mode in one buffer
```

The mode line shows `MR` while the mode is on, and `MR[3]` when three
pieces are selected.

## Selecting with the mouse

| Mouse            | What it does                                   |
|------------------|------------------------------------------------|
| `C-drag`         | Add the dragged text to the selections         |
| `C-double-click` | Add the word under the pointer                 |
| `C-triple-click` | Add the line under the pointer                 |
| `C-click`        | Deselect the selection under the pointer, or select the picture under it |
| `M-drag`         | In a PDF: select a picture area (a figure)     |
| `M-click`        | In a PDF: deselect the picture area under it   |

A region made the normal way (a plain drag, or `C-SPC` and motion) becomes a
selection as soon as you start a `C-drag`, so "drag, then C-drag, then
C-drag" works as it does in a word processor.

## Keyboard commands

All commands are under the `C-c m` prefix.

| Key           | Command                       | What it does                                                         |
|---------------|-------------------------------|----------------------------------------------------------------------|
| `C-c m SPC`   | `multi-region-toggle`         | Select the active region; without one, select the picture or word at point, or deselect the selection point is in |
| `C-c m w`     | `multi-region-copy`           | Copy all selections as one string (`M-w` does this too)             |
| `C-c m k`     | `multi-region-kill`           | Kill all selections (`C-w` does this too)                           |
| `C-c m r`     | `multi-region-replace`        | Replace every selection with a string                               |
| `C-c m x`     | `multi-region-do`             | Run a region command, such as `upcase-region`, on every selection   |
| `C-c m n`     | `multi-region-next`           | Jump to the next selection                                          |
| `C-c m p`     | `multi-region-previous`       | Jump to the previous selection                                      |
| `C-c m u`     | `multi-region-remove-last`    | Deselect the most recent selection                                  |
| `C-c m c`     | `multi-region-clear`          | Deselect everything                                                 |
| `C-c m /`     | `multi-region-restore`        | Bring back what was last deselected all at once                     |

`M-w` and `C-w` behave as usual while nothing is selected. Copied selections
are joined with a newline; give `C-c m w` a prefix argument (`C-u C-c m w`)
to type a different separator for one copy.

Copying deselects everything, unless `multi-region-keep-after-copy` is on.
`C-c m /` brings back whatever the last full deselect removed, whether it
came from `C-c m c`, a copy, or `C-g` in a PDF or EPUB. Selections you made
since stay selected.

## Pictures

A picture shown in the buffer is selected whole or not at all. Dragging
across it with the text around it selects it with the text, and `C-click`
on a picture selects just the picture. A selected picture gets a band in the
selection color around it (`multi-region-image-margin` sets its width).

This works for pictures in eww and EPUBs, for Org inline images
(`org-display-inline-images`, `C-c C-x C-v`) and Markdown inline images
(`markdown-display-inline-images`, `C-c C-x C-i`). In Org and Markdown a
picture link to a file that exists counts even when the picture is not
shown.

Copying (`M-w`) copies the text only; pictures are for packages that build
notes from the selections (see [For other packages](#for-other-packages)).

## In each kind of buffer

**Text, Org, Markdown and code.** Everything above works. Undo after
`C-w` or `C-c m r` puts the text back.

**eww and other read-only buffers.** Selecting, copying, jumping and
deselecting work. Kill copies the selections and then stops with "Buffer is
read-only", as Emacs does for an ordinary region. Replace and run-command
stop with the same message. On a link,
`C-drag` selects; a `C-click` that selects nothing still opens the link in a
new window, as eww does without this mode.

**EPUB (nov.el).** Selections are remembered for each chapter and come back
when you return to it. Copying collects them from all chapters, in reading
order. `C-g` deselects everything in every chapter.

**PDF (pdf-tools).** A PDF page is an image, so there is no point or mark
and selecting is done with the mouse:

- A plain drag starts a new selection on the page.
- `C-drag` adds another one.
- A click without a drag, with or without Control, keeps the selections.
- Selections are remembered for each page, and copying collects them from
  all pages.
- `C-g` deselects everything on every page.
- `M-drag` selects a picture area: a rectangle, such as a figure, kept as a
  picture rather than as text. It is drawn the way pdf-tools draws
  rectangles, and stays selected next to the text. `M-click` inside one
  deselects it.

Copy (`M-w`, `C-c m w`), deselect last (`C-c m u`), deselect all (`C-c m c`)
and bring back (`C-c m /`) work in PDFs; copy takes the text only, not
picture areas. Kill, replace, run-command and
next/previous don't apply there. `C-w` copies instead of killing.

Highlights are drawn with `pdf-view-selection-style`, so they cover exactly
the text that will be copied.

pdf-tools 1.3 forgets the regions already on a page when `C-drag` adds a new
one. multi-region works around that, so `C-drag` keeps them. Older
pdf-tools versions work too.

## Options

Set these with `M-x customize-group RET multi-region` or `setq`:

| Option                          | Default | Meaning                                                        |
|---------------------------------|---------|----------------------------------------------------------------|
| `multi-region-separator`        | `"\n"`  | String put between selections when they are copied or killed   |
| `multi-region-thing`            | `word`  | What `C-c m SPC` selects without a region: `word`, `symbol`, `sentence`, `line`, or any `thing-at-point` thing |
| `multi-region-keep-after-copy`  | `nil`   | Keep the selections after copying them                         |
| `multi-region-face`             | `region`| Face of the selections                                         |
| `multi-region-image-margin`     | `4`     | Width in pixels of the band around selected pictures; 0 for none |

## Other keys

```elisp
;; Use C-c s instead of C-c m as the prefix.
(define-key multi-region-mode-map (kbd "C-c m") nil)
(define-key multi-region-mode-map (kbd "C-c s") multi-region-command-map)

;; Select with M-S-drag instead of C-drag.
(define-key multi-region-mode-map [C-down-mouse-1] nil)
(define-key multi-region-mode-map [M-S-down-mouse-1] #'multi-region-mouse-drag)
```

## For other packages

`(multi-region-selections &optional absorb)` returns the selections of the
current buffer in reading order, without changing them. With `absorb`, an
active region becomes a selection first.

- In a text buffer each selection is `(:beg BEG :end END :text TEXT)`.
- In an EPUB it also has `:chapter`, the chapter index.
- A text selection that holds pictures also has `:images`, a list in buffer
  order. Each picture is `(:beg BEG :end END :file FILE)`, or
  `(:beg BEG :end END :data DATA :type TYPE)` for one that is not in a file
  (eww keeps downloaded pictures in memory). Pictures in an EPUB are files
  in nov.el's temporary folder, which goes away with the buffer, so copy
  them rather than link to them.
- In a PDF it is `(:page PAGE :edges (LEFT TOP RIGHT BOTTOM) :text TEXT)`,
  with edges as fractions of the page size. A picture area also has
  `:figure t`, and its text is empty. On each page, text comes in the order
  it was selected and each picture area before the first text below its
  top.

`(multi-region-clear)` deselects everything.
[readq](https://github.com/n0w1ht/readq) uses these to turn several
passages into one extract.

## Running the tests

```sh
emacs -Q --batch -L . -l multi-region-test.el -f ert-run-tests-batch-and-exit
```

The tests imitate pdf-tools and nov.el, so neither needs to be installed.

## License

GPL-3.0-or-later. See the header of `multi-region.el`.
