![Insert Pair Edit](doc/insert-pair-edit.gif)

----------------------------------------------------------------------
# insert-pair-edit

This project defines a [GNU Emacs](https://www.gnu.org/emacs/)
package, `insert-pair-edit`, which supplies a set of commands that are
targeted as being a more feature rich alternative to the standard
`M-(` **Emacs** keybinding, `insert-parentheses`. 

----------------------------------------------------------------------
## Overview

The `insert-pair-edit` package supplies commands to _insert_ and
_update_ "PAIRs" within an **Emacs** buffer.  The _PAIRs_ consist of
_OPEN_ and _CLOSE_ strings which delimit text in some fashion.

The primary function of the `insert-pair-edit` package is the
**Emacs** interactive command `insert-pair-edit`.  When executed, the
`insert-pair-edit` interactive command will prompt the user to enter
a `customize`-able _MNEMONIC_ identifying a _PAIR_ via the **Emacs**
`minibuffer`. 

Selection of a _MNEMONIC_ will cause a 'major-mode dependent' _PAIR_ to
be inserted around a 'lexical unit' at _POINT_.  The _PAIR_ consists
of _OPEN_ and _CLOSE_ strings which delimit text in some fashion.

This _PAIR_ can then be _moved_ about the buffer using the commands
defined within the `ipe-edit-mode` minor mode.

----------------------------------------------------------------------
## Editing PAIRs

After the `insert-pair-edit` command is run, the _OPEN_ and _CLOSE_
strings of the _PAIR_ are inserted into the current buffer as
overlays, then, `insert-pair-edit` runs the command `ipe-edit-mode` to
enter the **Insert Pair Edit (ipe)** minor mode. 

The `ipe-edit-mode` supplies further commands to interactively and
independently move the overlays representing the _OPEN_ and _CLOSE_
strings for the inserted _PAIR_ about the buffer, and to either
insert (`ipe-edit--insert-pair`), or discard (`ipe-edit--abort`)
them once they have been correctly positioned.

Movement of the _OPEN_ and _CLOSE_ overlays is based upon 'lexical
units'.  The 'lexical units' are either: characters, words, lines,
or lists (S-expressions).

By default, movement will be by 'words', but this can be changed
interactively via the: `ipe-edit--movement-by-*` commands or, by
`ipe-pairs` / `ipe-mode-pairs` `customize`-ations.

![ipe-edit-mode Commands](doc/ipe-edit-mode-menu.png)

Additional commands are supplied to operate on the _CONTENTS_ of the
_PAIR_ (i.e. the text between the _OPEN_ and _CLOSE_ overlays.)  Text
can be copied, deleted, replaced and case converted.

Certain characters between the _OPEN_ and _CLOSE_ overlays can also be
ESCAPE-d.  These characters will be replaced by overlays, which
will be updated by the `ipe-edit-mode` movement commands, and
inserted when the `ipe-edit--insert-pair` command is invoked.

Customizations for the mode can be found under the `ipe` group.

-------------------------------------------------------------------
## Pairs Menu

![Insert Pair Edit Menu](doc/insert-pair-edit-menu.png)

If running **Emacs** in a graphical environment which supports menus,
the `insert-pair-edit` package can add an extra `Pairs` menu item to
the standard **Emacs** `Edit` Menu.

The set of _PAIRs_ listed under the `Insert PAIR` / `Update
PAIR` / `Delete PAIR` sub-menus will change dynamically based upon
the current buffer's `major-mode`.

![Insert Pair Edit Menu](doc/insert-pair-edit-mode-menu.png)

Selecting a menu item under `Insert PAIR` will insert the selected
_PAIR_ into the current buffer around _POINT_, and enter
`ipe-edit-mode`.

Selecting a menu item under `Update PAIR` will update the _nearest_
_PAIR_ (of the given type) and enter `ipe-edit-mode`.

Selecting a menu item under `Delete PAIR` will delete the _nearest_
_PAIR_ (of the given type) and remain in the current **Emacs** mode.

----------------------------------------------------------------------
## Installation:

Download the `insert-pair-edit` project:

```
git clone https://github.com/BriansEmacs/insert-pair-edit.el <path-to-download-dir>/ipe
```

Add the following to your `.emacs` file:

```
(add-to-list 'load-path "<path-to-download-dir>/ipe"))
(add-to-list 'load-path "<path-to-download-dir>/ipe/modes"))
(require 'insert-pair-edit)
(global-set-key (kbd "M-(") 'insert-pair-edit)
```

----------------------------------------------------------------------
## Further Help

After installation: from the Emacs `Edit` menu:

```
Edit >
  Pairs >
    Info
    Help
```

From the keyboard:

```
M-x ipe-help
M-x ipe-help--info
```

----------------------------------------------------------------------
