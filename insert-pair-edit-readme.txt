This package defines a command, `insert-pair-edit', which is targeted
as being a more feature rich alternative to the standard 'M-(' Emacs
keybinding, `insert-parentheses'.

`insert-pair-edit' will prompt the user to enter a customizable
MNEMONIC that allows selection of a 'major-mode dependent' PAIR to be
inserted around point.  The PAIR consists of OPEN and CLOSE strings
which are used to delimit text in some fashion.

First, the OPEN and CLOSE strings are inserted as overlays, then
`insert-pair-edit' runs the command `ipe-edit-mode' to enter the
'Insert Pair Edit' (ipe) minor mode.

The `ipe-edit-mode' supplies commands to interactively and
independently move the overlays representing the OPEN and CLOSE
strings for the inserted PAIR about the buffer, and to either insert
(`ipe-edit--insert-pair'), or discard (`ipe-edit--abort') them once
they have been correctly positioned.

Movement of the OPEN and CLOSE overlays is based upon 'movement
units'.  The 'lexical units' are either: characters, words, lines, or
lists (S-expressions).  (For the full list of movement / editing
commands, see the documentation for `ipe-edit-mode'.)

By default, movement will be by 'words', but this can be changed
interactively via the: `ipe-edit--movement-by-*' commands or, by
`ipe-pairs' / `ipe-mode-pairs' `customize'-ations.

Additional commands are supplied to operate on the CONTENTS of the
PAIR (i.e. the text between the OPEN and CLOSE overlays.)  Text can be
copied, deleted, replaced and case converted.

Certain characters between the OPEN and CLOSE overlays can also be
ESCAPE-d.  These characters will be replaced by overlays, which will
be updated by the `ipe-edit-mode' movement commands, and inserted when
the `ipe-edit--insert-pair' command is invoked.

Customizations for the mode can be found under the `ipe' group.
