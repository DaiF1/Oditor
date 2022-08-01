# Oditor
A simple text editor written in OCaml

![demo](img/demo.gif)

## Text editing

Oditor is a text editor written with Vim keybindings.

### Normal mode 

You can move around using HJKL, enter insert mode by pressing I and command mode
by pressing ':'.

The text will automatically scroll up or down by moving the cursor out of the
screen.

### Insert mode

Insert mode allows the user to add text. It can be exited by pressing the esc key.

### Command mode

Command mode supports different operations:

- 'edit xxx' which takes as parameter the file you want to edit. 
    Only works for existing files.

- 'w' or 'w xxx' which saves the current file. It can be given a new file name 
    as parameter

- 'q' which quits Oditor.

- 'wq' which saves the file and quits Oditor.

## Syntax Highlighting

Oditor is an editor for OCaml, so it will only suppport this language.

Syntax Highlighting is still a WIP.

## Coming soon

- Autocompletion
- Tuareg mode
- Dune compilation tools 
