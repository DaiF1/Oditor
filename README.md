# Oditor
A simple text editor written in OCaml

![demo](img/demo.gif)

## Who to run ?

Easy,
- clone the repo
- `cd Oditor`
- `dune exec ./oditor.exe`

## Text editing

Oditor is a text editor for OCaml. It provides syntax highlighting for `.ml`
files and uses Vim key bindings.

### Normal mode 

You can move around using `h`, `j`, `k`, `l` or using the `e`, `w` and `b` 
motions.

The text will automatically scroll up or down by moving the cursor out of the
screen.

You can enter Insert mode by pressing:

- `i` to insert on current character

- `a` to insert on next character

- `I` to insert at the start of the line

- `A` to insert at the end of the line

### Insert mode

Insert mode allows the user to add text. It can be exited by pressing the esc key.

### Command mode

Command mode supports different operations:

- `edit xxx` which takes as parameter the file you want to edit. 
    Only works for existing files.

- `w` or `w xxx` which saves the current file. It can be given a new file name 
    as parameter

- `q` which quits Oditor.

- `wq` which saves the file and quits Oditor.

You can use `Ctrl+W` to empty the command buffer.

## Coming soon

- Autocompletion
- Tuareg mode
- Dune compilation tools 
