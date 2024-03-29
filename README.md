# Oditor
A simple text editor written in OCaml

![demo](img/demo.gif)

## Install Oditor

To install Oditor, simply clone the repository and use the command
`opam install .` at its root.

## Text editing

Oditor is a text editor for OCaml. It provides syntax highlighting for `.ml`
files and uses Vim key bindings.

#### Using the editor

 You can move around using the arrow keys or `ctrl+arrow` to move faster across
 words.

 To quit the editor, press `ctrl+q`.

 To run a command, press `ctrl+r` and type your command. To see the availables
 commands, check out the **Commands** section.

#### File Manipulation

To create a new file, you simply need to press `ctrl+n`.
You can save this file using `ctrl+s`. If the file was already saved, you can
enter an empty string and it will be saved at the previous location.
You can also open a new file using `ctrl+o`

## Commands

Here are the currently supported commands:

- "setkmap", change the current keymap.

## Vim Controls

The editor comes with builtin vim controls. Those can be loaded by using the
command `setkmap vim`

#### Normal mode 

You can move around using `h`, `j`, `k`, `l` or using the `e`, `w` and `b` 
motions.

The text will automatically scroll up or down by moving the cursor out of the
screen.

You can enter Insert mode by pressing:

- `i` to insert on current character

- `a` to insert on next character

- `I` to insert at the start of the line

- `A` to insert at the end of the line

#### Insert mode

Insert mode allows the user to add text. It can be exited by pressing the esc key.

#### Command mode

Here are the supported commands with the vim keymaps:

- `edit xxx` which takes as parameter the file you want to edit. 
    Only works for existing files.

- `w` or `w xxx` which saves the current file. It can be given a new file name 
    as parameter

- `q` which quits Oditor.

- `wq` which saves the file and quits Oditor.

You can use `Ctrl+W` to empty the command buffer.

## Configuration

You can configure Oditor using a file named `config.yml`. Its default location
is in `~/.config/oditor`.

Here is a sample config :

```yml
keymaps: default # Can be default or vim. When nothing is specified, Oditor will use default

colors: # Colors for syntax highlighting. Ansi VGA colors used if nothing is specified
    default: "#ffffff"
    digit: "#ffff00"
    string: "#00ff00"
    char: "#00ffff"
    comment: "#999999"
    keyword: "#ff0000"
    operator: "ff00ff"
```

## Coming soon

- Autocompletion
- Tuareg mode
- Dune compilation tools 
