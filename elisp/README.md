# Installation
1. install dependencies

   ```bash
   sudo apt-get install git-el clang-format-3.8 cscope
   ```
2. copy this directory ("elisp") into your home directory
3. edit your .emacs file, adding:

   ```elisp
   (load "~/elisp/emacs.el")
   ```

# Key mappings

Most standard emacs keys are preserved.

### Function keys
|   |   |   |   |   |
|---|---|---|---|---|
|**F1**: load file   |  **F2**: save file  | **F3**: prev file (alpha)  | **F4** next file  | **F5**: rename buffer  |
|**F6** switch to buffer | **F7**: compile C, run Py | **F8**: next error | **F9**: debug  | **F10**: shell  |


*Example: For multiple shells: F10, rename with F5, then F10 again*

### Buffer switching
* Ctrl-f1 to Ctrl-f4: switch to buffers 1 thru 4
* Ctrl-f5: change the title of thie emacs window.  Hit enter to auto-title.
* F3, F4: switch to prev or next buffer alphabetically
* Ctrl-f7: switch to the compilation buffer
* Ctrl-f9: switch to the debug buffer
* Ctrl-f10: switch to the shell buffer
* F6: type in a buffer name to switch to

### Pane management
|   |   |
|---|---|
|**Alt-\\** | (Alt-backslash): split window vertically |
|**Alt--**| (Alt-minus): split window horizontally|
|**Alt-0**| (Alt-zero): remove split window|

### Content control/navigation
|   |   |
|---|---|
|**Alt-g**| goto line|
|**Ctrl-f**, or **Ctrl-s**| search|
|**Ctrl-r**| search backwards|
|**Ctrl-z**| undo|
|**Ctrl-v**| paste|
|**Ctrl-c**| copy|

end and home keys are to the line, not the file


### Formatting
|   |   |
|---|---|
|**Ctrl-tab**| clang format region|
|**Ctrl-i**| indent region|

### CSCOPE integration (Ctrl-q)

|   |   |
|---|---|
|**Ctrl-q q**| recalculate cscope tags|
|**Ctrl-q s**| cscope find symbol|
|**Ctrl-q d**| cscope find definition|
|**Ctrl-q f**| cscope find file|
|**Ctrl-q i**| cscope find include|
|**Ctrl-q c**| cscope find caller|
|**Ctrl-q t**| cscope find text|
|**Ctrl-q w**| switch to a new target window|

In cscope buffer:
* \<CR\> or mouse click -- goes to the referenced file/line
* \<SPACE\> views the referenced file/line
* alt-< previous query page
* alt-> next query page


### git Integration (Ctrl-e)
|   |   |
|---|---|
|**Ctrl-e s**| git status (most commands only work from this screen)|
|**Ctrl-e b**| toggle git blame (works in a file context)|
