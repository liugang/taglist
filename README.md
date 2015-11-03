# taglist
The taglist plugin is a source code browser plugin for Emacs and provides an overview of the structure of the programming language files

The "Tag List" plugin is a source code browser plugin for Emacs and provides
an overview of the structure of the programming language files and allows you
to efficiently browse through source code files for different programming
languages.

Ctags support this 43 languages Ant Asm Asp Awk Basic BETA C C++ C# Cobol
DosBatch Eiffel Erlang Flex Fortran Go HTML Java JavaScript Lisp Lua Make
MatLab ObjectiveC OCaml Pascal Perl PHP Python REXX Ruby Scheme Sh SLang SML
SQL Tcl Tex Vera Verilog VHDL Vim YACC

Contains some useful functions features for developers similar to
those from VisualAssist.  Remember that convenient M-o, M-g and M-m?

## Tag navigation.

   When taglist-list-tags called, a new buffer is shown, containing list of
   different type tags. You can select the tag moving to its line and press
   ENTER to jump to the method. You also can type a string in the buffer and
   method list will be reduced to those which contain the string as a
   substring. Nice highlight is implemented.

## Header <-> Body file switch.

   You can easily switch between body (c, cpp, cc...) and its corresponding
   header file (h, hpp...) using taglist-switch-h-cpp. The counterpart file
   is first searched in opened buffers and if there is no match the file is
   searched in the same directory. You can adjust body to header
   correspondence customizing taglist-header-switches variable.

taglist uses ctags (http://ctags.sourceforge.net/)

Suggestion: https://ctags.io/

# Usage:

## Install ctags(exuberant-ctags) package
## Add convenient keymaps for fast taglist calls in prog-mode

```elisp
   (defun my-c-mode-common-hook ()
     (define-key c-mode-base-map (kbd "M-o") 'taglist-switch-h-cpp)
     (define-key c-mode-base-map (kbd "M-m") 'taglist-list-tags))
   (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

   (defun my-python-mode-hook ()
     (define-key python-mode-map (kbd "M-m") 'taglist-list-tags))
   (add-hook 'python-mode-hook 'my-python-mode-hook)

   (define-key lisp-mode-shared-map (kbd "M-m") 'taglist-list-tags)
```

## Open any programming file, press M-m. Try to type any tag name.

## Open any .cpp file.  Press M-o.  If there is .h or .hpp file in the
   same folder, it will be opened.
