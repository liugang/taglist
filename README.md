# Emacs Taglist Mode

The Taglist plugin is a source code browser plugin for Emacs and provides an
overview of the structure of the programming language files and allows you to
efficiently browse through source code files for different programming
languages.

It contains some useful functions features for developers:

* Tag navigation.

   When taglist-list-tags called, a new buffer is shown, containing list of
   different type tags. You can select the tag moving to its line and press
   ENTER to jump to the method. You also can type a string in the buffer and
   method list will be reduced to those which contain the string as a
   substring. Nice highlight is implemented.

* Header <-> Body file switch.

   You can easily switch between body (c, cpp, cc...) and its corresponding
   header file (h, hpp...) using taglist-switch-h-cpp. The counterpart file
   is first searched in opened buffers and if there is no match the file is
   searched in the same directory. You can adjust body to header
   correspondence customizing taglist-header-switches variable.

Taglist uses ctags as backend (https://ctags.io/ or http://ctags.sourceforge.net/)

# Usage:

* Install ctags(exuberant-ctags) package
* Add convenient keymaps for fast taglist calls in prog-mode

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

* Open any programming file, press M-m. Try to type any tag name.

* Open any .cpp file.  Press M-o.  If there is .h or .hpp file in the
   same folder, it will be opened.

# Screenshot

![Image](images/taglist-c-src.png?raw=true)

![Image](images/taglist-tags.png?raw=true)
