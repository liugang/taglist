;;; taglist.el --- Taglist provides an overview of the structure of the programming language files.

;; Copyright (C) 2015 Galen Liu
;; Author: Galen Liu <galen.gang.liu at gmail.com>

;; This file is *NOT* part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA

;; Version: 1.0

;; Compatibility: Emacs 24

;;; Commentary:

;; The "Tag List" plugin is a source code browser plugin for Emacs and provides
;; an overview of the structure of the programming language files and allows you
;; to efficiently browse through source code files for different programming
;; languages.

;; Ctags support this 43 languages Ant Asm Asp Awk Basic BETA C C++ C# Cobol
;; DosBatch Eiffel Erlang Flex Fortran Go HTML Java JavaScript Lisp Lua Make
;; MatLab ObjectiveC OCaml Pascal Perl PHP Python REXX Ruby Scheme Sh SLang SML
;; SQL Tcl Tex Vera Verilog VHDL Vim YACC

;; Contains some useful functions features for developers similar to
;; those from VisualAssist.  Remember that convenient M-o, M-g and M-m?

;; 1) Tag navigation.
;;
;;    When taglist-list-tags called, a new buffer is shown, containing list of
;;    different type tags. You can select the tag moving to its line and press
;;    ENTER to jump to the tag. You also can type a string in the buffer and tag
;;    list will be reduced to those which contain the string as a substring.
;;    Nice highlight is implemented.

;; 2) Header <-> Body file switch.
;;
;;    You can easily switch between body (c, cpp, cc...) and its corresponding
;;    header file (h, hpp...) using taglist-switch-h-cpp. The counterpart file
;;    is first searched in opened buffers and if there is no match the file is
;;    searched in the same directory. You can adjust body to header
;;    correspondence customizing taglist-header-switches variable.

;; taglist uses ctags (http://ctags.sourceforge.net/)
;; https://ctags.io/

;; Usage:

;; 1) Install ctags(exuberant-ctags) package
;; 2) Add convenient keymaps for fast taglist calls in prog-mode
;;
;;    (defun my-c-mode-common-hook ()
;;      (define-key c-mode-base-map (kbd "M-o") 'taglist-switch-h-cpp)
;;      (define-key c-mode-base-map (kbd "M-m") 'taglist-list-tags))
;;    (add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
;;
;;    (defun my-python-mode-hook ()
;;      (define-key python-mode-map (kbd "M-m") 'taglist-list-tags))
;;    (add-hook 'python-mode-hook 'my-python-mode-hook)
;;
;;    (define-key lisp-mode-shared-map (kbd "M-m") 'taglist-list-tags)
;;
;; 3) Open any programming file, press M-m. Try to type any tag name.
;;
;; 4) Open any .cpp file.  Press M-o.  If there is .h or .hpp file in the
;;    same folder, it will be opened.

;;; Changelog:

;; 03 Nov 2015 -- v1.0 Initial version created.

;;; Code:

(require 'cl)
;; ================================== My STRING utils ========================
(defun taglist-string-without-last (string n)
  "This function truncates from the STRING last N characters."
  (substring string 0 (max 0(- (length string) n))))

(defun taglist-string-ends-with (string end)
  "Check whether STRING ends with END substring."
  (string= end (substring string (- (length end)))))
;; ================================== My STRING utils end ====================

;; ================================== CPP-H switch ===========================
;;;###autoload
(defvar taglist-header-switches '(("h" . ("cpp" "cc" "c"))
                                  ("hpp" . ("cpp" "cc"))
                                  ("cpp" . ("h" "hpp"))
                                  ("c" . ("h"))
                                  ("C" . ("H"))
                                  ("H" . ("C" "CPP" "CC"))
                                  ("cc" . ("h" "hpp")))
  "This variable defines possible switches for `taglist-switch-h-cpp' function.
Its format is list of (from . (to1 to2 to3...)) elements.  From and toN are
strings which are extentions of the files.")

;;;###autoload
(defun taglist-switch-h-cpp ()
  "Switch header and body file according to `taglist-header-switches' var.
The current buffer's file name extention is searched in
`taglist-header-switches' variable to find out extention for file's counterpart,
for example *.hpp <--> *.cpp."
  (interactive)
  (let* ((ext (file-name-extension (buffer-file-name)))
         (base-name (taglist-string-without-last (buffer-name) (length ext)))
         (base-path (taglist-string-without-last (buffer-file-name) (length ext)))
         (count-ext (cdr (find-if (lambda (i) (string= (car i) ext)) taglist-header-switches))))
    (cond
     (count-ext
      (unless
          (or
           (loop for b in (mapcar (lambda (i) (concat base-name i)) count-ext)
                 when (bufferp (get-buffer b)) return (switch-to-buffer b))
           (loop for c in (mapcar (lambda (count-ext) (concat base-path count-ext)) count-ext)
                 when (file-exists-p c) return (find-file c)))
        (message "There is no corresponding pair (header or body) file.")))
     (t
      (message "It is not a header or body file! See taglist-header-switches variable.")))))
;; ================================== CPP-H switch end =========================

;; ================================== Tag navigator =========================

(defvar taglist-source-code-buffer nil
  "Source code buffer")

(defvar taglist-all-tags nil
  "Collection of tags used when searching for current selection.")

(defvar taglist-actual-tags nil
  "Collection of actual tags used when searching for current selection.")

(defvar taglist-search-string nil
  "The current search string during a search.")

;; overlays, use hightlight tags
(defvar taglist-overlays nil
  "List of active overlays.")

(defvar taglist-current-language nil
  "current programming language.")

(defvar taglist-current-line nil
  "current line")

(defvar taglist-current-major-mode nil
  "current major mode")

(defface taglist-tag-type-face
  '((t :inherit font-lock-function-name-face :height 1.2))
  "Face for highlighting tag's type.")

(defvar taglist-tag-type-face 'taglist-tag-type-face)

(defconst taglist-font-lock-keywords-1
  (list
   '("^[^ ]*$" . taglist-tag-type-face)
   )
  "Minimal highlighting expressions for taglist mode")

(defvar taglist-font-lock-keywords taglist-font-lock-keywords-1
  "Default highlighting expressions for taglist mode")


;; major mode name map to language
;; (add-to-list 'taglist-major-to-language-alist '("<lang>-mode" "<lang>"))

(defvar taglist-major-to-language-alist
  `(
    ("emacs-lisp-mode" "lisp")
    ("lisp-mode" "lisp")
    ("c-mode" "c")
    ("c++-mode" "c++")
    ;; rust is support at https://github.com/universal-ctags
    ("rst-mode" "rst")
    ;; https://github.com/dominikh/go-mode.el.git
    ("go-mode" "go")
    ("java-mode" "java")
    ("python-mode" "python")
    ("ruby-mode" "ruby")
    ("javascript-mode" "javascript")
    ("objc-mode" "objc")
    ("asm-mode" "asm")
    ("sh-mode" "sh")
    ("shell-script-mode" "sh")
    ("sql-mode" "sql")
    ("tcl-mode" "tcl")
    ("fortran-mode" "fortran")
    ("f90-mode" "fortran")
    ("scheme-mode" "scheme")
    ("pascal-mode" "pascal")
    ("perl-mode" "perl")
    ("verilog-mode" "verilog")
    ("vhdl-mode" "vhdl")
    ("awk-mode" "awk")
    ("html-mode" "html")
    ("latex-mode" "tex")
    ("makefile-gmake-mode" "make")
    ;; https://github.com/mcandre/vimrc-mode.git
    ("vimrc-mode" "vim")
    ))

(defvar taglist-language-to-ctags-alist
  `(
    ;; Ant language
    ("ant" "Ant;p:projects;t:targets")

    ;; assembly language
    ("asm" "Asm;d:define;l:label;m:macro;t:type")

    ;; aspperl language
    ("aspperl" "Asp;c:constants;v:variable;f:function;s:subroutine")

    ;; aspvbs language
    ("aspvbs" "Asp;c:constants;v:variable;f:function;s:subroutine")

    ;; awk language
    ("awk"  "Awk;f:function")

    ;; basic language
    ("basic" "Basic;c:constant;l:label;g:enum;v:variable;t:type;f:function")

    ;; beta language
    ("beta" "BETA;f:fragment;s:slot;v:pattern")

    ;; c language
    ("c" "C;d:macro;g:enum;s:struct;u:union;t:typedef;\
v:variable;f:function")

    ;; c++ language
    ("c++" "C++;n:namespace;v:variable;d:macro;t:typedef;\
c:class;g:enum;s:struct;u:union;f:function")

    ;; c# language
    ("cs" "C#;d:macro;t:typedef;n:namespace;c:class;E:event;\
g:enum;s:struct;i:interface;p:properties;m:method")

    ;; cobol language
    ("cobol" "Cobol;d:data;f:file;g:group;p:paragraph;\
P:program;s:section")

    ;; D programming language
    ("d" "C++;n:namespace;v:variable;t:typedef;c:class;\
g:enum;s:struct;u:union;f:function")

    ;; Dosbatch
    ("dosbatch" "DosBatch;l:labels;v:variables")

    ;; eiffel language
    ("eiffel" "Eiffel;c:class;f:feature")

    ;; erlang language
    ("erlang" "Erlang;d:macro;r:record;m:module;f:function")

    ;; expect (same as tcl) language
    ("expect" "Tcl;c:class;f:method;p:procedure")

    ;; flex
    ("flex" "Flex;v:global;c:classes;p:properties;m:methods;f:functions;x:mxtags")

    ;; fortran language
    ("Fortran" "fortran;p:program;b:block data;c:common;e:entry;\
i:interface;k:type;l:label;m:module;n:namelist;t:derived;v:variable;\
f:function;s:subroutine")

    ;; GO language
    ("go" "Go;f:function;p:package;t:struct")

    ;; HTML language
    ("html" "HTML;a:anchor;f:function")

    ;; java language
    ("java" "Java;p:package;c:class;i:interface;g:enum;f:field;m:method")

    ;; javascript language
    ("javascript" "JavaScript;c:class;m:method;v:global;f:function;p:properties")

    ;; lisp language
    ("lisp" "Lisp;v:variable;f:function")

    ;; lua language
    ("lua" "Lua;f:function")

    ;; makefiles
    ("make" "Make;m:macro")

    ;; Matlab
    ("matlab" "MatLab;f:function")

    ;; Ocamal
    ("ocamal" "OCaml;M:module;v:global;t:type;c:class;\
f:function;m:method;C:constructor;e:exception")

    ;; pascal language
    ("pascal" "Pascal;f:function;p:procedure")

    ;; perl language
    ("perl" "Perl;c:constant;l:label;p:package;s:subroutine")

    ;; php language
    ("php" "PHP;c:class;i:interface;d:constant;v:variable;f:function")

    ;; python language
    ("python" "Python;c:class;m:member;f:function")

    ;; cython language
    ("pyrex" "Python;c:classe;m:memder;f:function")

    ;; rexx language
    ("rexx" "REXX;s:subroutine")

    ;; ruby language
    ("ruby" "Ruby;c:class;f:method;F:function;m:singleton method")

    ;; scheme language
    ("Scheme" "scheme;s:set;f:function")

    ;; shell language
    ("sh" "Sh;f:function")

    ;; C shell language
    ("csh" "Sh;f:function")

    ;; Z shell language
    ("zsh" "Sh;f:function")

    ;; slang language
    ("slang" "SLang;n:namespace;f:function")

    ;; sml language
    ("sml" "SML;e:exception;c:functor;s:signature;r:structure;\
t:type;v:value;c:functor;f:function")

    ;; sql language
    ("sql" "SQL;f:functions;P:packages;p:procedures;t:tables;T:triggers;\
v:variables;e:events;U:publications;R:services;D:domains;x:MLTableScripts;\
y:MLConnScripts;z:MLProperties;i:indexes;c:cursors;V:views;d:prototypes;\
l:local variables;F:record fields;L:block label;r:records;s:subtypes")

    ;; tcl language
    ("tcl" "Tcl;c:class;f:method;m:method;p:procedure")

    ;; Tex
    ("tex" "Tex;c:chapters;s:sections;u:subsections;b:subsubsections;\
p:parts;P:paragraphs;G:subparagraphs")

    ;; vera language
    ("vera" "Vera;c:class;d:macro;e:enumerator;f:function;g:enum;m:member;\
p:program;P:prototype;t:task;T:typedef;v:variable;x:externvar")

    ;; verilog language
    ("verilog" "Verilog;m:module;c:constant;P:parameter;e:event;\
r:register;t:task;w:write;p:port;v:variable;f:function")

    ;; VHDL
    ("vhdl" "VHDL;c:constant;t:type;T:subtype;r:record;e:entity;\
f:function;p:procedure;P:package")

    ;; vim language
    ("vim" "Vim;v:variable;a:autocmds;c:commands;m:map;f:function")

    ;; yacc language
    ("yacc" "YACC;l:label")

    ))

(defun taglist-goto-line (N)
  "goto specified line <N>"
  (goto-char (point-min))
  (forward-line (1- N))
  )

(defun taglist-detect-language-by-major-mode ()
  "detect language by modeline"
  (let ((language nil)
        (i 0))

    (while (and
            (not language)
            (< i (length taglist-major-to-language-alist)))

      (if (string= taglist-current-major-mode (car (elt taglist-major-to-language-alist i)))
          (setq language (nth 1 (elt taglist-major-to-language-alist i)))
        )
      (setq i (1+ i))
      )
    ;; (message "detect language by major mode = %s" language)
    language
    )
  )

;; TODO
(defun taglist-detect-language-by-modeline ()
  "detect language by modeline"
  (let ((language nil))
    (setq language nil)
    language
    )
  )

;; TODO
(defun taglist-detect-language-by-shebang ()
  "detect language by shebang"
  (let ((language nil))
    (setq language nil)
    language
    )
  )

(defun taglist-detect-language-by-filename ()
  "detect language by filename"
  (let ((language nil))
    (setq language "c")

    ;; Ant: ant.xml, build.xml

    ;; Asm: *.s *.S *.asm *.ASM *.nasm *.a51 *.A51
    ;; (who use this?)       *.29[kK] *.[68][68][kKsSxX] *.[xX][68][68]

    ;; Asp: *.asp *.aspx *.asa *.asax *.ascx *.ashx *.asmx *.axd


    ;; Awk: *.awk *.auk *.gawk *.mawk *.nawk

    ;; BlitzBasic: .bb .decls
    ;; PureBasic: .pb .pbi
    ;; Visual Basic: .vb .bas .cls .frm .frx .vba .vbhtml .vbs

    ;; BETA     *.bet

    ;; C: *.c *.cats *.idc .w
    ;; C++: *.c++ *.cc *.cp *.cpp *.cxx *.h *.h++ *.hh *.hp *.hpp *.hxx *.C *.H .inl .ipp .tcc .tpp

    ;; Emacs Lisp: .el
    ;;  filename: .emacs .emacs.desktop
    language
    )
  )

;; TODO
(defun taglist-detect-language-by-heuristics ()
  "detect language by heuristics"
  (let ((language nil))
    (setq language nil)
    language
    )
  )

(defun taglist-detect-language ()
  "return programming language of the file"

  (let ((language (taglist-detect-language-by-major-mode)))

    (if (not language)
        (setq language (taglist-detect-language-by-shebang))
      )

    (if (not language)
        (setq language (taglist-detect-language-by-shebang))
      )

    (if (not language)
        (setq language (taglist-detect-language-by-filename))
      )

    (if (not language)
        (setq language (taglist-detect-language-by-heuristics))
      )

    ;; (message "last language = %s" language)
    language
    )
  )


(defun taglist-get-kinds-by-language (language)
  "get kinds by kinds"
  (let ((kinds ""))
    (dolist (element (taglist-get-kinds-map-by-language language))
      (setq kinds (concat kinds (substring element 0 1)))
      )
    kinds
    )
  )

(defun taglist-get-ctags-language-config (language)
  "get ctags language config"
  (let ((language-config nil)
        (i 0))

    (while (and
            (not language-config)
            (< i (length taglist-language-to-ctags-alist)))

      (if (string= language (car (elt taglist-language-to-ctags-alist i)))
          (setq language-config (nth 1 (elt taglist-language-to-ctags-alist i)))
        )
      (setq i (1+ i))
      )
    ;; (message "detect language-config by language = %s" language-config)
    language-config
    )
  )

(defun taglist-get-kinds-map-by-language (language)
  "get ctags language kinds by language"

  ;; (message "language = %s" language)
  (let ((ctags-language-config (taglist-get-ctags-language-config language))
        (ctags-language-kinds nil))
    ;; (cdr list)       2nd to last elements.
    ;; "C;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function"
    ;; (message "ctags-language-config = %s" ctags-language-config)
    (setq ctags-language-kinds (cdr (split-string ctags-language-config ";" t)))
    ;; (message "ctags-language-kinds = %S" ctags-language-kinds)
    ctags-language-kinds
    )
  )

(defun taglist-get-ctags-language-name (language)
  "get ctags-language by ctags-language"

  ;; (message "language = %s" language)
  (let ((ctags-language-config (taglist-get-ctags-language-config language))
        (ctags-language nil))
    ;; (cdr list)       2nd to last elements.
    ;; "C;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function"
    ;; (message "ctags-language-config = %s" ctags-language-config)
    (setq ctags-language (car (split-string ctags-language-config ";" t)))
    ;; (message "ctags-language = %S" ctags-language)
    ctags-language
    )
  )

(defun taglist-get-tag-lines ()
  "return all tags;"

  (let* ((detected-language (taglist-detect-language))
         (ctags-language (taglist-get-ctags-language-name detected-language))
         (ctags-lang-kinds (taglist-get-kinds-by-language detected-language))
         (file (buffer-file-name taglist-source-code-buffer)))
    ;; (message "file = %s" file)

    (setq taglist-current-language detected-language)
    (if file
        (split-string
         (apply #'taglist-ctags-process-string
                "ctags"
                (append (list "-f" "-" ;; -f tagfile
                              "--format=2" ;; --format=level, Change the format of the
                              ;; output tag file.(1,2)
                              "--excmd=number" ;; --excmd=type, type are: number,
                              ;; pattern, mixed
                              "--fields=nks"   ;; --fields=[+|-]flags, n: Line number
                              ;; of tag definition; k: Kind of tag as
                              ;; a single letter [enabled]; s: Scope
                              ;; of tag definition [enabled]
                              "--sort=no" ;; --sort[=yes|no|foldcase] Indicates
                              ;; whether the tag file should be sorted on
                              ;; the tag name (default is yes)
                              (concat "--language-force=" ctags-language) ;; --language-force=language;
                              ;; forces the
                              ;; specified
                              ;; language
                              (concat "--" ctags-language "-kinds=" ctags-lang-kinds) ;; --<LANG>-kinds=[+|-]kinds;
                              ;; Specifies a
                              ;; list of
                              ;; language-specific
                              ;; kinds of tags
                              ;; (or kinds) to
                              ;; include in the
                              ;; output file
                              ;; for a
                              ;; particular
                              ;; language
                              file
                              )))
         "\n" t)
      (insert (concat "Warnning: " (buffer-name taglist-source-code-buffer) " doesn't exist on your disk, you should save it first!\n"))
      nil
      )
    )
  )

;; e.g. tagline =  main      /path/to/src/ip46.c     38;"    f       line:38
(defun taglist-convert-to-elements (tagline)
  "Return a list of three strings, representing type, parent and name of tag F."
  (let ((elements (split-string tagline "\t" t)))
    (list (car elements) (string-to-number (nth 2 elements)) (nth 3 elements))
    )
  )

;;;###autoload
(defun taglist-list-tags ()
  "Show tag list of current buffer in a newly created buffer.
This function is recommended to be bound to some convinient hotkey."
  (interactive)
  (setq taglist-source-code-buffer (current-buffer))
  (setq taglist-current-line (line-number-at-pos))
  (setq taglist-current-major-mode major-mode)

  (switch-to-buffer (get-buffer-create (concat (buffer-name (current-buffer)) " tag list")) t)
  (taglist-mode))

(defun taglist-jump-to-tag ()
  "Jump to a tag, corresponding the current line in tag buffer.
When called standing on a line of tag list, it closes the list
buffer and sets the point to a tag, corresponding the line."
  (interactive)

  (let ((tag-record (nth (1- (line-number-at-pos)) taglist-actual-tags)))

    (if (and tag-record (= 0 (taglist-line tag-record)))
        (setq tag-record (nth (line-number-at-pos) taglist-actual-tags) )
      )

    (if (and tag-record (taglist-line tag-record))
        (progn
          (kill-buffer (current-buffer))

          (switch-to-buffer taglist-source-code-buffer t)

          (taglist-goto-line (taglist-line tag-record))

          (recenter)
          )
      (message "The line does not contain tag description!")
      )
    )
  )

(defun taglist-matches-all (string substrings)
  "Return non-nil if STRING contain each of SUBSTRINGS as a substring."
  (reduce (lambda (prev part) (and prev (string-match part string))) substrings :initial-value t))

(defstruct taglist
  (tag)      ;; tag name, e.g. main
  (line)     ;; line number of tag, e.g. 11
  (type)     ;; type of the tag: e.g. f
  )


(defun taglist-gen-display-struct (types taglines)
  "Return a list of three strings, representing type, parent and name of tag F."

  ;; (message "types = %s, taglines = %s" types taglines)
  (let ((tags-struct nil))

    ;; loop for each tag type
    (dolist (tag-type types)
      (let* ((element (split-string tag-type ":" t))
             (type-abbr (car element))
             (type-name (nth 1 element))
             (type-occur nil)
             )

        ;; (message "%s %s" type-abbr type-name)

        ;; loop for all taglines
        (dolist (tagline taglines)
          ;; tagline (main 38 f)
          ;; when current tag type matched
          (when (string= type-abbr (nth 2 tagline))

            (if type-occur ;; when current tag is not first time appear.
                ;; (message "%s" tagline)
                (setq tags-struct (append tags-struct (list (make-taglist :tag (concat "  " (car tagline)) :line (nth 1 tagline) :type (nth 2 tagline)))))
              ;; if current tag is first time appear, then display type name and current tag.
              (setq tags-struct (append tags-struct (list (make-taglist :tag type-name :line 0 :type ""))))
              (setq tags-struct (append tags-struct (list (make-taglist :tag (concat "  " (car tagline)) :line (nth 1 tagline) :type (nth 2 tagline)))))
              ;; (message "%s" type-name)
              ;; (message "%s" tagline)
              (setq type-occur t)
              )
            )
          ) ;; 2nd dolist
        )
      ) ;; 1th dolist
    ;; (message "tags-struct = %s" tags-struct)
    ;; return tags-struct
    tags-struct
    )
  )

(defun taglist-search-string-updated ()
  "Update tag list according to search string."

  (message taglist-search-string)

  ;; let s:tlist_def_c_settings = 'c;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function'
  ;; (setq types (list "d:Macro" "g:Enum" "s:Struct" "u:Union" "t:Typedef" "v:Variable" "f:Function"))
  (setq types (taglist-get-kinds-map-by-language taglist-current-language))

  (let ((taglist-actual-tags_tmp
         ;; remove-if-not: Remove all items not satisfying search string.
         (remove-if-not
          (lambda (element)
            (taglist-matches-all (car element) (split-string taglist-search-string))
            )
          taglist-all-tags)))

    ;; ("DEBUG" 11 "d") ("main" 18 "f")
    (setq taglist-actual-tags (taglist-gen-display-struct types taglist-actual-tags_tmp))
    )

  ;; (message "taglist-actual-tags = %S" taglist-actual-tags)

  (erase-buffer)

  (dolist (i taglist-overlays)
    ;; Delete the overlay from its buffer.
    (delete-overlay i))

  (setq taglist-overlays nil)

  ;; [cl-struct-taglist "main" 38 "f"]
  (dolist (tag-record taglist-actual-tags)
    (insert (concat (taglist-tag tag-record) "\n"))
    )

  (let ((offset 0)
        (pos 0)
        (line-pos nil)
        (tag-record nil)
        (output (buffer-string))
        (search-strings (split-string taglist-search-string)))
    (dolist (search-string search-strings)
      ;; (message "output = %s" output)

      (while (string-match search-string output offset)
        (progn

          (setq pos (match-beginning 0))
          (setq offset (match-end 0))
          ;; (message "pos = %d, offset = %d" pos offset)
          ;; match: start form 0, overlay: start from 1;

          (setq tag-record (nth (1- (line-number-at-pos pos)) taglist-actual-tags))
          (if (not (= 0 (taglist-line tag-record)))
              (progn
                (push (make-overlay (+ 1 pos) (+ 1 offset)) taglist-overlays)
                (overlay-put (car taglist-overlays) 'face '(background-color . "yellow"))
                (if (not line-pos)
                    (setq line-pos (line-number-at-pos pos)))
                ))
          ))
      ) ;; dolist
    ;; (if (not line-pos)
    ;;     (taglist-goto-line (+ 1 (/ (count-lines (point-min) (point-max)) 2))))
    (if line-pos
        (taglist-goto-line line-pos)
      (taglist-goto-line (+ 1 (/ (count-lines (point-min) (point-max)) 2))))
    ) ;; let
  )

(defun taglist-key-pressed (key)
  "Called when KEY is pressed."
  (setq taglist-search-string (concat taglist-search-string (char-to-string key)))
  (taglist-search-string-updated))

(defun taglist-backspace-pressed ()
  "Called when Backspace is pressed."
  (interactive)
  (setq taglist-search-string (taglist-string-without-last taglist-search-string 1))
  (taglist-search-string-updated))

(defun taglist-make-key-function (key)
  "Return a function for KEY."
  `(lambda () (interactive) (taglist-key-pressed ,key)))

(defun taglist-key-itself (map key)
  "Maps in the MAP KEY to its function."
  (define-key map (char-to-string key) (taglist-make-key-function key)))

(defun taglist-escape ()
  "Kill tag list buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer taglist-source-code-buffer))

(defvar taglist-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (do ((k (string-to-char "a") (+ 1 k))) ((> k (string-to-char "z")))
      (define-key
        map
        (read-kbd-macro (char-to-string k))
        (taglist-make-key-function k)))
    (do ((k (string-to-char "A") (+ 1 k))) ((> k (string-to-char "Z")))
      (define-key
        map
        (read-kbd-macro (char-to-string k))
        (taglist-make-key-function k)))
    (do ((k (string-to-char "0") (+ 1 k))) ((> k (string-to-char "9")))
      (define-key
        map
        (read-kbd-macro (char-to-string k))
        (taglist-make-key-function k)))
    (dolist (k (string-to-list "=><&!"))
      (define-key
        map
        (read-kbd-macro (char-to-string k))
        (taglist-make-key-function k)))

    (taglist-key-itself map (string-to-char " "))
    (taglist-key-itself map (string-to-char "_"))

    (define-key map (kbd "<RET>") 'taglist-jump-to-tag)
    (define-key map (kbd "<backspace>") 'taglist-backspace-pressed)
    ;; Cause M-v doesn't work!!!
    ;; (define-key map (kbd "<ESC>") 'taglist-escape)
    (define-key map (kbd "C-q") 'taglist-escape)
    (define-key map (kbd "M-q") 'taglist-escape)
    (define-key map "\C-v" 'scroll-up)
    (define-key map "\M-v" 'scroll-down)
    map)
  "Keymap for `taglist-mode'.")

(defcustom ctags-executable-directory nil
  "If non-nil the directory to search global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'ctags)

(defun taglist-ctags-program-path (name)
  (if ctags-executable-directory
      ;; (expand-file-name NAME &optional DEFAULT-DIRECTORY)
      ;; Convert filename NAME to absolute, and canonicalize it.
      (expand-file-name name ctags-executable-directory)
    name))

(defun taglist-ctags-process-string (program &rest args)
  (with-temp-buffer
    ;; (process-file PROGRAM &optional INFILE BUFFER DISPLAY &rest ARGS)
    ;; Process files synchronously in a separate process.
    (let ((exit (apply #'process-file
                       (taglist-ctags-program-path program) nil t nil args))
          (output (progn
                    (goto-char (point-max))
                    (skip-chars-backward " \t\n")
                    (buffer-substring (point-min) (point)))))
      (or (zerop exit)
          (error "`%s' non-zero exit: %s" program output))
      output)))

(defun taglist-highlight-current-tag ()
  "Highlight current tag line"
  ;; taglist-all-tags =
  ;; (("INET_ADDRSTRLEN" 17 "d")
  ;;  ("INET6_ADDRSTRLEN" 18 "d")
  ;;  ("ZERO" 19 "d")
  ;;  ("BUF_SIZE" 20 "d")
  ;;  ("global_var" 22 "v")
  ;;  ("IP4_2_6" 24 "f")
  ;;  ("IP6_2_4" 31 "f")
  ;;  ("main" 38 "f"))
  ;; get current line number; and find range of line nubmer in taglist-all-tags
  ;; Set current line corresponding to the current function/tag if any
  (let ((prev-line 0)
        (curr-line 0)
        (tag-line 0)
        (i 0))

    (while (and
            (= 0 tag-line)
            (< i (length taglist-all-tags)))

      (setq curr-line (nth 1 (elt taglist-all-tags i)))
      (setq i (1+ i))

      ;; (message "taglist-current-line = %d, prev-line = %d curr-line = %d, tag-line = %d"
      ;;          taglist-current-line
      ;;          prev-line
      ;;          curr-line
      ;;          tag-line)

      (if (= taglist-current-line curr-line)
          ;; best case
          (setq tag-line curr-line)

        (if (< taglist-current-line curr-line)
            ;; if current line in source code buffer less than the line number
            ;; of current encounter tag in taglist, then choose the line number
            ;; of previous tags.
            (if (= 0 prev-line)
                ;; if line number of previous tag is zero, then choose the line
                ;; number of current tag
                (setq tag-line curr-line)
              (setq tag-line prev-line)
              )
          ;; if current line in source code buffer is big than the line number
          ;; of current encounter tag in taglist, then continue loop.
          )
        )
      ;; (message "tag-line = %d" tag-line)

      (setq prev-line curr-line)
      )

    (if (= 0 tag-line)
        (setq tag-line prev-line))

    ;; (message "found tag-line = %d" tag-line)

    (let ((line (position-if
                 (lambda (item) (= tag-line (taglist-line item)))
                 taglist-actual-tags)))
      (when line
        (taglist-goto-line (1+ line))))
    )

  (hl-line-mode)
  )

(defun taglist-mode-init ()
  "Initialize tag list mode."

  ;; current tag search string
  (make-local-variable 'taglist-search-string)
  ;; list of taglist-tag structures
  (make-local-variable 'taglist-all-tags)
  ;; subset of taglist-all-tags that contain taglist-search string in the name string
  (make-local-variable 'taglist-actual-tags)
  ;; overlays used to highligh search string matches in tag names
  (make-local-variable 'taglist-overlays)
  (make-local-variable 'taglist-current-language)
  (set (make-local-variable 'font-lock-defaults) '(taglist-font-lock-keywords))
  ;; (message "current-line = %d" taglist-current-line)
  (setq taglist-overlays nil)
  (setq taglist-search-string "")

  (setq taglist-all-tags
        (let* ((tag-lines (taglist-get-tag-lines)))
          (if tag-lines
              ;; (message "tag-lines = %s" tag-lines)
              (mapcar 'taglist-convert-to-elements tag-lines)
            ;; (message "tag-lines is nil")
            )
          ))

  ;; (message "taglist-all-tags %S" taglist-all-tags)

  (if (not taglist-all-tags)
      (insert "No taglist, Press <C-q> or <M-q> to exist!!!\n")
    (taglist-search-string-updated)
    (taglist-highlight-current-tag)
    )
  )

(define-derived-mode taglist-mode nil "Taglist"
  "EmacsAssist tag selection mode.
   \\{taglist-mode-map}
   Turning on Text mode runs the normal hook `taglist-mode-hook'."
  (taglist-mode-init))


;; ================================== Tag navigator end ======================

(provide 'taglist)

;;; taglist.el ends here

;; taglist.el:122:70:Warning: function `find-if' from cl package called at runtime
;; taglist.el:632:96:Warning: function `reduce' from cl package called at runtime
;; taglist.el:698:11:Warning: function `remove-if-not' from cl package called at runtime
;; taglist.el:902:18:Warning: function `position-if' from cl package called at runtime

;; [PATCH] emacs: Suppress warnings about using cl at runtime
;; https://notmuchmail.org/pipermail/notmuch/2012/010297.html

;; Local Variables:
;; byte-compile-warnings: (not cl-functions)
;; End:
