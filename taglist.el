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
;;    ENTER to jump to the method. You also can type a string in the buffer and
;;    method list will be reduced to those which contain the string as a
;;    substring. Nice highlight is implemented.

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

;; 切换C/C++头文件
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
;; 存储当前的tag
(defvar taglist-current-tag nil
  "Current Semantic tag in source buffer.")

;; 这个buffer用于选择tags
(defvar taglist-buffer nil
  "Buffer used to selecting tags in Taglist.")

;; 列
(defvar taglist-names-column 2
  "Column used when selecting tags in Taglist.")

;; 当搜索的时候方法的集合
(defvar taglist-tags nil
  "Collection of methods used when searching for current selection.")

;; 实际的方法
(defvar taglist-actual-methods nil
  "Collection of actual methods used when searching for current selection.")

;; 当前的搜索字符串
(defvar taglist-search-string nil
  "The current search string during a search.")

;; overlays用于高亮搜索字符串.
(defvar taglist-overlays nil
  "List of active overlays.")

(defvar taglist-current-language nil
  "current programming language.")

(defvar taglist-current-line nil
  "current line")

(defvar taglist-language-hash-table nil
  "taglist language hash table")

(defvar taglist-current-major-mode nil
  "current major mode")

(defun taglist-detect-language-by-major-mode ()
  "detect language by modeline"
  (let ((language nil))
    (message "major mode = %s" taglist-current-major-mode)
    (cond
     ((string= "emacs-lisp-mode" taglist-current-major-mode)
      (setq language "lisp")
      )
     ((string= "c-mode" taglist-current-major-mode)
      (setq language "c")
      )
     ((string= "c++-mode" taglist-current-major-mode)
      (setq language "c++")
      )
     ((string= "python-mode" taglist-current-major-mode)
      (setq language "python")
      )
     ((string= "ruby-mode" taglist-current-major-mode)
      (setq language "ruby")
      )
     ((string= "java-mode" taglist-current-major-mode)
      (setq language "java")
      )
     ((string= "objc-mode" taglist-current-major-mode)
      (setq language "ObjectiveC")
      )
     ((string= "vimrc-mode" taglist-current-major-mode)
      (setq language "vim")
      )
     ;; ((string= "xxx-mode" taglist-current-major-mode)
     ;;  (setq language "xxx")
     ;;  )
     (t (setq language nil))
     )
    (message "detect language by major mode = %s" language)
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

    (message "last language = %s" language)
    language
    )
  )


(defun taglist-get-kinds-by-language (language)
  "get kinds by kinds"
  (let ((kinds ""))
    (dolist (elt (taglist-get-kinds-map-by-language language))
      (setq kinds (concat kinds (substring elt 0 1)))
      )
    kinds
    )
  )

(defun taglist-get-kinds-map-by-language (language)
  "get kinds by kinds"

  (message "language = %s" language)
  (let ((hash-value (gethash language taglist-language-hash-table))
        (kinds nil))
    ;; (cdr list)       2nd to last elements.
    ;; "C;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function"
    ;; (message "hash-value = %s" hash-value)
    (setq kinds (cdr (split-string hash-value ";" t)))
    (message "kinds = %S" kinds)
    kinds
    )
  )

(defun taglist-get-ctags-language (language)
  "get ctags-language by ctags-language"

  ;; (message "language = %s" language)
  (let ((hash-value (gethash language taglist-language-hash-table))
        (ctags-language nil))
    ;; (cdr list)       2nd to last elements.
    ;; "C;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function"
    (message "hash-value = %s" hash-value)
    (setq ctags-language (car (split-string hash-value ";" t)))
    (message "ctags-language = %S" ctags-language)
    ctags-language
    )
  )

;; 返回当前buffer的所有tags, 一行是一个字符串
(defun taglist-get-tag-lines ()
  "return ctags output buffer;"

  ;; (buffer-file-name) "c" "dgsutvf"

  (let* ((detected-language (taglist-detect-language))   ;; 如果识别文件类型识别, 则提示用户输入文件类型.(识别文件类型是难点)
         (ctags-language (taglist-get-ctags-language detected-language))
         (ctags-lang-kinds (taglist-get-kinds-by-language detected-language)) ;; 根据文件类型自动选择 tag 的类型(kinds)
         (file (buffer-file-name taglist-buffer))) ;; 文件名
    ;; (message "file = %s" file)

    (setq taglist-current-language detected-language)
    (if file
        (split-string
         (apply #'ctags-process-string
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
      (insert (concat "Warnning: " (buffer-name taglist-buffer) " doesn't exist on your disk, you should save it first!\n"))
      nil
      )
    )
  )

;; 返回3个字符串的一个列表: 类型, 父亲, tag的名字
;; tagline =  main      /home/galen_liu/.emacs.d/src/ip46.c     38;"    f       line:38
(defun taglist-convert-to-elements (tagline)
  "Return a list of three strings, representing type, parent and name of tag F."
  (let ((elements (split-string tagline "\t" t)))
    (list (car elements) (string-to-number (nth 2 elements)) (nth 3 elements))
    )
  )


;; 显示当前buffer的方法/函数列表
;;;###autoload
(defun taglist-list-tags ()
  "Show method/function list of current buffer in a newly created buffer.
This function is recommended to be bound to some convinient hotkey."
  (interactive)
  (setq taglist-buffer (current-buffer))
  (setq taglist-current-line (line-number-at-pos))
  (setq taglist-current-major-mode major-mode)

  ;; (setq taglist-current-tag (semantic-current-tag))
  (switch-to-buffer (get-buffer-create (concat (buffer-name (current-buffer)) " method list")) t)
  (taglist-mode))

;; 跳转到指定方法
(defun taglist-jump-to-method ()
  "Jump to a method/function, corresponding the current line in method buffer.
When called standing on a line of method/function list, it closes the list
buffer and sets the point to a method/function, corresponding the line."
  (interactive)

  (let ((tag-record (nth (1- (line-number-at-pos)) taglist-actual-methods)))

    (if (and tag-record (= 0 (taglist-line tag-record)))
        (setq tag-record (nth (line-number-at-pos) taglist-actual-methods) )
      )

    (if (and tag-record (taglist-line tag-record))
        (progn
          ;; 关闭当前buffer
          (kill-buffer (current-buffer))

          ;; 切换到源代码buffer
          (switch-to-buffer taglist-buffer t)

          ;; 跳转到指定位置
          (goto-line (taglist-line tag-record))

          (recenter)
          )
      (message "The line does not contain method description!")
      )
    )
  )

;; 如果string包含substring, 则返回true
(defun taglist-matches-all (string substrings)
  "Return non-nil if STRING contain each of SUBSTRINGS as a substring."
  (reduce (lambda (prev part) (and prev (string-match part string))) substrings :initial-value t))

(defstruct taglist
  (tag)      ;; 名字, e.g. main
  (line)     ;; tag的行号位置, e.g. 11
  (type)     ;; tag的类型 比如: f
  )


(defun taglist-gen-display-struct (types taglines)
  "Return a list of three strings, representing type, parent and name of tag F."

  ;; (message "types = %s, taglines = %s" types taglines)
  (let ((tags-struct nil))
    ;; 查找每一种类型
    (dolist (tag-type types)
      (let* ((elt (split-string tag-type ":" t))
             (type-abbr (car elt))
             (type-name (nth 1 elt))
             (type-occur nil)
             )

        ;; (message "%s %s" type-abbr type-name)

        (dolist (tagline taglines)
          ;; tagline (main 38 f)
          ;; 当找到该种类型的tag的时候
          (when (string= type-abbr (nth 2 tagline))
            ;; 如果是第一次找到, 输出全名称
            (if type-occur
                ;; (message "%s" tagline)
                (setq tags-struct (append tags-struct (list (make-taglist :tag (concat "  " (car tagline)) :line (nth 1 tagline) :type (nth 2 tagline)))))
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

;; 根据搜索字符串更新方法/函数列表
(defun taglist-search-string-updated ()
  "Update method/function list according to search string."

  (message taglist-search-string)

  ;; let s:tlist_def_c_settings = 'c;d:macro;g:enum;s:struct;u:union;t:typedef;v:variable;f:function'
  ;; (setq types (list "d:Macro" "g:Enum" "s:Struct" "u:Union" "t:Typedef" "v:Variable" "f:Function"))
  (setq types (taglist-get-kinds-map-by-language taglist-current-language))

  (let ((taglist-actual-methods_tmp
         ;; remove-if-not 把list中不满足条件的全部删除掉
         (remove-if-not
          (lambda (elt)
            (taglist-matches-all (car elt) (split-string taglist-search-string))
            )
          taglist-tags)))

    ;; ("DEBUG" 11 "d") ("main" 18 "f")
    (setq taglist-actual-methods (taglist-gen-display-struct types taglist-actual-methods_tmp))
    )

  ;; (message "taglist-actual-methods = %S" taglist-actual-methods)

  (erase-buffer)

  (dolist (i taglist-overlays)
    ;; Delete the overlay from its buffer.
    (delete-overlay i))

  (setq taglist-overlays nil)

  ;; (defstruct taglist
  ;; (tag)      ;; 名字, e.g. main
  ;; (line)     ;; tag的行号位置, e.g. 11
  ;; (type)     ;; tag的类型 比如: f
  ;; )

  ;; [cl-struct-taglist "main" 38 "f"]
  (dolist (tag-record taglist-actual-methods)
    (insert (concat (taglist-tag tag-record) "\n"))
    )

  (let ((offset 0)
        (pos 0)
        (line-pos nil)
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

          (setq tag-record (nth (1- (line-number-at-pos pos)) taglist-actual-methods))
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
    ;;     (goto-line (+ 1 (/ (count-lines (point-min) (point-max)) 2))))
    (if line-pos
        (goto-line line-pos)
      (goto-line (+ 1 (/ (count-lines (point-min) (point-max)) 2))))
    ) ;; let
  )


;; 当key按下的时候调用
(defun taglist-key-pressed (key)
  "Called when KEY is pressed."
  (setq taglist-search-string (concat taglist-search-string (char-to-string key)))
  (taglist-search-string-updated))

;; 当退格键按下的时候调用.
(defun taglist-backspace-pressed ()
  "Called when Backspace is pressed."
  (interactive)
  (setq taglist-search-string (taglist-string-without-last taglist-search-string 1))
  (taglist-search-string-updated))

;; 根据key返回一个函数.
(defun taglist-make-key-function (key)
  "Return a function for KEY."
  `(lambda () (interactive) (taglist-key-pressed ,key)))

;; 映射key到它的函数
(defun taglist-key-itself (map key)
  "Maps in the MAP KEY to its function."
  (define-key map (char-to-string key) (taglist-make-key-function key)))

;; 退出方法列表buffer
(defun taglist-escape ()
  "Kill method list buffer."
  (interactive)
  (kill-buffer (current-buffer))
  (switch-to-buffer taglist-buffer))

;; 快捷键映射
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

    (define-key map (kbd "<RET>") 'taglist-jump-to-method)
    (define-key map (kbd "<backspace>") 'taglist-backspace-pressed)
    (define-key map (kbd "<ESC>") 'taglist-escape)
    ;; (define-key map (kbd "M-v") 'scroll-up)
    ;; (define-key map (kbd "C-v") 'scroll-down)
    map)
  "Keymap for `taglist-mode'.")

(defcustom ctags-executable-directory nil
  "If non-nil the directory to search global executables."
  :type '(choice (const :tag "Unset" nil) directory)
  :risky t
  :group 'ctags)

(defun ctags-program-path (name)
  (if ctags-executable-directory
      ;; (expand-file-name NAME &optional DEFAULT-DIRECTORY)
      ;; Convert filename NAME to absolute, and canonicalize it.
      (expand-file-name name ctags-executable-directory)
    name))

;; 运行外部程序, 输出为字符串.
(defun ctags-process-string (program &rest args)
  (with-temp-buffer
    ;; (process-file PROGRAM &optional INFILE BUFFER DISPLAY &rest ARGS)
    ;; Process files synchronously in a separate process.
    (let ((exit (apply #'process-file
                       (ctags-program-path program) nil t nil args))
          (output (progn
                    (goto-char (point-max))
                    (skip-chars-backward " \t\n")
                    (buffer-substring (point-min) (point)))))
      (or (zerop exit)
          (error "`%s' non-zero exit: %s" program output))
      output)))

(defun taglist-highlight-current-tag ()
  "Highlight current tag line"
  ;; 高亮当前tag的行
  ;; taglist-tags =  (("INET_ADDRSTRLEN" 17 "d") ("INET6_ADDRSTRLEN" 18 "d") ("ZERO" 19 "d") ("BUF_SIZE" 20 "d") ("global_var" 22 "v") ("IP4_2_6" 24 "f") ("IP6_2_4" 31 "f") ("main" 38 "f"))
  ;; get current line number; and find range of line nubmer in taglist-tags
  ;; Set current line corresponding to the current function/method if any
  (let ((prev-line 0)
        (curr-line 0)
        (tag-line 0)
        (i 0))

    (while (and
            (= 0 tag-line)
            (< i (length taglist-tags)))

      (setq curr-line (nth 1 (elt taglist-tags i)))
      (setq i (1+ i))

      ;; (message "taglist-current-line = %d, prev-line = %d curr-line = %d, tag-line = %d"
      ;;          taglist-current-line
      ;;          prev-line
      ;;          curr-line
      ;;          tag-line)

      (if (= taglist-current-line curr-line)
          ;; 最好的情况.
          (setq tag-line curr-line)

        (if (< taglist-current-line curr-line)
            ;; 当前行小于当前tag的行号的情况, 则选择前一个tag的行号
            (if (= 0 prev-line)
                ;; 如果前一个行号是0, 则选择当前的tag行号
                (setq tag-line curr-line)
              (setq tag-line prev-line)
              )
          ;; 当前行大于当前tag行号的情况, 继续循环, 直到当前行比当前tag的行号小.
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
                 taglist-actual-methods)))
      (when line
        (goto-line (1+ line))))
    )

  (hl-line-mode)
  )


(defun taglist-language-hash-table-init()
  "language hash table"
  (let ((language-hash-table))

    ;; create a hash table
    (setq language-hash-table (make-hash-table :test 'equal))

    ;; (detected language) -> (ctags language):(ctags language kinds)
    ;; add entries

    ;; Ant language
    (puthash "Ant" "Ant;p:projects;t:targets" language-hash-table)

    ;; assembly language
    (puthash "asm" "Asm;d:define;l:label;m:macro;t:type" language-hash-table)

    ;; aspperl language
    (puthash "aspperl" "Asp;c:constants;v:variable;f:function;s:subroutine" language-hash-table)

    ;; aspvbs language
    (puthash "aspvbs" "Asp;c:constants;v:variable;f:function;s:subroutine" language-hash-table)

    ;; awk language
    (puthash "awk"  "Awk;f:function" language-hash-table)

    ;; basic language
    (puthash "basic" "Basic;c:constant;l:label;g:enum;v:variable;t:type;f:function" language-hash-table)

    ;; beta language
    (puthash "beta" "BETA;f:fragment;s:slot;v:pattern" language-hash-table)

    ;; c language
    (puthash "c" "C;d:macro;g:enum;s:struct;u:union;t:typedef;\
v:variable;f:function" language-hash-table)

    ;; c++ language
    (puthash "c++" "C++;n:namespace;v:variable;d:macro;t:typedef;\
c:class;g:enum;s:struct;u:union;f:function" language-hash-table)

    ;; c# language
    (puthash "cs" "C#;d:macro;t:typedef;n:namespace;c:class;E:event;\
g:enum;s:struct;i:interface;p:properties;m:method" language-hash-table)

    ;; cobol language
    (puthash "cobol" "Cobol;d:data;f:file;g:group;p:paragraph;\
P:program;s:section" language-hash-table)

    ;; D programming language
    (puthash "d" "C++;n:namespace;v:variable;t:typedef;c:class;\
g:enum;s:struct;u:union;f:function" language-hash-table)

    ;; Dosbatch
    (puthash "dosbatch" "DosBatch;l:labels;v:variables" language-hash-table)

    ;; eiffel language
    (puthash "eiffel" "Eiffel;c:class;f:feature" language-hash-table)

    ;; erlang language
    (puthash "erlang" "Erlang;d:macro;r:record;m:module;f:function" language-hash-table)

    ;; expect (same as tcl) language
    (puthash "expect" "Tcl;c:class;f:method;p:procedure" language-hash-table)

    ;; flex
    (puthash "flex" "Flex;v:global;c:classes;p:properties;m:methods;f:functions;x:mxtags" language-hash-table)

    ;; fortran language
    (puthash "Fortran" "fortran;p:program;b:block data;c:common;e:entry;\
i:interface;k:type;l:label;m:module;n:namelist;t:derived;v:variable;\
f:function;s:subroutine" language-hash-table)

    ;; GO language
    (puthash "go" "Go;f:function;p:package;t:struct" language-hash-table)

    ;; HTML language
    (puthash "html" "HTML;a:anchor;f:function" language-hash-table)

    ;; java language
    (puthash "java" "Java;p:package;c:class;i:interface;g:enum;f:field;m:method" language-hash-table)

    ;; javascript language
    (puthash "javascript" "JavaScript;c:class;m:method;v:global;f:function;p:properties" language-hash-table)

    ;; lisp language
    (puthash "lisp" "Lisp;f:function" language-hash-table)

    ;; lua language
    (puthash "lua" "Lua;f:function" language-hash-table)

    ;; makefiles
    (puthash "make" "Make;m:macro" language-hash-table)

    ;; Matlab
    (puthash "matlab" "MatLab;f:function" language-hash-table)

    ;; Ocamal
    (puthash "ocamal" "OCaml;M:module;v:global;t:type;c:class;\
f:function;m:method;C:constructor;e:exception" language-hash-table)

    ;; pascal language
    (puthash "pascal" "Pascal;f:function;p:procedure" language-hash-table)

    ;; perl language
    (puthash "perl" "Perl;c:constant;l:label;p:package;s:subroutine" language-hash-table)

    ;; php language
    (puthash "php" "PHP;c:class;i:interface;d:constant;v:variable;f:function" language-hash-table)

    ;; python language
    (puthash "python" "Python;c:class;m:member;f:function" language-hash-table)

    ;; cython language
    (puthash "pyrex" "Python;c:classe;m:memder;f:function" language-hash-table)

    ;; rexx language
    (puthash "rexx" "REXX;s:subroutine" language-hash-table)

    ;; ruby language
    (puthash "ruby" "Ruby;c:class;f:method;F:function;m:singleton method" language-hash-table)

    ;; scheme language
    (puthash "Scheme" "scheme;s:set;f:function" language-hash-table)

    ;; shell language
    (puthash "sh" "Sh;f:function" language-hash-table)

    ;; C shell language
    (puthash "csh" "Sh;f:function" language-hash-table)

    ;; Z shell language
    (puthash "zsh" "Sh;f:function" language-hash-table)

    ;; slang language
    (puthash "slang" "SLang;n:namespace;f:function" language-hash-table)

    ;; sml language
    (puthash "sml" "SML;e:exception;c:functor;s:signature;r:structure;\
t:type;v:value;c:functor;f:function" language-hash-table)

    ;; sql language
    (puthash "sql" "SQL;f:functions;P:packages;p:procedures;t:tables;T:triggers;\
v:variables;e:events;U:publications;R:services;D:domains;x:MLTableScripts;\
y:MLConnScripts;z:MLProperties;i:indexes;c:cursors;V:views;d:prototypes;\
l:local variables;F:record fields;L:block label;r:records;s:subtypes" language-hash-table)

    ;; tcl language
    (puthash "tcl" "Tcl;c:class;f:method;m:method;p:procedure" language-hash-table)

    ;; Tex
    (puthash "tex" "Tex;c:chapters;s:sections;u:subsections;b:subsubsections;\
p:parts;P:paragraphs;G:subparagraphs" language-hash-table)

    ;; vera language
    (puthash "vera" "Vera;c:class;d:macro;e:enumerator;f:function;g:enum;m:member;\
p:program;P:prototype;t:task;T:typedef;v:variable;x:externvar" language-hash-table)

    ;; verilog language
    (puthash "verilog" "Verilog;m:module;c:constant;P:parameter;e:event;\
r:register;t:task;w:write;p:port;v:variable;f:function" language-hash-table)

    ;; VHDL
    (puthash "vhdl" "VHDL;c:constant;t:type;T:subtype;r:record;e:entity;\
f:function;p:procedure;P:package" language-hash-table)

    ;; vim language
    (puthash "vim" "Vim;v:variable;a:autocmds;c:commands;m:map;f:function" language-hash-table)

    ;; yacc language
    (puthash "yacc" "YACC;l:label" language-hash-table)

    language-hash-table
    )

  )

(defun taglist-mode-init ()
  "Initialize method/function list mode."
  ;; 当前的搜索字符串.
  (make-local-variable 'taglist-search-string)   ;; current method search string
  ;; 所有方法的结构体列表.
  (make-local-variable 'taglist-tags)         ;; list of taglist-method structures
  ;; 包含搜索字符串的所有方法的结构体列表.
  (make-local-variable 'taglist-actual-methods)  ;; subset of taglist-tags that contain taglist-search string in the name string
  ;; 方法名开始的列.
  (make-local-variable 'taglist-names-column)    ;; this is the column where method name fields starts
  ;; overlays用于高亮搜索字符串.
  (make-local-variable 'taglist-overlays)        ;; overlays used to highligh search string matches in method names

  (make-local-variable 'taglist-current-language)
  ;; (make-local-variable 'taglist-current-line)

  (make-local-variable 'taglist-language-hash-table)

  (message "current-line = %d" taglist-current-line)
  (setq taglist-overlays nil)
  (setq taglist-search-string "")

  (setq taglist-language-hash-table (taglist-language-hash-table-init))

  (setq taglist-tags
        (let* ((tag-lines (taglist-get-tag-lines)))
          (if tag-lines
              ;; (message "tag-lines = %s" tag-lines)
              (mapcar 'taglist-convert-to-elements tag-lines)
            ;; (message "tag-lines is nil")
            )
          ))

  ;; (message "taglist-tags %S" taglist-tags)

  (if (not taglist-tags)
      (insert "No taglist, Press ESC to exist!!!\n")
    ;; 把结果更新到屏幕上.
    (taglist-search-string-updated)
    (taglist-highlight-current-tag)
    )
  )

;; 定义mode
(define-derived-mode taglist-mode nil "Taglist"
  "EmacsAssist method selection mode.
   \\{taglist-mode-map}
   Turning on Text mode runs the normal hook `taglist-mode-hook'."
  (taglist-mode-init))

(global-set-key (kbd "M-i") 'taglist-list-tags)
;; ================================== Tag navigator end ======================

(provide 'taglist)

;;; taglist.el ends here
;; http://ergoemacs.org/emacs/elisp_break_loop.html
