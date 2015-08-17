;; -*-no-byte-compile: t; -*-
;;; tabbar.el --- Display a tab bar in the header line
;;; Copy & Modified.

;; Copyright (C) 2003, 2004, 2005 David Ponce

;; Author: David Ponce <david@dponce.com>
;; Maintainer: David Ponce <david@dponce.com>
;; Created: 25 February 2003
;; Keywords: convenience
;; Revision: $Id: tabbar.el,v 1.7 2009/03/02 21:02:34 davidswelt Exp $

(defconst tabbar-version "2.0")

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; This library provides the Tabbar global minor mode to display a tab
;; bar in the header line of Emacs 21 and later versions.  You can use
;; the mouse to click on a tab and select it.  Also, three buttons are
;; displayed on the left side of the tab bar in this order: the
;; "home", "scroll left", and "scroll right" buttons.  The "home"
;; button is a general purpose button used to change something on the
;; tab bar.  The scroll left and scroll right buttons are used to
;; scroll tabs horizontally.  Tabs can be divided up into groups to
;; maintain several sets of tabs at the same time (see also the
;; chapter "Core" below for more details on tab grouping).  Only one
;; group is displayed on the tab bar, and the "home" button, for
;; example, can be used to navigate through the different groups, to
;; show different tab bars.
;;
;; In a graphic environment, using the mouse is probably the preferred
;; way to work with the tab bar.  However, you can also use the tab
;; bar when Emacs is running on a terminal, so it is possible to use
;; commands to press special buttons, or to navigate cyclically
;; through tabs.
;;
;; These commands, and default keyboard shortcuts, are provided:
;;
;; `tabbar-mode'
;;     Toggle the Tabbar global minor mode.  When enabled a tab bar is
;;     displayed in the header line.
;;
;; `tabbar-local-mode'         (C-c <C-f10>)
;;     Toggle the Tabbar-Local minor mode.  Provided the global minor
;;     mode is turned on, the tab bar becomes local in the current
;;     buffer when the local minor mode is enabled.  This permits to
;;     see the tab bar in a buffer where the header line is already
;;     used by another mode (like `Info-mode' for example).
;;
;; `tabbar-mwheel-mode'
;;     Toggle the Tabbar-Mwheel global minor mode.  When enabled you
;;     can use the mouse wheel to navigate through tabs of groups.
;;
;; `tabbar-press-home'         (C-c <C-home>)
;; `tabbar-press-scroll-left'  (C-c <C-prior>)
;; `tabbar-press-scroll-right' (C-c <C-next>)
;;     Simulate a mouse-1 click on respectively the "home", "scroll
;;     left", and "scroll right" buttons.  A numeric prefix argument
;;     value of 2, or 3, respectively simulates a mouse-2, or mouse-3
;;     click.
;;
;; `tabbar-backward'           (C-c <C-left>)
;; `tabbar-forward'            (C-c <C-right>)
;;     Are the basic commands to navigate cyclically through tabs or
;;     groups of tabs.  The cycle is controlled by the
;;     `tabbar-cycle-scope' option.  The default is to navigate
;;     through all tabs across all existing groups of tabs.  You can
;;     change the default behavior to navigate only through the tabs
;;     visible on the tab bar, or through groups of tabs only.  Or use
;;     the more specialized commands below.
;;
;; `tabbar-backward-tab'
;; `tabbar-forward-tab'
;;     Navigate through the tabs visible on the tab bar.
;;
;; `tabbar-backward-group'     (C-c <C-up>)
;; `tabbar-forward-group'      (C-c <C-down>)
;;     Navigate through existing groups of tabs.
;;
;;
;; Core
;; ----
;;
;; The content of the tab bar is represented by an internal data
;; structure: a tab set.  A tab set is a collection (group) of tabs,
;; identified by an unique name.  In a tab set, at any time, one and
;; only one tab is designated as selected within the tab set.
;;
;; A tab is a simple data structure giving the value of the tab, and a
;; reference to its tab set container.  A tab value can be any Lisp
;; object.  Each tab object is guaranteed to be unique.
;;
;; A tab set is displayed on the tab bar through a "view" defined by
;; the index of the leftmost tab shown.  Thus, it is possible to
;; scroll the tab bar horizontally by changing the start index of the
;; tab set view.
;;
;; The visual representation of a tab bar is a list of valid
;; `header-line-format' template elements, one for each special
;; button, and for each tab found into a tab set "view".  When the
;; visual representation of a tab is required, the function specified
;; in the variable `tabbar-tab-label-function' is called to obtain it.
;; The visual representation of a special button is obtained by
;; calling the function specified in `tabbar-button-label-function',
;; which is passed a button name among `home', `scroll-left', or
;; `scroll-right'.  There are also options and faces to customize the
;; appearance of buttons and tabs (see the code for more details).
;;
;; When the mouse is over a tab, the function specified in
;; `tabbar-help-on-tab-function' is called, which is passed the tab
;; and should return a help string to display.  When a tab is
;; selected, the function specified in `tabbar-select-tab-function' is
;; called, which is passed the tab and the event received.
;;
;; Similarly, to control the behavior of the special buttons, the
;; following variables are available, for respectively the `home',
;; `scroll-left' and `scroll-right' value of `<button>':
;;
;; `tabbar-<button>-function'
;;    Function called when <button> is selected.  The function is
;;    passed the mouse event received.
;;
;; `tabbar-<button>-help-function'
;;    Function called with no arguments to obtain a help string
;;    displayed when the mouse is over <button>.
;;
;; To increase performance, each tab set automatically maintains its
;; visual representation in a cache.  As far as possible, the cache is
;; used to display the tab set, and refreshed only when necessary.
;;
;; Several tab sets can be maintained at the same time.  Only one is
;; displayed on the tab bar, it is obtained by calling the function
;; specified in the variable `tabbar-current-tabset-function'.
;;
;; A special tab set is maintained, that contains the list of the
;; currently selected tabs in the existing tab sets.  This tab set is
;; useful to show the existing tab sets in a tab bar, and switch
;; between them easily.  The function `tabbar-get-tabsets-tabset'
;; returns this special tab set.
;;
;;
;; Buffer tabs
;; -----------
;;
;; The default tab bar implementation provided displays buffers in
;; dedicated tabs.  Selecting a tab, switch (mouse-1), or pop
;; (mouse-2), to the buffer it contains.
;;
;; The list of buffers put in tabs is provided by the function
;; specified in the variable `tabbar-buffer-list-function'.  The
;; default function: `tabbar-buffer-list', excludes buffers whose name
;; starts with a space, when they are not visiting a file.
;;
;; Buffers are organized in groups, each one represented by a tab set.
;; A buffer can have no group, or belong to more than one group.  The
;; function specified by the variable `tabbar-buffer-groups-function'
;; is called for each buffer to obtain the groups it belongs to.  The
;; default function provided: `tabbar-buffer-groups' organizes buffers
;; depending on their major mode (see that function for details).
;;
;; The "home" button toggles display of buffer groups on the tab bar,
;; allowing to easily show another buffer group by clicking on the
;; associated tab.
;;
;; Known problems:
;;
;; Bug item #858306 at <http://sf.net/tracker/?group_id=79309>:
;; tabbar-mode crashes GNU Emacs 21.3 on MS-Windows 98/95.
;;

;;; History:
;;

;;; Code:

;;; Options
;;

(defgroup tabbar nil
  "Display a tab bar in the header line."
  :group 'convenience)

(defcustom tabbar-cycle-scope nil
  "*Specify the scope of cyclic navigation through tabs.
The following scopes are possible:

- `tabs'
    Navigate through visible tabs only.
- `groups'
    Navigate through tab groups only.
- default
    Navigate through visible tabs, then through tab groups."
  :group 'tabbar
  :type '(choice :tag "Cycle through..."
                 (const :tag "Visible Tabs Only" tabs)
                 (const :tag "Tab Groups Only" groups)
                 (const :tag "Visible Tabs then Tab Groups" nil)))

(defcustom tabbar-auto-scroll-flag t
  "*Non-nil means to automatically scroll the tab bar.
That is, when a tab is selected outside of the tab bar visible area,
the tab bar is scrolled horizontally so the selected tab becomes
visible."
  :group 'tabbar
  :type 'boolean)

(defvar tabbar-inhibit-functions '(tabbar-default-inhibit-function)
  "List of functions to be called before displaying the tab bar.
Those functions are called one by one, with no arguments, until one of
them returns a non-nil value, and thus, prevents to display the tab
bar.")

(defvar tabbar-current-tabset-function nil
  "Function called with no argument to obtain the current tab set.
This is the tab set displayed on the tab bar.")

(defvar tabbar-tab-label-function nil
  "Function that obtains a tab label displayed on the tab bar.
The function is passed a tab and should return a string.")

(defvar tabbar-select-tab-function nil
  "Function that select a tab.
The function is passed a mouse event and a tab, and should make it the
selected tab.")

(defvar tabbar-help-on-tab-function nil
  "Function to obtain a help string for a tab.
The help string is displayed when the mouse is onto the button.  The
function is passed the tab and should return a help string or nil for
none.")

(defvar tabbar-button-label-function nil
  "Function that obtains a button label displayed on the tab bar.
The function is passed a button name should return a propertized
string to display.")

(defvar tabbar-home-function nil
  "Function called when clicking on the tab bar home button.
The function is passed the mouse event received.")

(defvar tabbar-home-help-function nil
  "Function to obtain a help string for the tab bar home button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

(defvar tabbar-scroll-left-function 'tabbar-scroll-left
  "Function that scrolls tabs on left.
The function is passed the mouse event received when clicking on the
scroll left button.  It should scroll the current tab set.")

(defvar tabbar-scroll-left-help-function 'tabbar-scroll-left-help
  "Function to obtain a help string for the scroll left button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

(defvar tabbar-scroll-right-function 'tabbar-scroll-right
  "Function that scrolls tabs on right.
The function is passed the mouse event received when clicking on the
scroll right button.  It should scroll the current tab set.")

(defvar tabbar-scroll-right-help-function 'tabbar-scroll-right-help
  "Function to obtain a help string for the scroll right button.
The help string is displayed when the mouse is onto the button.
The function is called with no arguments.")

;;; Misc.
;;
(eval-and-compile
  (defalias 'tabbar-display-update
    (if (fboundp 'force-window-update)
        #'(lambda () (force-window-update (selected-window)))
      'force-mode-line-update)))

(defsubst tabbar-click-p (event)
  "Return non-nil if EVENT is a mouse click event."
  (memq 'click (event-modifiers event)))

(defun tabbar-shorten (str width)
  "Return a shortened string from STR that fits in the given display WIDTH.
WIDTH is specified in terms of character display width in the current
buffer; see also `char-width'.  If STR display width is greater than
WIDTH, STR is truncated and an ellipsis string \"...\" is inserted at
end or in the middle of the returned string, depending on available
room."
  (let* ((n  (length str))
         (sw (string-width str))
         (el "...")
         (ew (string-width el))
         (w  0)
         (i  0))
    (cond
     ;; STR fit in WIDTH, return it.
     ((<= sw width)
      str)
     ;; There isn't enough room for the ellipsis, STR is just
     ;; truncated to fit in WIDTH.
     ((<= width ew)
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (substring str 0 i))
     ;; There isn't enough room to insert the ellipsis in the middle
     ;; of the truncated string, so put the ellipsis at end.
     ((zerop (setq sw (/ (- width ew) 2)))
      (setq width (- width ew))
      (while (< w width)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (concat (substring str 0 i) el))
     ;; Put the ellipsis in the middle of the truncated string.
     (t
      (while (< w sw)
        (setq w (+ w (char-width (aref str i)))
              i (1+ i)))
      (setq w (+ w ew))
      (while (< w width)
        (setq n (1- n)
              w (+ w (char-width (aref str n)))))
      (concat (substring str 0 i) el (substring str n)))
     )))

;;; Tab and tab set
;;
(defsubst tabbar-make-tab (object tabset)
  "Return a new tab with value OBJECT.
TABSET is the tab set the tab belongs to."
  (cons object tabset))

(defsubst tabbar-tab-value (tab)
  "Return the value of tab TAB."
  (car tab))

(defsubst tabbar-tab-tabset (tab)
  "Return the tab set TAB belongs to."
  (cdr tab))

(defvar tabbar-tabsets nil
  "The tab sets store.")

(defvar tabbar-tabsets-tabset nil
  "The special tab set of existing tab sets.")

(defvar tabbar-current-tabset nil
  "The tab set currently displayed on the tab bar.")
(make-variable-buffer-local 'tabbar-current-tabset)

(defvar tabbar-init-hook nil
  "Hook run after tab bar data has been initialized.
You should use this hook to initialize dependent data.")

(defsubst tabbar-init-tabsets-store ()
  "Initialize the tab set store."
  (setq tabbar-tabsets (make-vector 31 0)
        tabbar-tabsets-tabset (make-symbol "tabbar-tabsets-tabset"))
  (put tabbar-tabsets-tabset 'start 0)
  (run-hooks 'tabbar-init-hook))

(defvar tabbar-quit-hook nil
  "Hook run after tab bar data has been freed.
You should use this hook to reset dependent data.")

(defsubst tabbar-free-tabsets-store ()
  "Free the tab set store."
  (setq tabbar-tabsets nil
        tabbar-tabsets-tabset nil)
  (run-hooks 'tabbar-quit-hook))

;; Define an "hygienic" function free of side effect between its local
;; variables and those of the callee.
(eval-and-compile
  (defalias 'tabbar-map-tabsets
    (let ((function (make-symbol "function"))
          (result   (make-symbol "result"))
          (tabset   (make-symbol "tabset")))
      `(lambda (,function)
         "Apply FUNCTION to each tab set, and make a list of the results.
The result is a list just as long as the number of existing tab sets."
         (let (,result)
           (if tabbar-tabsets
         (mapatoms
    #'(lambda (,tabset)
        (push (funcall ,function ,tabset) ,result))
    tabbar-tabsets))
           ,result)))))

(defun tabbar-make-tabset (name &rest objects)
  "Make a new tab set whose name is the string NAME.
It is initialized with tabs build from the list of OBJECTS."
  (let* ((tabset (intern name tabbar-tabsets))
         (tabs (mapcar #'(lambda (object)
                           (tabbar-make-tab object tabset))
                       objects)))
    (set tabset tabs)
    (put tabset 'select (car tabs))
    (put tabset 'start 0)
    tabset))

(defsubst tabbar-get-tabset (name)
  "Return the tab set whose name is the string NAME.
Return nil if not found."
  (intern-soft name tabbar-tabsets))

(defsubst tabbar-delete-tabset (tabset)
  "Delete the tab set TABSET.
That is, remove it from the tab sets store."
  (unintern tabset tabbar-tabsets))

(defsubst tabbar-tabs (tabset)
  "Return the list of tabs in TABSET."
  (symbol-value tabset))

(defsubst tabbar-tab-values (tabset)
  "Return the list of tab values in TABSET."
  (mapcar 'tabbar-tab-value (tabbar-tabs tabset)))

(defsubst tabbar-get-tab (object tabset)
  "Search for a tab with value OBJECT in TABSET.
Return the tab found, or nil if not found."
  (assoc object (tabbar-tabs tabset)))

(defsubst tabbar-member (tab tabset)
  "Return non-nil if TAB is in TABSET."
  (or (eq (tabbar-tab-tabset tab) tabset)
      (memq tab (tabbar-tabs tabset))))

(defsubst tabbar-template (tabset)
  "Return the cached visual representation of TABSET.
That is, a `header-line-format' template, or nil if the cache is
empty."
  (get tabset 'template))

(defsubst tabbar-set-template (tabset template)
  "Set the cached visual representation of TABSET to TEMPLATE.
TEMPLATE must be a valid `header-line-format' template, or nil to
cleanup the cache."
  (put tabset 'template template))

(defsubst tabbar-selected-tab (tabset)
  "Return the tab selected in TABSET."
  (get tabset 'select))

(defsubst tabbar-selected-value (tabset)
  "Return the value of the tab selected in TABSET."
  (tabbar-tab-value (tabbar-selected-tab tabset)))

(defsubst tabbar-selected-p (tab tabset)
  "Return non-nil if TAB is the selected tab in TABSET."
  (eq tab (tabbar-selected-tab tabset)))

(defsubst tabbar-modified-p (tab tabset)
  "Return non-nil if TAB is a modified tab in TABSET."
  (and (buffer-modified-p (tabbar-tab-value tab))
       (buffer-file-name (tabbar-tab-value tab))))

(defvar tabbar--track-selected nil)

(defsubst tabbar-select-tab (tab tabset)
  "Make TAB the selected tab in TABSET.
Does nothing if TAB is not found in TABSET.
Return TAB if selected, nil if not."
  (when (tabbar-member tab tabset)
    (unless (tabbar-selected-p tab tabset)
      (tabbar-set-template tabset nil)
      (setq tabbar--track-selected tabbar-auto-scroll-flag))
    (put tabset 'select tab)))

(defsubst tabbar-select-tab-value (object tabset)
  "Make the tab with value OBJECT, the selected tab in TABSET.
Does nothing if a tab with value OBJECT is not found in TABSET.
Return the tab selected, or nil if nothing was selected."
  (tabbar-select-tab (tabbar-get-tab object tabset) tabset))

(defsubst tabbar-start (tabset)
  "Return the index of the first visible tab in TABSET."
  (get tabset 'start))

(defsubst tabbar-view (tabset)
  "Return the list of visible tabs in TABSET.
That is, the sub-list of tabs starting at the first visible one."
  (nthcdr (tabbar-start tabset) (tabbar-tabs tabset)))

(defun tabbar-add-tab (tabset object &optional append)
  "Add to TABSET a tab with value OBJECT if there isn't one there yet.
If the tab is added, it is added at the beginning of the tab list,
unless the optional argument APPEND is non-nil, in which case it is
added at the end."
  (let ((tabs (tabbar-tabs tabset)))
    (if (tabbar-get-tab object tabset)
        tabs
      (let ((tab (tabbar-make-tab object tabset)))
        (tabbar-set-template tabset nil)
        (set tabset (if append
                        (append tabs (list tab))
                      (cons tab tabs)))))))

(defun tabbar-delete-tab (tab)
  "Remove TAB from its tab set."
  (let* ((tabset (tabbar-tab-tabset tab))
         (tabs   (tabbar-tabs tabset))
         (sel    (eq tab (tabbar-selected-tab tabset)))
         (next   (and sel (cdr (memq tab tabs)))))
    (tabbar-set-template tabset nil)
    (setq tabs (delq tab tabs))
    ;; When the selected tab is deleted, select the next one, if
    ;; available, or the last one otherwise.
    (and sel (tabbar-select-tab (car (or next (last tabs))) tabset))
    (set tabset tabs)))

(defun tabbar-scroll (tabset count)
  "Scroll the visible tabs in TABSET of COUNT units.
If COUNT is positive move the view on right.  If COUNT is negative,
move the view on left."
  (let ((start (min (max 0 (+ (tabbar-start tabset) count))
                    (1- (length (tabbar-tabs tabset))))))
    (when (/= start (tabbar-start tabset))
      (tabbar-set-template tabset nil)
      (put tabset 'start start))))

(defun tabbar-tab-next (tabset tab &optional before)
  "Search in TABSET for the tab after TAB.
If optional argument BEFORE is non-nil, search for the tab before
TAB.  Return the tab found, or nil otherwise."
  (let* (last (tabs (tabbar-tabs tabset)))
    (while (and tabs (not (eq tab (car tabs))))
      (setq last (car tabs)
            tabs (cdr tabs)))
    (and tabs (if before last (nth 1 tabs)))))

(defun tabbar-current-tabset (&optional update)
  "Return the tab set currently displayed on the tab bar.
If optional argument UPDATE is non-nil, call the user defined function
`tabbar-current-tabset-function' to obtain it.  Otherwise return the
current cached copy."
  (and update tabbar-current-tabset-function
       (setq tabbar-current-tabset
             (funcall tabbar-current-tabset-function)))
  tabbar-current-tabset)

(defun tabbar-get-tabsets-tabset ()
  "Return the tab set of selected tabs in existing tab sets."
  (set tabbar-tabsets-tabset (tabbar-map-tabsets 'tabbar-selected-tab))
  (tabbar-scroll tabbar-tabsets-tabset 0)
  (tabbar-set-template tabbar-tabsets-tabset nil)
  tabbar-tabsets-tabset)

;;; Faces
;;
(defface tabbar-default
  '((((class color grayscale) (background dark))
     :inherit variable-pitch
     :height 0.8
     :foreground "grey75"
     :background "gray50"
     )
    (((class mono) (background light))
     :inherit variable-pitch
     :height 0.8
     :foreground "black"
     :background "white"
     )
    (((class mono) (background dark))
     :inherit variable-pitch
     :height 0.8
     :foreground "white"
     :background "black"
     )
    (t
     :inherit variable-pitch
     :height 0.8
     :foreground "gray50"
     :background "gray75"
     ))
  "Default face used in the tab bar."
  :group 'tabbar)

(defface tabbar-unselected
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style released-button)
     ))
  "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-selected
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style pressed-button)
     :foreground "blue"
     ))
  "Face used for the selected tab."
  :group 'tabbar)

(defface tabbar-modified
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style released-button)
     :foreground "green"
     ))
  "Face used for unsaved tabs."
  :group 'tabbar)

(defface tabbar-highlight
  '((t
     :underline t
     ))
  "Face used to highlight a tab during mouse-overs."
  :group 'tabbar)

(defface tabbar-separator
  '((t
     :inherit tabbar-default
     ))
  "Face used for separators between tabs."
  :group 'tabbar)

(defface tabbar-button
  '((t
     :inherit tabbar-default
     :box (:line-width 1 :color "white" :style released-button)
     ))
  "Face used for tab bar buttons."
  :group 'tabbar)

(defface tabbar-button-highlight
  '((t
     :inherit tabbar-default
     ))
  "Face used to highlight a button during mouse-overs."
  :group 'tabbar)

(defcustom tabbar-background-color nil
  "*Background color of the tab bar.
By default, use the background color specified for the
`tabbar-default' face (or inherited from another face), or the
background color of the `default' face otherwise."
  :group 'tabbar
  :type '(choice (const :tag "Default" nil)
                 (color)))

(defsubst tabbar-background-color ()
  "Return the background color of the tab bar."
  (or tabbar-background-color
      (let* ((face 'tabbar-default)
             (color (face-background face)))
        (while (null color)
          (or (facep (setq face (face-attribute face :inherit)))
              (setq face 'default))
          (setq color (face-background face)))
        color)))

;;; Buttons and separator look and feel
;;
(defconst tabbar-button-widget
  '(cons
    (cons :tag "Enabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    (cons :tag "Disabled"
          (string)
          (repeat :tag "Image"
                  :extra-offset 2
                  (restricted-sexp :tag "Spec"
                                   :match-alternatives (listp))))
    )
  "Widget for editing a tab bar button.
A button is specified as a pair (ENABLED-BUTTON . DISABLED-BUTTON),
where ENABLED-BUTTON and DISABLED-BUTTON specify the value used when
the button is respectively enabled and disabled.  Each button value is
a pair (STRING . IMAGE) where STRING is a string value, and IMAGE a
list of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING.
If only the ENABLED-BUTTON image is provided, a DISABLED-BUTTON image
is derived from it.")

;;; Home button
;;
(defvar tabbar-home-button-value nil
  "Value of the home button.")

;;; Scroll left button
;;
(defvar tabbar-scroll-left-button-value nil
  "Value of the scroll left button.")

;;; Scroll right button
;;
(defvar tabbar-scroll-right-button-value nil
  "Value of the scroll right button.")

;;; Separator
;;
(defconst tabbar-separator-widget
  '(cons (choice (string)
                 (number :tag "Space width" 0.2))
         (repeat :tag "Image"
                 :extra-offset 2
                 (restricted-sexp :tag "Spec"
                                  :match-alternatives (listp))))
  "Widget for editing a tab bar separator.
A separator is specified as a pair (STRING-OR-WIDTH . IMAGE) where
STRING-OR-WIDTH is a string value or a space width, and IMAGE a list
of image specifications.
If IMAGE is non-nil, try to use that image, else use STRING-OR-WIDTH.
The value (\"\"), or (0) hide separators.")

(defvar tabbar-separator-value nil
  "Value of the separator used between tabs.")

(defcustom tabbar-separator (list 0.2)
  "Separator used between tabs.
The variable `tabbar-separator-widget' gives details on this widget."
  :group 'tabbar
  :type tabbar-separator-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of separator value.
          (setq tabbar-separator-value nil)))

;;; Images
;;
(defcustom tabbar-use-images t
  "*Non-nil means to try to use images in tab bar.
That is for buttons and separators."
  :group 'tabbar
  :type 'boolean
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of all buttons and separator values.
          (setq tabbar-separator-value nil
                tabbar-home-button-value nil
                tabbar-scroll-left-button-value nil
                tabbar-scroll-right-button-value nil)))

;; the following cache only provides minor speed benefits
;; but it may be a workaround for the close-tab/undo.png display issue
(defvar tabbar-cached-image nil)
(defvar tabbar-cached-spec nil)
(defsubst tabbar-find-image (specs)
  "Find an image, choosing one of a list of image specifications.
SPECS is a list of image specifications.  See also `find-image'."
  (if (eq tabbar-cached-spec specs)
      tabbar-cached-image
    (when (and tabbar-use-images (display-images-p))
      (condition-case nil
    (prog1
        (setq tabbar-cached-image (find-image specs))
      (setq tabbar-cached-spec specs))
  (error nil)))))

(defsubst tabbar-disable-image (image)
  "From IMAGE, return a new image which looks disabled."
  (setq image (copy-sequence image))
  (setcdr image (plist-put (cdr image) :conversion 'disabled))
  image)

(defsubst tabbar-normalize-image (image &optional margin)
  "Make IMAGE centered and transparent.
If optional MARGIN is non-nil, it must be a number of pixels to add as
an extra margin around the image."
  (let ((plist (cdr image)))
    (or (plist-get plist :ascent)
        (setq plist (plist-put plist :ascent 'center)))
    (or (plist-get plist :mask)
        (setq plist (plist-put plist :mask '(heuristic t))))
    (or (not (natnump margin))
        (plist-get plist :margin)
        (plist-put plist :margin margin))
    (setcdr image plist))
  image)

;;; Button keymaps and callbacks
;;
(defun tabbar-make-mouse-keymap (callback)
  "Return a keymap that call CALLBACK on mouse events.
CALLBACK is passed the received mouse event."
  (let ((keymap (make-sparse-keymap)))
    ;; Pass mouse-1, mouse-2 and mouse-3 events to CALLBACK.
    (define-key keymap [header-line down-mouse-1] 'ignore)
    (define-key keymap [header-line mouse-1] callback)
    (define-key keymap [header-line down-mouse-2] 'ignore)
    (define-key keymap [header-line mouse-2] callback)
    (define-key keymap [header-line down-mouse-3] 'ignore)
    (define-key keymap [header-line mouse-3] callback)
    keymap))

(defsubst tabbar-make-mouse-event (&optional type)
  "Return a mouse click event.
Optional argument TYPE is a mouse-click event or one of the
symbols `mouse-1', `mouse-2' or `mouse-3'.
The default is `mouse-1'."
  (if (tabbar-click-p type)
      type
    (list (or (memq type '(mouse-2 mouse-3)) 'mouse-1)
          (or (event-start nil) ;; Emacs 21.4
              (list (selected-window) (point) '(0 . 0) 0)))))

;;; Buttons
;;
(defconst tabbar-default-button-keymap
  (tabbar-make-mouse-keymap 'tabbar-select-button-callback)
  "Default keymap of a button.")

(defun tabbar-help-on-button (window object position)
  "Return a help string or nil for none, for the button under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the button label under the mouse.
POSITION is the position in that label.
Call `tabbar-NAME-help-function' where NAME is the button name
associated to OBJECT."
  (let* ((name (get-text-property position 'tabbar-button object))
         (funvar (and name
                      (intern-soft (format "tabbar-%s-help-function"
                                           name)))))
    (and (symbol-value funvar)
         (funcall (symbol-value funvar)))))

(defsubst tabbar-click-on-button (name &optional type)
  "Handle a mouse click event on button NAME.
Call `tabbar-select-NAME-function' with the received, or simulated
mouse click event.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let ((funvar (intern-soft (format "tabbar-%s-function" name))))
    (when (symbol-value funvar)
      (funcall (symbol-value funvar) (tabbar-make-mouse-event type))
      (tabbar-display-update))))

(defun tabbar-select-button-callback (event)
  "Handle a mouse EVENT on a button.
Pass mouse click events on a button to `tabbar-click-on-button'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-button
       (get-text-property (cdr target) 'tabbar-button (car target))
       event))))

(defun tabbar-make-button-keymap (name)
  "Return a keymap to handle mouse click events on button NAME."
  (if (fboundp 'posn-string)
      tabbar-default-button-keymap
    (let ((event (make-symbol "event")))
      (tabbar-make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tabbar-click-p ,event)
               (tabbar-click-on-button ',name ,event)))))))

;;; Button callbacks
;;
(defun tabbar-scroll-left (event)
  "On mouse EVENT, scroll current tab set on left."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) -1)))

(defun tabbar-scroll-left-help ()
  "Help string shown when mouse is over the scroll left button."
  "mouse-1: scroll tabs left.")

(defun tabbar-scroll-right (event)
  "On mouse EVENT, scroll current tab set on right."
  (when (eq (event-basic-type event) 'mouse-1)
    (tabbar-scroll (tabbar-current-tabset) 1)))

(defun tabbar-scroll-right-help ()
  "Help string shown when mouse is over the scroll right button."
  "mouse-1: scroll tabs right.")

;;; Tabs
;;
(defconst tabbar-default-tab-keymap
  (tabbar-make-mouse-keymap 'tabbar-select-tab-callback)
  "Default keymap of a tab.")

(defun tabbar-help-on-tab (window object position)
  "Return a help string or nil for none, for the tab under the mouse.
WINDOW is the window in which the help was found (unused).
OBJECT is the tab label under the mouse.
POSITION is the position in that label.
Call `tabbar-help-on-tab-function' with the associated tab."
  (when tabbar-help-on-tab-function
    (let ((tab (get-text-property position 'tabbar-tab object)))
      (funcall tabbar-help-on-tab-function tab))))

(defsubst tabbar-click-on-tab (tab &optional type)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (when tabbar-select-tab-function
    (funcall tabbar-select-tab-function
             (tabbar-make-mouse-event type) tab)
    (tabbar-display-update)))

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (when (tabbar-click-p event)
    (let ((target (posn-string (event-start event))))
      (tabbar-click-on-tab
       (get-text-property (cdr target) 'tabbar-tab (car target))
       event))))

(defun tabbar-make-tab-keymap (tab)
  "Return a keymap to handle mouse click events on TAB."
  (if (fboundp 'posn-string)
      tabbar-default-tab-keymap
    (let ((event (make-symbol "event")))
      (tabbar-make-mouse-keymap
       `(lambda (,event)
          (interactive "@e")
          (and (tabbar-click-p ,event)
               (tabbar-click-on-tab ',tab ,event)))))))

;;; Tab bar construction
;;
(defun tabbar-button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-NAME-button'."
  (let* ((btn (symbol-value
               (intern-soft (format "tabbar-%s-button" name))))
         (on  (tabbar-find-image (cdar btn)))
         (off (and on (tabbar-find-image (cddr btn)))))
    (when on
      (tabbar-normalize-image on 1)
      (if off
          (tabbar-normalize-image off 1)
        ;; If there is no disabled button image, derive one from the
        ;; button enabled image.
        (setq off (tabbar-disable-image on))))
    (cons
     (propertize (or (caar btn) " ") 'display on)
     (propertize (or (cadr btn) " ") 'display off))))

(defun tabbar-line-button (name)
  "Return the display representation of button NAME.
That is, a propertized string used as an `header-line-format' template
element."
  (let ((label (if tabbar-button-label-function
                   (funcall tabbar-button-label-function name)
                 (cons name name))))
    ;; Cache the display value of the enabled/disabled buttons in
    ;; variables `tabbar-NAME-button-value'.
    (set (intern (format "tabbar-%s-button-value"  name))
         (cons
          (propertize (car label)
                      'tabbar-button name
                      'face 'tabbar-button
                      'mouse-face 'tabbar-button-highlight
                      'pointer 'hand
                      'local-map (tabbar-make-button-keymap name)
                      'help-echo 'tabbar-help-on-button)
          (propertize (cdr label)
                      'face 'tabbar-button
                      'pointer 'arrow)))))

(defun tabbar-line-separator ()
  "Return the display representation of a tab bar separator.
That is, a propertized string used as an `header-line-format' template
element."
  (let ((image (tabbar-find-image (cdr tabbar-separator))))
    ;; Cache the separator display value in variable
    ;; `tabbar-separator-value'.
    (setq tabbar-separator-value
          (cond
           (image
            (propertize " "
                        'face 'tabbar-separator
                        'pointer 'arrow
                        'display (tabbar-normalize-image image)))
           ((numberp (car tabbar-separator))
            (propertize " "
                        'face 'tabbar-separator
                        'pointer 'arrow
                        'display (list 'space
                                       :width (car tabbar-separator))))
           ((propertize (or (car tabbar-separator) " ")
                        'face 'tabbar-separator
                        'pointer 'arrow))))
    ))

(defsubst tabbar-line-buttons (tabset)
  "Return a list of propertized strings for tab bar buttons.
TABSET is the tab set used to choose the appropriate buttons."
  (list
   (if tabbar-home-function
       (car tabbar-home-button-value)
     (cdr tabbar-home-button-value))
   (if (> (tabbar-start tabset) 0)
       (car tabbar-scroll-left-button-value)
     (cdr tabbar-scroll-left-button-value))
   (if (< (tabbar-start tabset)
          (1- (length (tabbar-tabs tabset))))
       (car tabbar-scroll-right-button-value)
     (cdr tabbar-scroll-right-button-value))
   tabbar-separator-value))

(defsubst tabbar-line-tab (tab)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (concat (propertize
           (if tabbar-tab-label-function
               (funcall tabbar-tab-label-function tab)
             tab)
           'tabbar-tab tab
           'local-map (tabbar-make-tab-keymap tab)
           'help-echo 'tabbar-help-on-tab
           'mouse-face 'tabbar-highlight
           'face (cond ((tabbar-selected-p tab (tabbar-current-tabset))
                        'tabbar-selected)
                       ((tabbar-modified-p tab (tabbar-current-tabset))
                        'tabbar-modified)
                       (t 'tabbar-unselected))
           'pointer 'hand)
          tabbar-separator-value))

(defun tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
         (tabs (tabbar-view tabset))
         (padcolor (tabbar-background-color))
         atsel elts)
    ;; Initialize buttons and separator values.
    (or tabbar-separator-value
        (tabbar-line-separator))
    (or tabbar-home-button-value
        (tabbar-line-button 'home))
    (or tabbar-scroll-left-button-value
        (tabbar-line-button 'scroll-left))
    (or tabbar-scroll-right-button-value
        (tabbar-line-button 'scroll-right))
    ;; Track the selected tab to ensure it is always visible.
    (when tabbar--track-selected
      (while (not (memq sel tabs))
        (tabbar-scroll tabset -1)
        (setq tabs (tabbar-view tabset)))
      (while (and tabs (not atsel))
        (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
              atsel (eq (car tabs) sel)
              tabs  (cdr tabs)))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tabbar-line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tabbar--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (setq elts (cons (tabbar-line-tab (car tabs)) elts)
            tabs (cdr tabs)))
    ;; Cache and return the new tab bar.
    (tabbar-set-template
     tabset
     (list (tabbar-line-buttons tabset)
           (nreverse elts)
           (propertize "%-"
                       'face (list :background padcolor
                                   :foreground padcolor)
                       'pointer 'arrow)))
    ))

(defun tabbar-line ()
  "Return the header line templates that represent the tab bar.
Inhibit display of the tab bar in current window if any of the
`tabbar-inhibit-functions' return non-nil."
  (cond
   ((run-hook-with-args-until-success 'tabbar-inhibit-functions)
    ;; Don't show the tab bar.
    (setq header-line-format nil))
   ((tabbar-current-tabset t)
    ;; When available, use a cached tab bar value, else recompute it.
    (or (tabbar-template tabbar-current-tabset)
        (tabbar-line-format tabbar-current-tabset)))))

(defconst tabbar-header-line-format '(:eval (tabbar-line))
  "The tab bar header line format.")

(defun tabbar-default-inhibit-function ()
  "Inhibit display of the tab bar in specified windows.
That is dedicated windows, and `checkdoc' status windows."
  (or (window-dedicated-p (selected-window))
      (member (buffer-name)
              (list " *Checkdoc Status*"
                    (if (boundp 'ispell-choices-buffer)
                        ispell-choices-buffer
                      "*Choices*")))))

;;; Cyclic navigation through tabs
;;
(defun tabbar-cycle (&optional backward type)
  "Cycle to the next available tab.
The scope of the cyclic navigation through tabs is specified by the
option `tabbar-cycle-scope'.
If optional argument BACKWARD is non-nil, cycle to the previous tab
instead.
Optional argument TYPE is a mouse event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((tabset (tabbar-current-tabset t))
         (ttabset (tabbar-get-tabsets-tabset))
         ;; If navigation through groups is requested, and there is
         ;; only one group, navigate through visible tabs.
         (cycle (if (and (eq tabbar-cycle-scope 'groups)
                         (not (cdr (tabbar-tabs ttabset))))
                    'tabs
                  tabbar-cycle-scope))
         selected tab)
    (when tabset
      (setq selected (tabbar-selected-tab tabset))
      (cond
       ;; Cycle through visible tabs only.
       ((eq cycle 'tabs)
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no tab after/before the selected one, cycle
        ;; to the first/last visible tab.
        (unless tab
          (setq tabset (tabbar-tabs tabset)
                tab (car (if backward (last tabset) tabset))))
        )
       ;; Cycle through tab groups only.
       ((eq cycle 'groups)
        (setq tab (tabbar-tab-next ttabset selected backward))
        ;; When there is no group after/before the selected one, cycle
        ;; to the first/last available group.
        (unless tab
          (setq tabset (tabbar-tabs ttabset)
                tab (car (if backward (last tabset) tabset))))
        )
       (t
        ;; Cycle through visible tabs then tab groups.
        (setq tab (tabbar-tab-next tabset selected backward))
        ;; When there is no visible tab after/before the selected one,
        ;; cycle to the next/previous available group.
        (unless tab
          (setq tab (tabbar-tab-next ttabset selected backward))
          ;; When there is no next/previous group, cycle to the
          ;; first/last available group.
          (unless tab
            (setq tabset (tabbar-tabs ttabset)
                  tab (car (if backward (last tabset) tabset))))
          ;; Select the first/last visible tab of the new group.
          (setq tabset (tabbar-tabs (tabbar-tab-tabset tab))
                tab (car (if backward (last tabset) tabset))))
        ))
      (tabbar-click-on-tab tab type))))

;;;###autoload
(defun tabbar-backward ()
  "Select the previous available tab.
Depend on the setting of the option `tabbar-cycle-scope'."
  (interactive)
  (tabbar-cycle t))

;;;###autoload
(defun tabbar-forward ()
  "Select the next available tab.
Depend on the setting of the option `tabbar-cycle-scope'."
  (interactive)
  (tabbar-cycle))

;;;###autoload
(defun tabbar-backward-group ()
  "Go to selected tab in the previous available group."
  (interactive)
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle t)))

;;;###autoload
(defun tabbar-forward-group ()
  "Go to selected tab in the next available group."
  (interactive)
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle)))

;;;###autoload
(defun tabbar-backward-tab ()
  "Select the previous visible tab."
  (interactive)
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle t)))

;;;###autoload
(defun tabbar-forward-tab ()
  "Select the next visible tab."
  (interactive)
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle)))

;;; Button press commands
;;
(defsubst tabbar--mouse (number)
  "Return a mouse button symbol from NUMBER.
That is mouse-2, or mouse-3 when NUMBER is respectively 2, or 3.
Return mouse-1 otherwise."
  (cond ((eq number 2) 'mouse-2)
        ((eq number 3) 'mouse-3)
        ('mouse-1)))

;;;###autoload
(defun tabbar-press-home (&optional arg)
  "Press the tab bar home button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tabbar-click-on-button 'home (tabbar--mouse arg)))

;;;###autoload
(defun tabbar-press-scroll-left (&optional arg)
  "Press the tab bar scroll-left button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tabbar-click-on-button 'scroll-left (tabbar--mouse arg)))

;;;###autoload
(defun tabbar-press-scroll-right (&optional arg)
  "Press the tab bar scroll-right button.
That is, simulate a mouse click on that button.
A numeric prefix ARG value of 2, or 3, respectively simulates a
mouse-2, or mouse-3 click.  The default is a mouse-1 click."
  (interactive "p")
  (tabbar-click-on-button 'scroll-right (tabbar--mouse arg)))

;;; Mouse-wheel support
;;
(require 'mwheel)

;;; Compatibility
;;
(defconst tabbar--mwheel-up-event
  (symbol-value (if (boundp 'mouse-wheel-up-event)
                    'mouse-wheel-up-event
                  'mouse-wheel-up-button)))

(defconst tabbar--mwheel-down-event
  (symbol-value (if (boundp 'mouse-wheel-down-event)
                    'mouse-wheel-down-event
                  'mouse-wheel-down-button)))

(defsubst tabbar--mwheel-key (event-type)
  "Return a mouse wheel key symbol from EVENT-TYPE.
When EVENT-TYPE is a symbol return it.
When it is a button number, return symbol `mouse-<EVENT-TYPE>'."
  (if (symbolp event-type)
      event-type
    (intern (format "mouse-%s" event-type))))

(defsubst tabbar--mwheel-up-p (event)
  "Return non-nil if EVENT is a mouse-wheel up event."
  (let ((x (event-basic-type event)))
    (if (eq 'mouse-wheel x)
        (< (car (cdr (cdr event))) 0)   ;; Emacs 21.3
      ;; Emacs > 21.3
      (eq x tabbar--mwheel-up-event))))

;;; Basic commands
;;
;;;###autoload
(defun tabbar-mwheel-backward (event)
  "Select the previous available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward'."
  (interactive "@e")
  (tabbar-cycle t event))

;;;###autoload
(defun tabbar-mwheel-forward (event)
  "Select the next available tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward'."
  (interactive "@e")
  (tabbar-cycle nil event))

;;;###autoload
(defun tabbar-mwheel-backward-group (event)
  "Go to selected tab in the previous available group.
If there is only one group, select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-group'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle t event)))

;;;###autoload
(defun tabbar-mwheel-forward-group (event)
  "Go to selected tab in the next available group.
If there is only one group, select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-group'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'groups))
    (tabbar-cycle nil event)))

;;;###autoload
(defun tabbar-mwheel-backward-tab (event)
  "Select the previous visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-backward-tab'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle t event)))

;;;###autoload
(defun tabbar-mwheel-forward-tab (event)
  "Select the next visible tab.
EVENT is the mouse event that triggered this command.
Mouse-enabled equivalent of the command `tabbar-forward-tab'."
  (interactive "@e")
  (let ((tabbar-cycle-scope 'tabs))
    (tabbar-cycle nil event)))

;;; Wrappers when there is only one generic mouse-wheel event
;;
;;;###autoload
(defun tabbar-mwheel-switch-tab (event)
  "Select the next or previous tab according to EVENT."
  (interactive "@e")
  (if (tabbar--mwheel-up-p event)
      (tabbar-mwheel-forward-tab event)
    (tabbar-mwheel-backward-tab event)))

;;;###autoload
(defun tabbar-mwheel-switch-group (event)
  "Select the next or previous group of tabs according to EVENT."
  (interactive "@e")
  (if (tabbar--mwheel-up-p event)
      (tabbar-mwheel-forward-group event)
    (tabbar-mwheel-backward-group event)))

;;; Minor modes
;;
(defsubst tabbar-mode-on-p ()
  "Return non-nil if Tabbar mode is on."
  (eq (default-value 'header-line-format)
      tabbar-header-line-format))

;;; Tabbar-Local mode
;;
(defvar tabbar--local-hlf nil)

;;;###autoload
(define-minor-mode tabbar-local-mode
  "Toggle local display of the tab bar.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.
When turned on, if a local header line is shown, it is hidden to show
the tab bar.  The tab bar is locally hidden otherwise.  When turned
off, if a local header line is hidden or the tab bar is locally
hidden, it is shown again.  Signal an error if Tabbar mode is off."
  :group 'tabbar
  :global nil
  (unless (tabbar-mode-on-p)
    (error "Tabbar mode must be enabled"))
;;; ON
  (if tabbar-local-mode
      (if (and (local-variable-p 'header-line-format)
               header-line-format)
          ;; A local header line exists, hide it to show the tab bar.
          (progn
            ;; Fail in case of an inconsistency because another local
            ;; header line is already hidden.
            (when (local-variable-p 'tabbar--local-hlf)
              (error "Another local header line is already hidden"))
            (set (make-local-variable 'tabbar--local-hlf)
                 header-line-format)
            (kill-local-variable 'header-line-format))
        ;; Otherwise hide the tab bar in this buffer.
        (setq header-line-format nil))
;;; OFF
    (if (local-variable-p 'tabbar--local-hlf)
        ;; A local header line is hidden, show it again.
        (progn
          (setq header-line-format tabbar--local-hlf)
          (kill-local-variable 'tabbar--local-hlf))
      ;; The tab bar is locally hidden, show it again.
      (kill-local-variable 'header-line-format))))

;;; Tabbar mode
;;
(defvar tabbar-prefix-key [(control ?c)]
  "The common prefix key used in Tabbar mode.")

(defvar tabbar-prefix-map
  (let ((km (make-sparse-keymap)))
    (define-key km [(control home)]  'tabbar-press-home)
    (define-key km [(control left)]  'tabbar-backward)
    (define-key km [(control right)] 'tabbar-forward)
    (define-key km [(control up)]    'tabbar-backward-group)
    (define-key km [(control down)]  'tabbar-forward-group)
    (define-key km [(control prior)] 'tabbar-press-scroll-left)
    (define-key km [(control next)]  'tabbar-press-scroll-right)
    (define-key km [(control f10)]   'tabbar-local-mode)
    km)
  "The key bindings provided in Tabbar mode.")

(defvar tabbar-mode-map
  (let ((km (make-sparse-keymap)))
    (define-key km tabbar-prefix-key tabbar-prefix-map)
    km)
  "Keymap to use in  Tabbar mode.")

(defvar tabbar--global-hlf nil)

;;;###autoload
(define-minor-mode tabbar-mode
  "Toggle display of a tab bar in the header line.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mode-map}"
  :group 'tabbar
  :require 'tabbar
  :global t
  :keymap tabbar-mode-map
  (if tabbar-mode
;;; ON
      (unless (tabbar-mode-on-p)
        ;; Save current default value of `header-line-format'.
        (setq tabbar--global-hlf (default-value 'header-line-format))
        (tabbar-init-tabsets-store)
        (setq-default header-line-format tabbar-header-line-format)
  (if (fboundp 'tabbar-define-access-keys) (tabbar-define-access-keys)))
;;; OFF
    (when (tabbar-mode-on-p)
      ;; Turn off Tabbar-Local mode globally.
      (mapc #'(lambda (b)
                (condition-case nil
                    (with-current-buffer b
                      (and tabbar-local-mode
                           (tabbar-local-mode -1)))
                  (error nil)))
            (buffer-list))
      ;; Restore previous `header-line-format'.
      (setq-default header-line-format tabbar--global-hlf)
      (tabbar-free-tabsets-store))
    ))

;;; Tabbar-Mwheel mode
;;
(defvar tabbar-mwheel-mode-map
  (let ((km (make-sparse-keymap)))
    (if (get 'mouse-wheel 'event-symbol-elements)
        ;; Use one generic mouse wheel event
        (define-key km [A-mouse-wheel]
          'tabbar-mwheel-switch-group)
      ;; Use separate up/down mouse wheel events
      (let ((up   (tabbar--mwheel-key tabbar--mwheel-up-event))
            (down (tabbar--mwheel-key tabbar--mwheel-down-event)))
        (define-key km `[header-line ,down]
          'tabbar-mwheel-backward-group)
        (define-key km `[header-line ,up]
          'tabbar-mwheel-forward-group)
        (define-key km `[header-line (control ,down)]
          'tabbar-mwheel-backward-tab)
        (define-key km `[header-line (control ,up)]
          'tabbar-mwheel-forward-tab)
        (define-key km `[header-line (shift ,down)]
          'tabbar-mwheel-backward)
        (define-key km `[header-line (shift ,up)]
          'tabbar-mwheel-forward)
        ))
    km)
  "Keymap to use in Tabbar-Mwheel mode.")

;;;###autoload
(define-minor-mode tabbar-mwheel-mode
  "Toggle use of the mouse wheel to navigate through tabs or groups.
With prefix argument ARG, turn on if positive, otherwise off.
Returns non-nil if the new state is enabled.

\\{tabbar-mwheel-mode-map}"
  :group 'tabbar
  :require 'tabbar
  :global t
  :keymap tabbar-mwheel-mode-map
  (when tabbar-mwheel-mode
    (unless (and (boundp 'mouse-wheel-mode)
                 mouse-wheel-mode
                 tabbar-mode)
      (tabbar-mwheel-mode -1))))

(defun tabbar-mwheel-follow ()
  "Toggle Tabbar-Mwheel following Tabbar and Mouse-Wheel modes."
  (if (boundp 'mouse-wheel-mode)
      (tabbar-mwheel-mode (if (and mouse-wheel-mode tabbar-mode) 1 -1))))

(add-hook 'tabbar-mode-hook      'tabbar-mwheel-follow)
(add-hook 'mouse-wheel-mode-hook 'tabbar-mwheel-follow)

;;; Buffer tabs
;;
(defgroup tabbar-buffer nil
  "Display buffers in the tab bar."
  :group 'tabbar)

(defcustom tabbar-buffer-home-button
  nil
  "The home button displayed when showing buffer tabs.
The enabled button value is displayed when showing tabs for groups of
buffers, and the disabled button value is displayed when showing
buffer tabs.
The variable `tabbar-button-widget' gives details on this widget."
  :group 'tabbar-buffer
  :type tabbar-button-widget
  :set '(lambda (variable value)
          (custom-set-default variable value)
          ;; Schedule refresh of button value.
          (setq tabbar-home-button-value nil)))

(defvar tabbar-buffer-list-function 'tabbar-buffer-list
  "Function that returns the list of buffers to show in tabs.
That function is called with no arguments and must return a list of
buffers.")

(defvar tabbar-buffer-groups-function 'tabbar-buffer-groups
  "Function that gives the group names the current buffer belongs to.
It must return a list of group names, or nil if the buffer has no
group.  Notice that it is better that a buffer belongs to one group.")

(defun tabbar-buffer-list ()
  "Return the list of buffers to show in tabs.
Exclude buffers whose name starts with a space, when they are not
visiting a file.  The current buffer is always included."
  (delq nil
        (mapcar #'(lambda (b)
                    (cond
                     ;; Always include the current buffer.
                     ((eq (current-buffer) b) b)
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((buffer-live-p b) b)))
                (buffer-list))))

(defun tabbar-buffer-mode-derived-p (mode parents)
  "Return non-nil if MODE derives from a mode in PARENTS."
  (let (derived)
    (while (and (not derived) mode)
      (if (memq mode parents)
          (setq derived t)
        (setq mode (get mode 'derived-mode-parent))))
    derived))

(defun tabbar-buffer-groups ()
  "Return the list of group names the current buffer belongs to.
Return a list of one element based on major mode."
  (list
   (cond
    ((or (get-buffer-process (current-buffer))
         ;; Check if the major mode derives from `comint-mode' or
         ;; `compilation-mode'.
         (tabbar-buffer-mode-derived-p
          major-mode '(comint-mode compilation-mode)))
     "Process"
     )
    ((member (buffer-name)
             '("*scratch*" "*Messages*"))
     "Common"
     )
    ((eq major-mode 'dired-mode)
     "Dired"
     )
    ((memq major-mode
           '(help-mode apropos-mode Info-mode Man-mode))
     "Help"
     )
    ((memq major-mode
           '(rmail-mode
             rmail-edit-mode vm-summary-mode vm-mode mail-mode
             mh-letter-mode mh-show-mode mh-folder-mode
             gnus-summary-mode message-mode gnus-group-mode
             gnus-article-mode score-mode gnus-browse-killed-mode))
     "Mail"
     )
    (t
     ;; Return `mode-name' if not blank, `major-mode' otherwise.
     (if (and (stringp mode-name)
              ;; Take care of preserving the match-data because this
              ;; function is called when updating the header line.
              (save-match-data (string-match "[^ ]" mode-name)))
         mode-name
       (symbol-name major-mode))
     ))))

;;; Group buffers in tab sets.
;;
(defvar tabbar--buffers nil)

(defun tabbar-buffer-update-groups ()
  "Update tab sets from groups of existing buffers.
Return the the first group where the current buffer is."
  (let ((bl (sort
             (mapcar
        ;; for each buffer, create list: buffer, buffer name, groups-list
        ;; sort on buffer name; store to bl (buffer list)
              #'(lambda (b)
                  (with-current-buffer b
                    (list (current-buffer)
                          (buffer-name)
                          (if tabbar-buffer-groups-function
                              (funcall tabbar-buffer-groups-function)
                            '("Common")))))
              (and tabbar-buffer-list-function
                   (funcall tabbar-buffer-list-function)))
             #'(lambda (e1 e2)
                 (string-lessp (nth 1 e1) (nth 1 e2))))))
    ;; If the cache has changed, update the tab sets.
    (unless (equal bl tabbar--buffers)
      ;; Add new buffers, or update changed ones.
      (dolist (e bl) ;; loop through buffer list
        (dolist (g (nth 2 e)) ;; for each member of groups-list for current buffer
          (let ((tabset (tabbar-get-tabset g))) ;; get group from group name
            (if tabset ;; if group exists
    ;; check if current buffer is same as any cached buffer
    ;; (search buffer list for matching buffer)
                (unless (equal e (assq (car e) tabbar--buffers)) ;; if not,...
                  ;; This is a new buffer, or a previously existing
                  ;; buffer that has been renamed, or moved to another
                  ;; group.  Update the tab set, and the display.
                  (tabbar-add-tab tabset (car e) t) ;; add to end of tabset
                  (tabbar-set-template tabset nil))
        ;;if tabset doesn't exist, make a new tabset with this buffer
              (tabbar-make-tabset g (car e))))))
      ;; Remove tabs for buffers not found in cache or moved to other
      ;; groups, and remove empty tabsets.
      (mapc 'tabbar-delete-tabset ;; delete each tabset named in following list:
            (tabbar-map-tabsets ;; apply following function to each tabset:
             #'(lambda (tabset)
                 (dolist (tab (tabbar-tabs tabset)) ;; for each tab in tabset
                   (let ((e (assq (tabbar-tab-value tab) bl))) ;; get buffer
                     (or (and e (memq tabset ;; skip if buffer exists and tabset is a member of groups-list for this buffer
                                      (mapcar 'tabbar-get-tabset
                                              (nth 2 e))))
                         (tabbar-delete-tab tab)))) ;; else remove tab from this set
                 ;; Return empty tab sets
                 (unless (tabbar-tabs tabset)
                   tabset)))) ;; return list of tabsets, replacing non-empties with nil
      ;; The new cache becomes the current one.
      (setq tabbar--buffers bl)))
  ;; Return the first group the current buffer belongs to.
  (car (nth 2 (assq (current-buffer) tabbar--buffers))))

;;; Tab bar callbacks
;;
(defvar tabbar--buffer-show-groups nil)

(defsubst tabbar-buffer-show-groups (flag)
  "Set display of tabs for groups of buffers to FLAG."
  (setq tabbar--buffer-show-groups flag
        ;; Redisplay the home button.
        tabbar-home-button-value nil))

(defun tabbar-buffer-tabs ()
  "Return the buffers to display on the tab bar, in a tab set."
  (let ((tabset (tabbar-get-tabset (tabbar-buffer-update-groups))))
    (tabbar-select-tab-value (current-buffer) tabset)
    (when tabbar--buffer-show-groups
      (setq tabset (tabbar-get-tabsets-tabset))
      (tabbar-select-tab-value (current-buffer) tabset))
    tabset))

(defun tabbar-buffer-button-label (name)
  "Return a label for button NAME.
That is a pair (ENABLED . DISABLED), where ENABLED and DISABLED are
respectively the appearance of the button when enabled and disabled.
They are propertized strings which could display images, as specified
by the variable `tabbar-button-label'.
When NAME is 'home, return a different ENABLED button if showing tabs
or groups.  Call the function `tabbar-button-label' otherwise."
  (let ((lab (tabbar-button-label name)))
    (when (eq name 'home)
      (let* ((btn tabbar-buffer-home-button)
             (on  nil)
             (off nil))
        ;; When `tabbar-buffer-home-button' does not provide a value,
        ;; default to the enabled value of `tabbar-home-button'.
        (setq on (get-text-property 0 'display (car lab)))
        
        (setq off (get-text-property 0 'display (car lab)))

        (setcar lab
                (if tabbar--buffer-show-groups
                    (propertize (or (caar btn) (car lab)) 'display on)
                  (propertize (or (cadr btn) (car lab)) 'display off)))
        ))
    lab))

(defun tabbar-buffer-tab-label (tab)
  "Return a label for TAB.
That is, a string used to represent it on the tab bar."
  (let ((label  (if tabbar--buffer-show-groups
                    (format "[%s]" (tabbar-tab-tabset tab))
                  (format "%s" (tabbar-tab-value tab)))))
    ;; Unless the tab bar auto scrolls to keep the selected tab
    ;; visible, shorten the tab label to keep as many tabs as possible
    ;; in the visible area of the tab bar.
    (if tabbar-auto-scroll-flag
        label
      (tabbar-shorten
       label (max 1 (/ (window-width)
                       (length (tabbar-view
                                (tabbar-current-tabset)))))))))

(defun tabbar-buffer-help-on-tab (tab)
  "Return the help string shown when mouse is onto TAB."
  (if tabbar--buffer-show-groups
      (let* ((tabset (tabbar-tab-tabset tab))
             (tab (tabbar-selected-tab tabset)))
        (format "mouse-1: switch to buffer %S in group [%s]"
                (buffer-name (tabbar-tab-value tab)) tabset))
    (format "mouse-1: switch to buffer %S\n\
mouse-2: pop to buffer, mouse-3: delete other windows"
            (buffer-name (tabbar-tab-value tab)))
    ))

(defun tabbar-buffer-select-tab (event tab)
  "On mouse EVENT, select TAB."
  (let ((mouse-button (event-basic-type event))
        (buffer (tabbar-tab-value tab)))
    (cond
     ((eq mouse-button 'mouse-2)
      (pop-to-buffer buffer t))
     ((eq mouse-button 'mouse-3)
      (delete-other-windows))
     (t
      (switch-to-buffer buffer)))
    ;; Don't show groups.
    (tabbar-buffer-show-groups nil)
    ))

(defun tabbar-buffer-click-on-home (event)
  "Handle a mouse click EVENT on the tab bar home button.
mouse-1, toggle the display of tabs for groups of buffers.
mouse-3, close the current buffer."
  (let ((mouse-button (event-basic-type event)))
    (cond
     ((eq mouse-button 'mouse-1)
      (tabbar-buffer-show-groups (not tabbar--buffer-show-groups)))
     ((eq mouse-button 'mouse-3)
      (kill-buffer nil))
     )))

(defun tabbar-buffer-help-on-home ()
  "Return the help string shown when mouse is onto the toggle button."
  (concat
   (if tabbar--buffer-show-groups
       "mouse-1: show buffers in selected group"
     "mouse-1: show groups of buffers")
   ", mouse-3: close current buffer"))

(defun tabbar-buffer-track-killed ()
  "Hook run just before actually killing a buffer.
In Tabbar mode, try to switch to a buffer in the current tab bar,
after the current buffer has been killed.  Try first the buffer in tab
after the current one, then the buffer in tab before.  On success, put
the sibling buffer in front of the buffer list, so it will be selected
first."
  (and (eq header-line-format tabbar-header-line-format)
       (eq tabbar-current-tabset-function 'tabbar-buffer-tabs)
       (eq (current-buffer) (window-buffer (selected-window)))
       (let ((bl (tabbar-tab-values (tabbar-current-tabset)))
             (b  (current-buffer))
             found sibling)
         (while (and bl (not found))
           (if (eq b (car bl))
               (setq found t)
             (setq sibling (car bl)))
           (setq bl (cdr bl)))
         (when (and (setq sibling (or (car bl) sibling))
                    (buffer-live-p sibling))
           ;; Move sibling buffer in front of the buffer list.
           (save-current-buffer
             (switch-to-buffer sibling))))))

;;; Tab bar buffer setup
;;
(defun tabbar-buffer-init ()
  "Initialize tab bar buffer data.
Run as `tabbar-init-hook'."
  (setq tabbar--buffers nil
        tabbar--buffer-show-groups nil
        tabbar-current-tabset-function 'tabbar-buffer-tabs
        tabbar-tab-label-function 'tabbar-buffer-tab-label
        tabbar-select-tab-function 'tabbar-buffer-select-tab
        tabbar-help-on-tab-function 'tabbar-buffer-help-on-tab
        tabbar-button-label-function 'tabbar-buffer-button-label
        tabbar-home-function 'tabbar-buffer-click-on-home
        tabbar-home-help-function 'tabbar-buffer-help-on-home
        )
  (add-hook 'kill-buffer-hook 'tabbar-buffer-track-killed))

(defun tabbar-buffer-quit ()
  "Quit tab bar buffer.
Run as `tabbar-quit-hook'."
  (setq tabbar--buffers nil
        tabbar--buffer-show-groups nil
        tabbar-current-tabset-function nil
        tabbar-tab-label-function nil
        tabbar-select-tab-function nil
        tabbar-help-on-tab-function nil
        tabbar-button-label-function nil
        tabbar-home-function nil
        tabbar-home-help-function nil
        )
  (remove-hook 'kill-buffer-hook 'tabbar-buffer-track-killed))

(add-hook 'tabbar-init-hook 'tabbar-buffer-init)
(add-hook 'tabbar-quit-hook 'tabbar-buffer-quit)

(setq tabbar-use-images nil)
(setq tabbar-ruler-global-tabbar t) ; If you want tabbar
(setq tabbar-ruler-global-ruler t) ; if you want a global ruler
(setq tabbar-ruler-popup-menu nil) ; If you want a popup menu.
(setq tabbar-ruler-popup-toolbar nil) ; If you want a popup toolbar
(setq tabbar-ruler-popup-scrollbar nil) ; If you want to only show the
                                      ; scroll bar when your mouse is moving.

; (eval-when-compile
;   (require 'cl))
; (require 'easymenu)


; (defun tabbar-popup-menu ()
;   "Keymap for pop-up menu.  Emacs only."
;   `(,(format "%s" (nth 0 tabbar-last-tab))
;     ["Close" tabbar-popup-close]
;     ["Close all BUT this" tabbar-popup-close-but]
;     "--"
;     ["Save" tabbar-popup-save]
;     ["Save As" tabbar-popup-save-as]
;     "--"
;     ["Rename File" tabbar-popup-rename
;      :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab))
;                   (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))]
;     ["Delete File" tabbar-popup-delete
;      :active (and (buffer-file-name (tabbar-tab-value tabbar-last-tab))
;                   (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab))))]
;     "--"
;     ["Gzip File" tabbar-popup-gz
;      :active (and (executable-find "gzip")
;                   (buffer-file-name (tabbar-tab-value tabbar-last-tab))
;                   (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
;                   (not (string-match "\\.gz\\(?:~\\|\\.~[0-9]+~\\)?\\'" (buffer-file-name (tabbar-tab-value tabbar-last-tab)))))]
;     ["Bzip File" tabbar-popup-bz2
;      :active (and (executable-find "bzip2")
;                   (buffer-file-name (tabbar-tab-value tabbar-last-tab))
;                   (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
;                   (not (string-match "\\.bz2\\(?:~\\|\\.~[0-9]+~\\)?\\'" (buffer-file-name (tabbar-tab-value tabbar-last-tab)))))]
;     ["Decompress File" tabbar-popup-decompress
;      :active (and
;               (buffer-file-name (tabbar-tab-value tabbar-last-tab))
;               (file-exists-p (buffer-file-name (tabbar-tab-value tabbar-last-tab)))
;               (string-match "\\(?:\\.\\(?:Z\\|gz\\|bz2\\|tbz2?\\|tgz\\|svgz\\|sifz\\|xz\\|dz\\)\\)\\(\\(?:~\\|\\.~[0-9]+~\\)?\\)\\'"
;                             (buffer-file-name (tabbar-tab-value tabbar-last-tab))))
;      ]
;     ;;    "--"
;     ;;    ["Print" tabbar-popup-print]
;     ))

; (defun tabbar-popup-print ()
;   "Print Buffer"
;   (interactive))

; (defun tabbar-popup-close ()
;   "Tab-bar pop up close"
;   (interactive)
;   (funcall tabbar-close-tab-function tabbar-last-tab))

; (defun tabbar-popup-close-but ()
;   "Tab-bar close all BUT this buffer"
;   (interactive)
;   (let ((cur (symbol-value (funcall tabbar-current-tabset-function))))
;     (mapc (lambda(tab)
;             (unless (eq tab tabbar-last-tab)
;               (funcall tabbar-close-tab-function tab)))
;           cur)))

; (defun tabbar-popup-save-as ()
;   "Tab-bar save as"
;   (interactive)
;   (let* ((buf (tabbar-tab-value tabbar-last-tab)))
;     (save-excursion
;       (set-buffer buf)
;       (call-interactively 'write-file))))

; (defun tabbar-popup-rename ()
;   "Tab-bar rename"
;   (interactive)
;   (let* ((buf (tabbar-tab-value tabbar-last-tab))
;          (fn (buffer-file-name buf)))
;     (save-excursion
;       (set-buffer buf)
;       (when (call-interactively 'write-file)
;         (if (string= fn (buffer-file-name (current-buffer)))
;             (error "Buffer has same name.  Just saved instead.")
;           (delete-file fn))))))

; (defun tabbar-popup-delete ()
;   "Tab-bar delete file"
;   (interactive)
;   (let* ((buf (tabbar-tab-value tabbar-last-tab))
;          (fn (buffer-file-name buf)))
;     (when (yes-or-no-p (format "Are you sure you want to delete %s?" buf))
;       (save-excursion
;         (set-buffer buf)
;         (set-buffer-modified-p nil)
;         (kill-buffer (current-buffer))
;         (delete-file fn)))))

; (defun tabbar-popup-remove-compression-ext (file-name &optional new-compression)
;   "Removes compression extension, and possibly adds a new extension"
;   (let ((ret file-name))
;     (when (string-match "\\(\\(?:\\.\\(?:Z\\|gz\\|bz2\\|tbz2?\\|tgz\\|svgz\\|sifz\\|xz\\|dz\\)\\)?\\)\\(\\(?:~\\|\\.~[0-9]+~\\)?\\)\\'" ret)
;       (setq ret (replace-match (concat (or new-compression "") (match-string 2 ret)) t t ret)))
;     (symbol-value 'ret)))

; (defun tabbar-popup-gz (&optional ext err)
;   "Gzips the file"
;   (interactive)
;   (let* ((buf (tabbar-tab-value tabbar-last-tab))
;          (fn (buffer-file-name buf))
;          (nfn (tabbar-popup-remove-compression-ext fn (or ext ".gz"))))
;     (if (string= fn nfn)
;         (error "Already has that compression!")
;       (save-excursion
;         (set-buffer buf)
;         (write-file nfn)
;         (if (not (file-exists-p nfn))
;             (error "%s" (or err "Could not gzip file!"))
;           (when (file-exists-p fn)
;             (delete-file fn)))))))

; (defun tabbar-popup-bz2 ()
;   "Bzip file"
;   (interactive)
;   (tabbar-popup-gz ".bz2" "Could not bzip the file!"))

; (defun tabbar-popup-decompress ()
;   "Decompress file"
;   (interactive)
;   (tabbar-popup-gz "" "Could not decompress the file!"))

; (defun tabbar-context-menu ()
;   "Pop up a context menu."
;   (interactive)
;   (popup-menu (tabbar-popup-menu)))


(defun tabbar-hex-color (color)
  "Gets the hexadecimal value of a color"
  (let ((ret color))
    (cond
     ((not (eq (type-of color) 'string))
      (setq ret "None"))
     ((string= "#" (substring color 0 1))
      (setq ret (upcase ret)))
     ((color-defined-p color)
      (setq ret (concat "#"
                        (mapconcat
                         (lambda(val)
                           (format "%02X" (* val 255)))
                         (color-name-to-rgb color) ""))))
     (t (setq ret "None")))
    (symbol-value 'ret)))

(defcustom tabbar-ruler-swap-faces nil
  "Swap the selected / unselected tab colors"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-invert-deselected t
  "Invert deselected tabs"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-modified-symbol nil
  "Add modified symbol in addition to changing the face."
  :type 'boolean
  :type 'tabbar-ruler)

;;;###autoload
(defun tabbar-install-faces (&optional frame)
  "Installs faces for a frame."
  (interactive)
  (copy-face 'mode-line 'tabbar-default frame)
  (if tabbar-ruler-swap-faces
      (progn
        (copy-face 'default 'tabbar-selected frame)
        (copy-face 'shadow 'tabbar-unselected frame)
        (if tabbar-ruler-invert-deselected
            (progn
              (copy-face 'tabbar-selected 'tabbar-unselected)
              (set-face-attribute 'tabbar-selected frame
                                  :box nil)
              (invert-face 'tabbar-selected))
          (set-face-attribute 'tabbar-selected frame
                              :inherit 'mode-line-buffer-id
                              :background (face-attribute 'mode-line-inactive :background)
                              :box nil))
        (copy-face 'mode-line-buffer-id 'tabbar-unselected-highlight frame)
        (copy-face 'mode-line-inactive 'tabbar-selected-highlight frame))
  (copy-face 'default 'tabbar-selected frame)
  (copy-face 'shadow 'tabbar-unselected frame)
  
  (if tabbar-ruler-invert-deselected
      (progn
        (copy-face 'tabbar-selected 'tabbar-unselected)
        (set-face-attribute 'tabbar-unselected frame
                            :box nil)
        (invert-face 'tabbar-unselected))
    (set-face-attribute 'tabbar-unselected frame
                        :inherit 'mode-line-buffer-id
                        :background (face-attribute 'mode-line-inactive :background)
                        :box nil))
  
  
  (copy-face 'mode-line-buffer-id 'tabbar-selected-highlight frame)
  (copy-face 'mode-line-inactive 'tabbar-unselected-highlight frame))
  
  (set-face-attribute 'tabbar-separator frame
                      :inherit 'tabbar-default
                      :box nil)
  
  (set-face-attribute 'tabbar-button frame
                      :inherit 'tabbar-default
                      :box nil))

(add-hook 'after-make-frame-functions 'tabbar-install-faces)
(add-hook 'emacs-startup-hook 'tabbar-install-faces)
(tabbar-install-faces)


;; Taken from powerline

(defun tabbar-create-or-get-tabbar-cache ()
  "Return a frame-local hash table that acts as a memoization
cache for tabbar. Create one if the frame doesn't have one
yet."
  (or (frame-parameter nil 'tabbar-cache)
      (let ((table (make-hash-table :test 'equal)))
        ;; Store it as a frame-local variable
        (modify-frame-parameters nil `((tabbar-cache . ,table)))
        table)))

;; from memoize.el @ http://nullprogram.com/blog/2010/07/26/
(defun tabbar-memoize (func)
  "Memoize FUNC.
If argument is a symbol then install the tabbar-memoized function over
the original function.  Use frame-local memoization."
  (typecase func
    (symbol (fset func (tabbar-memoize-wrap-frame-local (symbol-function func))) func)
    (function (tabbar-memoize-wrap-frame-local func))))

(defun tabbar-memoize-wrap-frame-local (func)
  "Return the tabbar-memoized version of FUNC.  The memoization cache is
frame-local."
  (let ((cache-sym (gensym))
        (val-sym (gensym))
        (args-sym (gensym)))
    `(lambda (&rest ,args-sym)
       ,(concat (documentation func) "\n(tabbar-memoized function)")
       (let* ((,cache-sym (tabbar-create-or-get-tabbar-cache))
              (,val-sym (gethash ,args-sym ,cache-sym)))
         (if ,val-sym
             ,val-sym
           (puthash ,args-sym (apply ,func ,args-sym) ,cache-sym))))))

(defun tabbar-ruler-tab-separator-image (face1 face2 &optional face3 next-on-top slope height)
  "Creates a Tabbar Ruler Separator Image.
FACE1 is the face to the left
FACE2 is the face to the right
FACE3 is the background face (optional)

When FACE1 is nil and FACE2 is present this function creates the
first tab image.

When FACE2 is nil and FACE1 is present this function creates the
last tab image.

When FACE1 = FACE2, this creates a non-selected separator

When FACE1 does not equal FACE2, this creates a selected separator
"
  (let* ((h (or height (max 20 (frame-char-height))))
         (m (or slope 2))
         (w (/ h m))
         (i h)
         x1 x2 e1 e2 e3 e4
         (color1 (if face1 (tabbar-hex-color (face-attribute face1 :background)) "None"))
         (color1-border (if face1 (tabbar-hex-color (face-attribute face1 :foreground)) "None"))
         (color2 (if face2 (tabbar-hex-color (face-attribute face2 :background)) "None"))
         (color2-border (if face2 (tabbar-hex-color (face-attribute face2 :foreground)) "None"))
         (color-background (if face3 (tabbar-hex-color (face-attribute face3 :background)) (tabbar-hex-color (face-attribute 'default :background))))
         (ret "/* XPM */\nstatic char * "))
    (cond
     ((string= color1 color2)
      (setq ret (concat ret "tabbar_ruler_default_separator")))
     ((not face2)
      (setq ret (concat ret "tabbar_ruler_separator_end")))
     (t
      (setq ret (concat ret "tabbar_ruler_separator_end_sel"))))
    (setq ret (concat ret "[] = {\n"))
    (setq ret (format "%s\"%s %s 5 1\",\n" ret (round w) (- h 1)))
    ;; Now do colors
    (setq ret (format "%s\"  c %s\",\n" ret color-background))
    (setq ret (format "%s\". c  %s\",\n" ret color1))
    (setq ret (format "%s\"> c %s\",\n" ret color1-border))
    (setq ret (format "%s\"= c %s\",\n" ret color2))
    (setq ret (format "%s\"+ c %s\"" ret color2-border))
    (while (>= i 1)
      (setq x1 (round (+ 1 (/ (- i 1) m))))
      (setq x2 (round (/ (- (+ h m) i) m)))
      (cond
       ((and face2 (>= x1 x2))
        (if (= x2 1)
            (setq e1 "")
          (setq e1 (make-string (- x2 1) (if (not face1)?  ?.))))
        (if (= x1 x2)
            (progn
              (if (or next-on-top (not face1))
                  (setq e2 "+")
                (setq e2 ">"))
              (setq e3 "")
              (setq e4 ""))
          (if face1
              (setq e2 ">")
            (setq e2 ""))
          (setq e3 (make-string (- x1 x2) ? ))
          (setq e4 "+"))
        (if (= x1 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x1) ?=))))
       ((or (and face1 (not face2))
            (and (< x1 x2) (not (or next-on-top (not face1)))))
        (if (= x2 1)
            (setq e1 "")
          (setq e1 (make-string (- x2 1) ?.)))
        (setq e2 ">")
        (setq e3 "")
        (setq e4 "")
        (if (= x2 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x2) (if (not face2) ?  ?=)))))
       ((and (< x1 x2) (or next-on-top (not face1)))
        (if (= x1 1)
            (setq e1 "")
          (setq e1 (make-string (- x1 1) (if (not face1) ?  ?.))))
        (setq e2 "+")
        (setq e3 "")
        (setq e4 "")
        (if (= x1 w)
            (setq e5 "")
          (setq e5 (make-string (- (round w) x1) ?=)))))
      (setq ret (format "%s,\n\"%s%s%s%s%s\"" ret e1 e2 e3 e4 e5))
      (setq i (- i 1)))
    (setq ret (format "%s};" ret))
    (symbol-value 'ret)))


(defun* tabbar-ruler-image (&key type disabled color)
  "Returns the scroll-images"
  (let ((clr2 (if disabled (tabbar-hex-color (face-attribute 'mode-line-inactive :background))
                (tabbar-hex-color (face-attribute 'mode-line :background))))
        (clr (or color (if disabled (tabbar-hex-color (face-attribute 'mode-line-inactive :foreground))
                         (tabbar-hex-color (face-attribute 'mode-line :foreground))))))
    (if (eq type 'close)
        (format "/* XPM */
        static char * close_tab_xpm[] = {
        \"14 11 3 1\",
        \"       c None\",
        \".      c %s\",
        \"+      c %s\",
        \"     .....    \",
        \"    .......   \",
        \"   .........  \",
        \"  ... ... ... \",
        \"  .... . .... \",
        \"  ..... ..... \",
        \"  .... . .... \",
        \"  ... ... ... \",
        \"   .........  \",
        \"    .......   \",
        \"     .....    \"};" clr clr2)
      
      (format
       "/* XPM */
static char * scroll_%s_%s_xpm[] = {
\"17 17 2 1\",
\"       c None\",
\".      c %s\",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
%s
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \",
\"                 \"};
" (symbol-name type)
(if disabled "disabled" "enabled")
clr
(cond
 ((eq 'right type)
  "\"                 \",
\"     ..          \",
\"     ....        \",
\"     ......      \",
\"     .....       \",
\"     ...         \",
"
  )
 ((eq 'left type)
  "\"                 \",
\"          ..     \",
\"        ....     \",
\"      ......     \",
\"       .....     \",
\"         ...     \","
  )
 ((eq 'up type)
  "\"        .        \",
\"       ..        \",
\"       ...       \",
\"      ....       \",
\"      .....      \",
\"      .....      \",")
 ((eq 'down type)
  "\"      .....      \",
\"      .....      \",
\"      ....       \",
\"       ...       \",
\"       ..        \",
\"        .        \","))))))


(defconst tabbar-home-button-enabled-image
  `((:type xpm :data ,(tabbar-ruler-image :type 'down)))
  "Default image for the enabled home button.")

(defconst tabbar-home-button-disabled-image
  `((:type xpm :data ,(tabbar-ruler-image :type 'up)))
  "Default image for the disabled home button")


(defconst tabbar-home-button
  (cons (cons "[o]" tabbar-home-button-enabled-image)
            (cons "[x]" tabbar-home-button-disabled-image)))

(setq tabbar-buffer-home-button
      (cons (cons "[+]" tabbar-home-button-enabled-image)
            (cons "[-]" tabbar-home-button-disabled-image)))

(setq tabbar-scroll-left-button-enabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'left))))

(setq tabbar-scroll-left-button-disabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'left :disabled t))))

(setq tabbar-scroll-left-button
      (cons (cons " <" tabbar-scroll-left-button-enabled-image)
            (cons " =" tabbar-scroll-left-button-disabled-image)))

(setq tabbar-scroll-right-button-enabled-image
      `((:type xpm :data ,(tabbar-ruler-image :type 'right))))

(setq tabbar-scroll-right-button-disabled-image
      `((:type xpm :data  ,(tabbar-ruler-image :type 'right :disabled t))))

(setq tabbar-scroll-right-button
      (cons (cons " >" tabbar-scroll-right-button-enabled-image)
            (cons " =" tabbar-scroll-right-button-disabled-image)))

(defsubst tabbar-normalize-image (image &optional margin nomask)
  "Make IMAGE centered and transparent.
If optional MARGIN is non-nil, it must be a number of pixels to add as
an extra margin around the image.  If optional NOMASK is non-nil, no mask
property is included."
  (let ((plist (cdr image)))
    (or (plist-get plist :ascent)
        (setq plist (plist-put plist :ascent 'center)))
    (or (plist-get plist :mask)
        (unless nomask
          (setq plist (plist-put plist :mask '(heuristic t)))))
    (or (not (natnump margin))
        (plist-get plist :margin)
        (plist-put plist :margin margin))
    (setcdr image plist))
  image)

(defvar tabbar-close-tab-function nil
  "Function to call to close a tabbar tab.  Passed a single argument, the tab
construct to be closed.")

(defvar tabbar-new-tab-function nil
  "Function to call to create a new buffer in tabbar-mode.  Optional single
argument is the MODE for the new buffer.")

;; for buffer tabs, use the usual command to close/kill a buffer
(defun tabbar-buffer-close-tab (tab)
  (let ((buffer (tabbar-tab-value tab)))
    (with-current-buffer buffer
      (kill-buffer buffer))))

(setq tabbar-close-tab-function 'tabbar-buffer-close-tab)

; (defvar tabbar-last-tab nil)

(defsubst tabbar-click-on-tab (tab &optional type action)
  "Handle a mouse click event on tab TAB.
Call `tabbar-select-tab-function' with the received, or simulated
mouse click event, and TAB.
Optional argument TYPE is a mouse click event type (see the function
`tabbar-make-mouse-event' for details)."
  (let* ((mouse-event (tabbar-make-mouse-event type))
         (mouse-button (event-basic-type mouse-event)))
    (if  (eq mouse-button 'mouse-3)
        (progn
          (setq tabbar-last-tab tab)
          (tabbar-context-menu))
      (if (eq action 'close-tab)
          (when (and (eq mouse-button 'mouse-1) tabbar-close-tab-function)
            (funcall tabbar-close-tab-function tab))
        (when tabbar-select-tab-function
          (funcall tabbar-select-tab-function
                   (tabbar-make-mouse-event type) tab)
          (tabbar-display-update))))))

; (defun tabbar-reset ()
;   "Reset memoized functions."
;   (interactive)
;   (tabbar-memoize 'tabbar-ruler-tab-separator-image)
;   (tabbar-memoize 'tabbar-make-tab-keymap)
;   (tabbar-memoize 'tabbar-ruler-image))
; (tabbar-reset)

; (defsubst tabbar-drag-p (event)
;   "Return non-nil if EVENT is a mouse drag event."
;   (memq 'drag (event-modifiers event)))

(defun tabbar-select-tab-callback (event)
  "Handle a mouse EVENT on a tab.
Pass mouse click events on a tab to `tabbar-click-on-tab'."
  (interactive "@e")
  (cond 
    ((tabbar-click-p event)
      (let ((target (posn-string (event-start event))))
        (tabbar-click-on-tab
          (get-text-property (cdr target) 'tabbar-tab (car target))
          event
          (get-text-property (cdr target) 'tabbar-action (car target)))))
    ((tabbar-drag-p event)
      (let ((start-target (posn-string (event-start event)))
            (end-target (posn-string (event-end event))))
        (tabbar-drag-tab
          (get-text-property (cdr start-target) 'tabbar-tab (car start-target))
          (get-text-property (cdr end-target) 'tabbar-tab (car end-target))
          event)))
  ))

; (defun tabbar-drag-tab (dragged-tab dropped-tab event)
;   "Handle DRAGGED-TAB dragged-and-dropped onto DROPPED-TAB.
;    Include full mouse EVENT from drag-and-drop action."
;   (let ((start-tabset (tabbar-tab-tabset dragged-tab)))
;     (when (and (eq start-tabset (tabbar-tab-tabset dropped-tab))
;            (not (eq dragged-tab dropped-tab)))
;       (let* ((tabs (tabbar-tabs start-tabset))
;          (drop-tail-length (length (memq dropped-tab tabs)))
;          (drag-tail-length (length (memq dragged-tab tabs)))
;          (dragdrop-pair (list dragged-tab dropped-tab))
;          new-tablist)
;     (when (> drag-tail-length drop-tail-length)
;       (setq dragdrop-pair (reverse dragdrop-pair)))
;     (dolist (thistab (reverse tabs))
;       ;; build list of tabs.  When we hit dragged-tab, don't append it.
;       ;; When we hit dropped-tab, append dragdrop-pair
;       (cond
;         ((eq thistab dragged-tab))
;         ((eq thistab dropped-tab)
;          (setq new-tablist (append dragdrop-pair new-tablist)))
;         (t (add-to-list 'new-tablist thistab))
;       ))
;     (set start-tabset new-tablist)
;     ;; (setq tabbar-window-cache nil)  ;; didn't help
;     (tabbar-set-template start-tabset nil)
;     ;; open the dragged tab
;     (funcall tabbar-select-tab-function
;              (tabbar-make-mouse-event event) dragged-tab)
;     (tabbar-display-update)
;     ))))

(defsubst tabbar-line-tab (tab &optional not-last sel)
  "Return the display representation of tab TAB.
That is, a propertized string used as an `header-line-format' template
element.
Call `tabbar-tab-label-function' to obtain a label for TAB."
  (let* ( (selected-p (tabbar-selected-p tab (tabbar-current-tabset)))
          (modified-p (buffer-modified-p (tabbar-tab-value tab)))
          (close-button-image (tabbar-find-image 
                               `((:type xpm :data ,(tabbar-ruler-image :type 'close :disabled (not modified-p)
                                                                       :color (if (eq tab sel)
                                                                                  (face-attribute 'default :foreground)
                                                                                "gray10"))))))
          (keymap (tabbar-make-tab-keymap tab))
          (separator-image (if tabbar-ruler-fancy-tab-separator
                               (tabbar-find-image
                                `((:type xpm :data
                                         ,(tabbar-ruler-tab-separator-image
                                           (if (eq tab sel)
                                               'tabbar-selected
                                             'tabbar-unselected)
                                           (if not-last
                                               (if (eq (car not-last) sel)
                                                   'tabbar-selected
                                                 'tabbar-unselected) nil)
                                           nil
                                           (if (and not-last
                                                    (eq (car not-last) sel))
                                               t nil)))))
                             nil))
          (face (if selected-p
                    (if modified-p
                        'tabbar-selected-modified
                      'tabbar-selected)
                  (if modified-p
                      'tabbar-unselected-modified
                    'tabbar-unselected))))
    (concat
     (propertize " " 'face face
                 'tabbar-tab tab
                 'local-map keymap
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand)
     (propertize 
      (if tabbar-tab-label-function
          (funcall tabbar-tab-label-function tab)
        tab)
      'tabbar-tab tab
      'local-map keymap
      'help-echo 'tabbar-help-on-tab
      'mouse-face 'tabbar-highlight
      'face face
      'pointer 'hand)
     (propertize (if (and modified-p tabbar-ruler-modified-symbol)
                     (with-temp-buffer
                       (condition-case err
                           (ucs-insert "207A")
                         (error
                          (condition-case err
                              (insert-char 0x207A)
                            (error (insert "*")))))
                       (insert " ")
                       (buffer-substring (point-min) (point-max))) " ")
                 'face face
                 'tabbar-tab tab
                 'local-map keymap
                 'help-echo 'tabbar-help-on-tab
                 'face face
                 'pointer 'hand)
     (if tabbar-ruler-fancy-close-image
         (propertize (with-temp-buffer
                       (condition-case err
                           (ucs-insert "00D7")
                         (error
                          (condition-case err
                              (insert-char #x00D7)
                            (error (insert "x")))))
                       (buffer-string))
                     'display (tabbar-normalize-image close-button-image 0)
                     'face face
                     'pointer 'hand
                     'tabbar-tab tab
                     'local-map keymap
                     'tabbar-action 'close-tab)
       (propertize
        (with-temp-buffer
          (condition-case err
              (ucs-insert "00D7")
            (error (condition-case err
                       (insert-char #x00D7)
                     (error (insert "x")))))
          (insert " ")
          (buffer-string))
        'face face
        'pointer 'hand
        'tabbar-tab tab
        'local-map keymap
        'tabbar-action 'close-tab))
     (if tabbar-ruler-fancy-tab-separator
         (propertize "|"
                     'display (tabbar-normalize-image separator-image))
       tabbar-separator-value))))

(defsubst tabbar-line-format (tabset)
  "Return the `header-line-format' value to display TABSET."
  (let* ((sel (tabbar-selected-tab tabset))
         (tabs (tabbar-view tabset))
         (padcolor (tabbar-background-color))
         atsel elts
         (separator-image (if tabbar-ruler-fancy-tab-separator
                              (tabbar-find-image
                               `((:type xpm :data
                                        ,(tabbar-ruler-tab-separator-image
                                          nil
                                          (if (eq (car tabs) sel)
                                              'tabbar-selected
                                            'tabbar-unselected)))))
                            nil)))
    ;; Initialize buttons and separator values.
    (or tabbar-separator-value
        (tabbar-line-separator))
    (or tabbar-home-button-value
        (tabbar-line-button 'home))
    (or tabbar-scroll-left-button-value
        (tabbar-line-button 'scroll-left))
    (or tabbar-scroll-right-button-value
        (tabbar-line-button 'scroll-right))
    ;; Track the selected tab to ensure it is always visible.
    (when tabbar--track-selected
      (while (not (memq sel tabs))
        (tabbar-scroll tabset -1)
        (setq tabs (tabbar-view tabset)))
      (while (and tabs (not atsel))
        (if tabbar-ruler-fancy-tab-separator
            (setq elts  (cons (tabbar-line-tab (car tabs) (cdr tabs) sel) elts)
                  atsel (eq (car tabs) sel)
                  tabs  (cdr tabs))
          (setq elts  (cons (tabbar-line-tab (car tabs)) elts)
                atsel (eq (car tabs) sel)
                tabs  (cdr tabs))))
      (setq elts (nreverse elts))
      ;; At this point the selected tab is the last elt in ELTS.
      ;; Scroll TABSET and ELTS until the selected tab becomes
      ;; visible.
      (with-temp-buffer
        (let ((truncate-partial-width-windows nil)
              (inhibit-modification-hooks t)
              deactivate-mark ;; Prevent deactivation of the mark!
              start)
          (setq truncate-lines nil
                buffer-undo-list t)
          (apply 'insert (tabbar-line-buttons tabset))
          (setq start (point))
          (while (and (cdr elts) ;; Always show the selected tab!
                      (progn
                        (delete-region start (point-max))
                        (goto-char (point-max))
                        (apply 'insert elts)
                        (goto-char (point-min))
                        (> (vertical-motion 1) 0)))
            (tabbar-scroll tabset 1)
            (setq elts (cdr elts)))))
      (setq elts (nreverse elts))
      (setq tabbar--track-selected nil))
    ;; Format remaining tabs.
    (while tabs
      (if tabbar-ruler-fancy-tab-separator
          (setq elts (cons (tabbar-line-tab (car tabs) (cdr tabs) sel) elts)
                tabs (cdr tabs))
        (setq elts (cons (tabbar-line-tab (car tabs)) elts)
              tabs (cdr tabs))))
    ;; Cache and return the new tab bar.
    (if tabbar-ruler-fancy-tab-separator
        (tabbar-set-template
         tabset
         (list (tabbar-line-buttons tabset)
               (propertize "|"
                           'display (tabbar-normalize-image separator-image))
               (nreverse elts)
               (propertize "%-"
                           'face (list :background padcolor
                                       :foreground padcolor)
                           'pointer 'arrow)))
      (tabbar-set-template
       tabset
       (list (tabbar-line-buttons tabset)
             (nreverse elts)
             (propertize "%-"
                         'face (list :background padcolor
                                     :foreground padcolor)
                         'pointer 'arrow))))))

(defface tabbar-selected-modified
  '((t
     :inherit tabbar-selected
     :foreground "DarkOrange3"
     :weight bold))
   "Face used for selected tabs."
  :group 'tabbar)

(defface tabbar-unselected-modified
  '((t
     :inherit tabbar-unselected
     :foreground "DarkOrange3"
     :weight bold))
   "Face used for unselected tabs."
  :group 'tabbar)

(defface tabbar-key-binding '((t
                               :foreground "white"))
  "Face for unselected, highlighted tabs."
  :group 'tabbar)

;; Hooks based on yswzing's hooks, but modified for this function state.
;; called each time the modification state of the buffer changed
(defun tabbar-ruler-modification-state-change ()
  (tabbar-set-template tabbar-current-tabset nil)
  (tabbar-display-update))

;; first-change-hook is called BEFORE the change is made
(defun tabbar-ruler-on-buffer-modification ()
  (set-buffer-modified-p t)
  (tabbar-ruler-modification-state-change))
(add-hook 'after-save-hook 'tabbar-ruler-modification-state-change)

(defcustom tabbar-ruler-global-tabbar 't
  "Should tabbar-ruler have a global tabbar?"
  :type 'boolean
  :group 'tabbar-ruler)
(defcustom tabbar-ruler-global-ruler nil
  "Should tabbar-ruler have a global ruler?"
  :type 'boolean
  :group 'tabbar-ruler)
; (defcustom tabbar-ruler-popup-menu nil
;   "Should tabbar-ruler have a popup menu.  As mouse moves toward top of window, the menu pops up."
;   :type 'boolean
;   :group 'tabbar-ruler)
; (defcustom tabbar-ruler-popup-toolbar nil
;   "Should tabbar-ruler have a popup toolbar.  As mouse moves toward top of window, the toolbar pops up."
;   :type 'boolean
;   :group 'tabbar-ruler)

; (defcustom tabbar-ruler-popup-scrollbar nil
;   "Should tabbas-ruler have popup scrollbar.  As mouse moves, the scroll-bar pops up.  Otherwise the sroll-bar is turned off."
;   :type 'boolean
;   :group 'tabbar-ruler)

; (defcustom tabbar-ruler-popup-menu-min-y 5 ;
;   "Minimum number of pixels from the top before a menu/toolbar pops up."
;   :type 'integer
;   :group 'tabbar-ruler)
; (defcustom tabbar-ruler-popup-menu-min-y-leave 50
;   "Minimum number of pixels form the top before a menu/toolbar disappears."
;   :type 'integer
;   :group 'tabbar-ruler)
(defcustom tabbar-ruler-do-not-switch-on-ruler-when-tabbar-is-on-y 75
  "Minimum number of pixels to switch on ruler when tabbar is on."
  :type 'integer          
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-excluded-buffers '("*Messages*" "*Completions*" "*ESS*")
  "Excluded buffers in tabbar."
  :type '(repeat (string :tag "Buffer Name"))
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-fight-igore-modes '(info-mode helm-mode package-menu-mode)
  "Exclude these mode when changing between tabbar and ruler."
  :type '(repeat (symbol :tag "Major Mode"))
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-fancy-tab-separator nil
  "Separate each tab with a fancy generated image"
  :type 'boolean
  :group 'tabbar-ruler)

(defcustom tabbar-ruler-fancy-close-image nil
  "Use an image for the close"
  :type 'boolean
  :group 'tabbar-ruler)

; (defcustom tabbar-ruler-movement-timer-delay 0.1
;   "Timer Delay for `tabbar-ruler-movement-timer'"
;   :type 'number
;   :group 'tabbar-ruler)

; (defvar tabbar-ruler-tabbar-off 't)
; (defvar tabbar-ruler-ruler-off 't)
; (set (make-variable-buffer-local 'tabbar-ruler-toolbar-off) nil)
; (set (make-variable-buffer-local 'tabbar-ruler-ruler-off) nil)

; (defvar tabbar-ruler-toolbar-off nil)
; (defvar tabbar-ruler-menu-off nil)
; (add-hook 'find-file-hook
;           (lambda()
;             (interactive)
;             (tabbar-ruler-tabbar-ruler-fight 't)))

(defcustom tabbar-ruler-ruler-display-commands
  '(ac-trigger-commands
    esn-upcase-char-self-insert
    esn-magic-$
    right-char
    left-char
    previous-line
    next-line
    backward-paragraph
    forward-paragraph
    cua-scroll-down
    cua-scroll-up
    cua-paste
    cua-paste-pop
    scroll-up
    scroll-down
    autopair-newline
    autopair-insert-opening
    autopair-skip-close-maybe
    autopair-backspace
    backward-delete-char-untabify
    delete-backward-char
    self-insert-command)
  "Ruler display commands."
  :group 'tabbar-ruler
  :type '(repeat symbol))

; (defun tabbar-ruler-tabbar-ruler-fight (&optional initialize)
;   "Defines the fighting behavior of the tabbar-ruler ruler and tabbar."
;   (condition-case error
;       (progn
;         (cond
;          ((minibufferp)
;           nil)
;          (tabbar-ruler-keep-tabbar
;           (setq tabbar-ruler-keep-tabbar nil)
;           nil)
;          ((and (save-match-data (string-match "^[*]Org Src " (buffer-name))))
;           nil)
;          ((member major-mode tabbar-ruler-fight-igore-modes)
;           nil)
;          ( (eq major-mode 'helm-mode)
;            nil)
;          ( (eq last-command 'mouse-drag-region)
;            (tabbar-ruler-mouse-movement))
;          ( (and tabbar-ruler-global-ruler tabbar-ruler-global-tabbar)
;            (cond
;             ( (memq last-command tabbar-ruler-ruler-display-commands)
;               (when tabbar-ruler-popup-scrollbar
;                 (scroll-bar-mode -1))
;               (when tabbar-ruler-ruler-off
;                 (ruler-mode 1)
;                 (setq tabbar-ruler-ruler-off nil))
;               (unless tabbar-ruler-tabbar-off
;                 (tabbar-mode -1)
;                 (setq tabbar-ruler-tabbar-off 't))
;               (when tabbar-ruler-popup-menu
;                 (unless tabbar-ruler-menu-off
;                   (unless (eq system-type 'darwin)
;         (menu-bar-mode -1))
;                   (setq tabbar-ruler-menu-off 't)))
;               (when tabbar-ruler-popup-toolbar
;                 (unless (eq system-type 'darwin)
;                   (unless tabbar-ruler-toolbar-off
;                     (tool-bar-mode -1)
;                     (setq tabbar-ruler-toolbar-off 't)))))
;             ( (save-match-data (string-match "\\(mouse\\|ignore\\|window\\|frame\\)" (format "%s" last-command)))
;               (when nil ;; Took this out;  Afterward it works much better...
;                 (unless tabbar-ruler-ruler-off
;                   (ruler-mode -1)
;                   (setq tabbar-ruler-ruler-off 't))
;                 (when tabbar-ruler-tabbar-off
;                   (tabbar-mode 1)
;                   (setq tabbar-ruler-tabbar-off nil))))
;             ( 't
;               (when (or initialize (and tabbar-ruler-ruler-off tabbar-ruler-tabbar-off))
;                 (when tabbar-ruler-popup-scrollbar
;                   (scroll-bar-mode -1))
;                 (when tabbar-ruler-ruler-off
;                   (ruler-mode 1)
;                   (setq tabbar-ruler-ruler-off nil))
;                 (unless tabbar-ruler-tabbar-off
;                   (tabbar-mode -1)
;                   (setq tabbar-ruler-tabbar-off 't))))))
;          ( tabbar-ruler-global-ruler
;            (when tabbar-ruler-ruler-off
;              (ruler-mode 1)
;              (setq tabbar-ruler-ruler-off nil)))
;          ( tabbar-ruler-global-tabbar
;            (when tabbar-ruler-tabbar-off
;              (tabbar-mode 1)
;              (setq tabbar-ruler-tabbar-off nil)))))
;     (error
;      (message "Error in post-command-hook for Ruler/Tabbar: %s" (error-message-string error)))))

; (add-hook 'post-command-hook 'tabbar-ruler-tabbar-ruler-fight)
; (defvar tabbar-ruler-movement-timer nil)
; (defvar tabbar-ruler-movement-x nil)
; (defvar tabbar-ruler-movement-y nil)

; (defun tabbar-ruler-mouse-movement ()
;   "Mouse Movement function"
;   (interactive)
;   (when tabbar-ruler-movement-timer
;     (cancel-timer tabbar-ruler-movement-timer))  
;   (let* ((y-pos (cddr (mouse-pixel-position)))
;          (x-pos (cadr (mouse-pixel-position))))
;     (unless y-pos
;       (setq y-pos tabbar-ruler-movement-y))
;     (unless x-pos
;       (setq x-pos tabbar-ruler-movement-x))
;     (when (or (not tabbar-ruler-movement-x) (not tabbar-ruler-movement-y)
;               (and tabbar-ruler-movement-x tabbar-ruler-movement-y
;                    (not
;                     (and
;                      (= tabbar-ruler-movement-x x-pos)
;                      (= tabbar-ruler-movement-y y-pos)))))
;       (when (and x-pos y-pos)
;         (when tabbar-ruler-popup-scrollbar
;           (scroll-bar-mode 1))
;         (setq tabbar-ruler-movement-x x-pos)
;         (setq tabbar-ruler-movement-y y-pos)
;         (unless tabbar-ruler-ruler-off
;           (ruler-mode -1)
;           (setq tabbar-ruler-ruler-off 't))
;         (when tabbar-ruler-tabbar-off
;           (tabbar-mode 1)
;           (setq tabbar-ruler-tabbar-off nil))
;         (if (>= (if (or tabbar-ruler-menu-off tabbar-ruler-toolbar-off)
;                     tabbar-ruler-popup-menu-min-y
;                   tabbar-ruler-popup-menu-min-y-leave) y-pos)
;             (progn
;               (when tabbar-ruler-popup-menu
;                 (when tabbar-ruler-menu-off
;                   (unless (eq system-type 'darwin)
;         (menu-bar-mode 1))
;                   (setq tabbar-ruler-menu-off nil)))
;               (when tabbar-ruler-popup-toolbar
;                 (unless (eq system-type 'darwin)
;                   (when tabbar-ruler-toolbar-off
;                     (tool-bar-mode 1)
;                     (setq tabbar-ruler-toolbar-off nil)))))
;           (when tabbar-ruler-popup-menu
;             (unless tabbar-ruler-menu-off
;               (unless (eq system-type 'darwin)
;     (menu-bar-mode -1))
;               (setq tabbar-ruler-menu-off 't)))
;           (when tabbar-ruler-popup-toolbar
;             (unless (eq system-type 'darwin)
;               (unless tabbar-ruler-toolbar-off
;                 (tool-bar-mode -1)
;                 (setq tabbar-ruler-toolbar-off 't)))))))
;     (setq tabbar-ruler-movement-timer (run-with-idle-timer
;                                        tabbar-ruler-movement-timer-delay
;                                        nil
;                                        'tabbar-ruler-mouse-movement))))
; (tabbar-ruler-mouse-movement)


; (setq tabbar-buffer-groups-function 'tabbar-buffer-groups)

; (defun last-tabbar-ruler-tabbar-buffer-groups nil)

; (defun tabbar-ruler-tabbar-buffer-groups ()
;   "Return the list of group names the current buffer belongs to.
; Return a list of one element based on major mode."
;   (setq last-tabbar-ruler-tabbar-buffer-groups
;         (list
;          (cond
;           ;;          ((or (get-buffer-process (current-buffer))
;           ;;               ;; Check if the major mode derives from `comint-mode' or
;           ;;               ;; `compilation-mode'.
;           ;;               (tabbar-buffer-mode-derived-p
;           ;;                major-mode '(comint-mode compilation-mode)))
;           ;;           "Process")
;           ;;    ((string-match "^ *[*]" (buffer-name))
;           ;;     "Common"
;           ;;     )
;           ((eq major-mode 'dired-mode)
;            "Dired")
;           ((memq major-mode
;                  '(help-mode apropos-mode Info-mode Man-mode))
;            "Help")
;           ((memq major-mode
;                  '(rmail-mode
;                    rmail-edit-mode vm-summary-mode vm-mode mail-mode
;                    mh-letter-mode mh-show-mode mh-folder-mode
;                    gnus-summary-mode message-mode gnus-group-mode
;                    gnus-article-mode score-mode gnus-browse-killed-mode))
;            "Mail")
;           (t
;            "Files"
;            ))))
;   (symbol-value 'last-tabbar-ruler-tabbar-buffer-groups))


; (defun tabbar-ruler-tabbar-buffer-list ()
;   "Return the list of buffers to show in tabs.
; Exclude buffers whose name starts with a space or *, when they are not
; visiting a file.  The current buffer is always included."
;   (delq nil
;         (mapcar #'(lambda (b)
;                     (cond
;                      ;; Always include the current buffer.
;                      ((eq (current-buffer) b) b)
;                      ((buffer-file-name b) b)
;                      ((member (buffer-name b) tabbar-ruler-excluded-buffers) nil)
;                      ;; ((string= "*Messages*" (format "%s" (buffer-name b))))
;                      ((char-equal ?\  (aref (buffer-name b) 0)) nil)
;                      ;;((char-equal ?* (aref (buffer-name b) 0)) nil)
;                      ((buffer-live-p b) b)))
;                 (buffer-list))))

(defvar tabbar-ruler-projectile-tabbar-buffer-group-calc nil 
  "Buffer group for projectile.  Should be buffer local and speed up calculation of buffer groups.")

(defun tabbar-ruler-projectile-tabbar-buffer-groups ()
  "Return the list of group names BUFFER belongs to.
    Return only one group for each buffer."
  
  (if tabbar-ruler-projectile-tabbar-buffer-group-calc
      (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
    (set (make-local-variable 'tabbar-ruler-projectile-tabbar-buffer-group-calc)
         
         (cond
          ((or (get-buffer-process (current-buffer)) (memq major-mode '(comint-mode compilation-mode))) '("Term"))
          ((string-equal "*" (substring (buffer-name) 0 1)) '("Misc"))
          ((condition-case err
               (projectile-project-root)
             (error nil)) (list (projectile-project-name)))
          ((memq major-mode '(emacs-lisp-mode python-mode emacs-lisp-mode c-mode c++-mode makefile-mode lua-mode vala-mode)) '("Coding"))
          ((memq major-mode '(javascript-mode js-mode nxhtml-mode html-mode css-mode)) '("HTML"))
          ((memq major-mode '(org-mode calendar-mode diary-mode)) '("Org"))
          ((memq major-mode '(dired-mode)) '("Dir"))
          (t '("Main"))))
    (symbol-value 'tabbar-ruler-projectile-tabbar-buffer-group-calc)))

(defun tabbar-ruler-group-by-projectile-project()
  "Group by projectile project."
  (interactive)
  (setq tabbar-buffer-groups-function 'tabbar-ruler-projectile-tabbar-buffer-groups))

; (defun tabbar-ruler-group-user-buffers-helper ()
;    ;; customize tabbar to have only 2 groups: emacs's and user's buffers
;    ;; all normal files will be shown in group user's buffers
;    (list (cond ((string-equal "*" (substring (buffer-name) 0 1)) "emacs's buffers")
;                ((eq major-mode 'dired-mode) "emacs's buffers")
;                (t "user's buffers"))))

; (defun tabbar-ruler-group-user-buffers ()
;    (interactive)
;    (setq tabbar-buffer-groups-function 'tabbar-ruler-group-user-buffers-helper))

; (defun tabbar-ruler-group-buffer-groups ()
;   "Use tabbar's major-mode grouping of buffers."
;   (interactive)
;   (setq tabbar-buffer-groups-function 'tabbar-buffer-groups))

; ;; default group mode
; (tabbar-ruler-group-user-buffers)

; ;;; Adapted from auto-hide in EmacsWiki

; (defvar tabbar-display-functions
;   '(tabbar-press-home
;     tabbar-backward
;     tabbar-forward
;     tabbar-backward-tab
;     tabbar-forward-tab
;     tabbar-backward-group
;     tabbar-forward-group
;     tabbar-press-scroll-left
;     tabbar-press-scroll-right)
;   "Tabbar movement functions")

; (defvar tabbar-ruler-keep-tabbar nil)

; (mapc
;  (lambda(x)
;    (eval `(defun ,(intern (concat "tabbar-ruler-" (symbol-name x))) (&optional arg)
;             ,(concat "Turn on tabbar before running `" (symbol-name x) "'")
;             (interactive "p")
;             (setq tabbar-ruler-keep-tabbar t)
;             (unless tabbar-ruler-ruler-off
;               (ruler-mode -1)
;               (setq tabbar-ruler-ruler-off 't))
;             (when tabbar-ruler-tabbar-off
;               (tabbar-mode 1)
;               (setq tabbar-ruler-tabbar-off nil))
;             (setq current-prefix-arg current-prefix-arg)
;             (call-interactively ',x)
;             (setq tabbar-ruler-keep-tabbar t)
;             (unless tabbar-ruler-ruler-off
;               (ruler-mode -1)
;               (setq tabbar-ruler-ruler-off 't))
;             (when tabbar-ruler-tabbar-off
;               (tabbar-mode 1)
;               (setq tabbar-ruler-tabbar-off nil)))))
;  tabbar-display-functions)

;;;###autoload
(defun tabbar-ruler-up (&optional arg)
  "Tabbar press up key."
  (interactive "p")
  (setq current-prefix-arg current-prefix-arg)
  (call-interactively 'tabbar-ruler-tabbar-press-home))

;;;###autoload
(defun tabbar-ruler-forward (&optional arg)
  "Forward ruler. Takes into consideration if the home-key was pressed.
This is based on the variable `tabbar--buffer-show-groups'"
  (interactive "p")
  (cond
   (tabbar--buffer-show-groups
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-forward-group)
    (tabbar-ruler-tabbar-press-home))
   (t
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-forward-tab))))

;;;###autoload
(defun tabbar-ruler-backward (&optional arg)
  "Backward ruler.  Takes into consideration if the home-key was pressed."
  (interactive "p")
  (cond
   (tabbar--buffer-show-groups
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-backward-group)
    (tabbar-ruler-tabbar-press-home))
   (t
    (setq current-prefix-arg current-prefix-arg)
    (call-interactively 'tabbar-ruler-tabbar-backward-tab))))

; (when (not (fboundp 'set-temporary-overlay-map))
;   ;; Backport this function from newer emacs versions
;   (defun set-temporary-overlay-map (map &optional keep-pred)
;     "Set a new keymap that will only exist for a short period of time.
; The new keymap to use must be given in the MAP variable. When to
; remove the keymap depends on user input and KEEP-PRED:

; - if KEEP-PRED is nil (the default), the keymap disappears as
;   soon as any key is pressed, whether or not the key is in MAP;

; - if KEEP-PRED is t, the keymap disappears as soon as a key *not*
;  i in MAP is pressed;

; - otherwise, KEEP-PRED must be a 0-arguments predicate that will
;   decide if the keymap should be removed (if predicate returns
;   nil) or kept (otherwise). The predicate will be called after
;   each key sequence."
    
;     (let* ((clearfunsym (make-symbol "clear-temporary-overlay-map"))
;            (overlaysym (make-symbol "t"))
;            (alist (list (cons overlaysym map)))
;            (clearfun
;             `(lambda ()
;                (unless ,(cond ((null keep-pred) nil)
;                               ((eq t keep-pred)
;                                `(eq this-command
;                                     (lookup-key ',map
;                                                 (this-command-keys-vector))))
;                               (t `(funcall ',keep-pred)))
;                  (remove-hook 'pre-command-hook ',clearfunsym)
;                  (setq emulation-mode-map-alists
;                        (delq ',alist emulation-mode-map-alists))))))
;       (set overlaysym overlaysym)
;       (fset clearfunsym clearfun)
;       (add-hook 'pre-command-hook clearfunsym)
      
;       (push alist emulation-mode-map-alists))))

; (defvar tabbar-ruler-move-keymap (make-sparse-keymap)
;   "Keymap for tabbar-ruler movement")

; (define-key tabbar-ruler-move-keymap [remap previous-line] 'tabbar-ruler-up)
; (define-key tabbar-ruler-move-keymap [remap next-line] 'tabbar-ruler-up)
; (define-key tabbar-ruler-move-keymap [remap backward-char] 'tabbar-ruler-backward)
; (define-key tabbar-ruler-move-keymap [remap forward-char] 'tabbar-ruler-forward)
; (define-key tabbar-ruler-move-keymap [remap left-char] 'tabbar-ruler-backward)
; (define-key tabbar-ruler-move-keymap [remap right-char] 'tabbar-ruler-forward)
; (define-key tabbar-ruler-move-keymap (kbd "SPC") (lambda() (interactive) (message "Exited tabbar-movement")))
; (define-key tabbar-ruler-move-keymap (kbd "<return>") (lambda() (interactive) (message "Exited tabbar-movement")))

; (defun tabbar-ruler-move-pred ()
;   "Determines if emacs should keep the temporary keymap
;   `tabbar-ruler-move-keymap' when running `tabbar-ruler-move'."
;   (memq this-command '(tabbar-ruler-up tabbar-ruler-backward tabbar-ruler-forward)))

; ;;;###autoload
; (defun tabbar-ruler-move ()
;   "Start the movement for the tabbar"
;   (interactive)
;   (setq tabbar-ruler-keep-tabbar t)
;   (unless tabbar-ruler-ruler-off
;     (ruler-mode -1)
;     (setq tabbar-ruler-ruler-off 't))
;   (when tabbar-ruler-tabbar-off
;     (tabbar-mode 1)
;     (setq tabbar-ruler-tabbar-off nil))
;   (message "Use arrow keys to change buffers (or line movement commands).  Exit with space/return or any other key.")
;   (set-temporary-overlay-map tabbar-ruler-move-keymap 'tabbar-ruler-move-pred))

; ;; Hook save and change events to show modified buffers in tabbar
; (defun on-saving-buffer ()
;   (tabbar-set-template tabbar-current-tabset nil)
;   (tabbar-display-update))
; (defun on-modifying-buffer ()
;   (set-buffer-modified-p (buffer-modified-p))
;   (tabbar-set-template tabbar-current-tabset nil)
;   (tabbar-display-update))
; (defun after-modifying-buffer (begin end length)
;   (set-buffer-modified-p (buffer-modified-p))
;   (tabbar-set-template tabbar-current-tabset nil)
;   (tabbar-display-update))
; (add-hook 'after-save-hook 'on-saving-buffer)
; (add-hook 'first-change-hook 'on-modifying-buffer)
; (add-hook 'after-change-functions 'after-modifying-buffer)

; (defun tabbar-ruler-remove-caches ()
;   "Remove caches for tabbar-ruler."
;   ;; Courtesy of Ba Manzi
;   ;; https://bitbucket.org/bamanzi/dotemacs-elite/issue/24/tabbar-ruler-not-work-in-emacs-244-keep
;   (mapc #'(lambda (frame)
;             (modify-frame-parameters frame '((tabbar-cache . nil))))
;         (frame-list)))

; (add-hook 'desktop-after-read-hook 'tabbar-ruler-remove-caches)

;;;###autoload
(defun tabbar-projectile-switch-project-action ()
  (interactive)
  )

(tabbar-ruler-group-by-projectile-project)

(tabbar-mode 1)

(provide 'tabbar)

(run-hooks 'tabbar-load-hook)

;;; tabbar.el ends here
