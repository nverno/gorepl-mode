;;; gorepl-mode.el --- Go REPL Interactive Development in top of Gore. -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Manuel Alonso

;; Author: Manuel Alonso <manuteali@gmail.com>
;; Maintainer: Manuel Alonso <manuteali@gmail.com>
;; URL: http://www.github.com/manute/gorepl-mode
;; Version: 1.0.0
;; Package-Requires: ((emacs "24") (s "1.11.0") (f "0.19.0"))
;; Keywords: languages, go, golang, gorepl

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This library provides a Go repl interactive development environment for Emacs, built on
;; top of Gore (https://github.com/motemen/gore).
;;
;;; Code:

(require 's)
(require 'f)
(require 'transient)
(require 'comint)

(defgroup gorepl nil
  "Go repl."
  :prefix "gorepl-"
  :group 'applications
  :link '(url-link :tag "Github" "https://github.com/manute/gorepl-mode")
  :link '(emacs-commentary-link :tag "Commentary" "gorepl"))

(defcustom gorepl-command "gore"
  "The command used to execute gore."
  :type 'string
  :group 'gorepl)

(defcustom gorepl-arguments '("-autoimport")
  "Default arguments for `gorepl-command'."
  :type '(repeat string))

(defcustom gorepl-buffer-name "Gore"
  "Default buffer name for the Gore repl."
  :type 'string)

(defconst gorepl-version "1.0.0")


(defun gorepl-buffer ()
  "Return inferior Gore buffer."
  (if (derived-mode-p 'gorepl-mode)
      (current-buffer)
    (let ((buffer-name (format "*%s*" gorepl-buffer-name)))
      (when (comint-check-proc buffer-name)
        buffer-name))))

(defun gorepl-process ()
  (get-buffer-process (gorepl-buffer)))

(defun gorepl-calculate-command (&optional prompt args)
  "Calculate the command to run Gore.
If PROMPT is non-nil, prompt user for command to run.
ARGS override `gorepl-arguments'."
  (let ((default (concat gorepl-command
                         " " (mapconcat 'identity (or args gorepl-arguments) " "))))
    (if prompt (read-shell-command "Run gore: " default) default)))

;; MANY THANKS to masteringenmacs for this:
;; https://www.masteringemacs.org/article/comint-writing-command-interpreter
(defun gorepl--run-gore (&optional prompt arguments show)
  "Run an inferior instance of Gore inside Emacs."
  (let ((buffer (gorepl-buffer)))
    (unless buffer
      (let* ((buffer-name (format "*%s*" gorepl-buffer-name))
             (cmdlist (split-string-and-unquote
                       (gorepl-calculate-command prompt arguments)))
             (program (car cmdlist))
             (args (cdr cmdlist)))
        (setq buffer (apply 'make-comint-in-buffer gorepl-buffer-name
                            buffer-name program nil args)))
      (with-current-buffer buffer
        (gorepl-mode)))
    (when show
      (pop-to-buffer buffer))
    buffer))

(define-derived-mode gorepl-mode comint-mode "GoREPL"
  "Major mode for interacting with an inferior Go REPL process."
  (setq-local comint-prompt-regexp "^gore> ")
  (setq-local comint-prompt-read-only t))

;;;;;;;;;;;;;;;;;;;;;;;
;; API
;;;;;;;;;;;;;;;;;;;;;;

(defun gorepl-version ()
  "Display GoREPL's version."
  (interactive)
  (message "GOREPL %s" gorepl-version))

(defun gorepl-run (&optional prompt show)
  "Start or switch to the GoREPL buffer.
With prefix PROMPT, read command to run Gore. When called interactively, or
if SHOW is non-nil, switch to the repl buffer."
  (interactive (list current-prefix-arg t))
  (gorepl--run-gore prompt nil show))

(defun gorepl-send-string (string &optional no-insert)
  "Send STRING to gore inferior process."
  (interactive (list (read-string "Gore command: ")))
  (let ((buf (gorepl-buffer)))
    (unless buf
      (user-error "No Gore repl running."))
    (with-current-buffer buf
      (if no-insert
          (comint-send-string (get-buffer-process (current-buffer)) string)
        (insert string))
      (comint-send-input))))

(defun gorepl-send-region (begin end)
  "Send region selected to Gore."
  (interactive "r")
  (gorepl-send-string (buffer-substring-no-properties begin end)))

(defun gorepl-send-line (&optional arg)
  "Evaluate current line."
  (interactive "P")
  (or arg (setq arg 1))
  (when (> arg 0)
    (gorepl-send-region (line-beginning-position) (line-end-position arg))))

(defun gorepl-load-file (filename &optional prompt show)
  "Run a GoREPL with a context file in it.
With prefix, prompt for FILENAME, otherwise load current buffer file.
With multiple prefix arguments PROMPT to edit run command."
  (interactive (list (if current-prefix-arg (read-file-name "Load file: ")
                       (buffer-file-name))
                     (> (prefix-numeric-value current-prefix-arg) 4)
                     t))
  (gorepl--run-gore
   prompt
   (append gorepl-arguments (list "-context" (expand-file-name filename)))
   show))

(defun gorepl-import (pkg)
  "Import <pkg path>"
  (interactive (list (read-string "Package path: ")))
  (when (s-contains? " " pkg)
    (user-error "Package names can't contain a space."))
  (gorepl-send-string (format ":import %s" pkg)))

(defun gorepl-print ()
  "Print the source code from this session"
  (interactive)
  (gorepl-send-string ":print"))

(defun gorepl-write ()
  "Write the source code from this session out to a file"
  (interactive)
  (let ((name (read-file-name "Output file name? ")))
    (when (s-blank? name)
      (user-error "No file name given."))
    (f-write-text (format "// gore dump on `%s' by `%s'\n\n"
                          (format-time-string
                           "%a %b %d %H:%M:%S %Z %Y"
                           (current-time))
                          (user-login-name))
                  'utf-8
                  name)
    (gorepl-send-string (format ":write %s" name))))

(defun gorepl-doc (exp-or-pkg)
  "Show documentation on <expression or package "
  (interactive (list (read-string "Expression or package? ")))
  (gorepl-send-string (format ":doc %s" exp-or-pkg)))

(defun gorepl-help ()
  "Show help."
  (interactive)
  (gorepl-send-string ":help"))

(defun gorepl-quit ()
  "Quit Gore repl."
  (interactive)
  (gorepl-send-string ":quit"))

(defun gorepl-restart (&optional show)
  "Restart gore. In others words: start a fresh gore session."
  (interactive (list t))
  (gorepl-quit)
  (sleep-for 1)
  (gorepl-run nil show))

(defun gorepl-send-line-and-step ()
  "Evaluate this line and move to next."
  (interactive)
  (call-interactively 'gorepl-send-line)
  (call-interactively 'next-logical-line))

;;;###autoload(autoload 'gorepl-menu "gorepl-mode" nil t)
(transient-define-prefix gorepl-menu ()
  [[ :if-not-mode gorepl-mode "Run"
    ("d" "Run empty" gorepl-run)
    ("f" "Run file" gorepl-load-file)]
   [ :if-not-mode gorepl-mode "Send"
    ("r" "Region" gorepl-send-region)
    ("c" "Line+Step" gorepl-send-line-and-step :transient t)
    ("l" "Line" gorepl-send-line)]
   ["REPL"
    ("i" "Import <pkg path>" gorepl-import)
    ("p" "Print this source" gorepl-print)
    ("w" "Write this source to <filename>" gorepl-write)
    ("h" "List `these' actual command" gorepl-help)
    ("R" "Restart this REPL" gorepl-restart)
    ("q" "Quit this REPL" gorepl-quit)]])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; DEFINE MINOR MODE
;;
;; Many thanks -> https://github.com/ruediger/rusti.el
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar-keymap gorepl-minor-mode-map
  :doc "Mode map for `gorepl-minor-mode'."
  "C-c C-g" #'gorepl-run
  "C-c C-l" #'gorepl-load-file
  "C-c C-r" #'gorepl-send-region
  "C-c C-e" #'gorepl-send-line)

(defcustom gorepl-minor-mode-lighter " GoRepl"
  "Text displayed in the mode line (Lighter) if `gorepl-minor-mode' is active."
  :group 'gorepl
  :type 'string)

;;;###autoload
(define-minor-mode gorepl-minor-mode
  "A minor mode to interact with an inferior Go REPL on top of gore."
  :group 'gorepl
  :lighter gorepl-minor-mode-lighter
  :keymap gorepl-minor-mode-map)


(provide 'gorepl-mode)
;;; gorepl-mode.el ends here
