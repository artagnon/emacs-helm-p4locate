;;; helm-p4locate.el --- the silver searcher with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-p4locate
;; Version: 0.33
;; Package-Requires: ((helm "1.5.6") (cl-lib "0.5"))

;; This program is free software; you can redistribute it and/or modify
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

;;; Code:

(require 'cl-lib)
(require 'helm)
(require 'helm-utils)

(declare-function helm-read-file-name "helm-mode")

(defgroup helm-p4locate nil
  "the silver searcher with helm interface"
  :group 'helm)

(defcustom helm-p4locate-base-command "~/bin/p4locate"
  "Base command of `p4l'"
  :type 'string
  :group 'helm-p4locate)

(defcustom helm-p4locate-command-option nil
  "Command line option of `p4l'. This is appended after `helm-p4locate-base-command'"
  :type 'string
  :group 'helm-p4locate)

(defcustom helm-p4locate-insert-at-point nil
  "Insert thing at point as search pattern.
   You can set value same as `thing-at-point'"
  :type 'symbol
  :group 'helm-p4locate)

(defcustom helm-p4locate-source-type 'one-line
  "Style of candidates"
  :type '(choice (const :tag "Show file:line number:content in one line" one-line)
                 (const :tag "Helm file-line style" file-line))
  :group 'helm-p4locate)

(defcustom helm-p4locate-use-grep-ignore-list nil
  "Use `grep-find-ignored-files' and `grep-find-ignored-directories' as ignore pattern.
They are specified to `--ignore' options."
  :type 'boolean
  :group 'helm-p4locate)

(defcustom helm-p4locate-always-set-extra-option nil
  "Always set `p4l' options of `helm-do-p4l'."
  :type 'boolean
  :group 'helm-p4locate)

(defcustom helm-p4locate-fuzzy-match nil
  "Enable fuzzy match"
  :type 'boolean
  :group 'helm-p4locate)

(defcustom helm-p4locate-edit-save t
  "Save buffers you edit at completed."
  :type 'boolean
  :group 'helm-p4locate)

(defvar helm-p4locate--command-history '())
(defvar helm-p4locate--context-stack nil)
(defvar helm-p4locate--default-directory nil)
(defvar helm-p4locate--last-default-directory nil)
(defvar helm-p4locate--last-query nil)
(defvar helm-p4locate--extra-options nil)
(defvar helm-p4locate--extra-options-history nil)
(defvar helm-p4locate--original-window nil)
(defvar helm-do-p4l--default-target nil)
(defvar helm-do-p4l--extensions nil)

(defun helm-p4locate--save-current-context ()
  (let ((curpoint (with-helm-current-buffer
                    (point))))
    (helm-aif (buffer-file-name helm-current-buffer)
        (push (list :file it :point curpoint) helm-p4locate--context-stack)
      (push (list :buffer helm-current-buffer :point curpoint) helm-p4locate--context-stack))))

(defsubst helm-p4locate--insert-thing-at-point (thing)
  (helm-aif (thing-at-point thing)
      (substring-no-properties it)
    ""))

(defun helm-p4locate--searched-word ()
  (if helm-p4locate-insert-at-point
      (helm-p4locate--insert-thing-at-point helm-p4locate-insert-at-point)
    ""))

(defun helm-p4locate--grep-ignore-list-to-options ()
  (require 'grep)
  (cl-loop for ignore in (append grep-find-ignored-files
                                 grep-find-ignored-directories)
           collect (concat "--ignore=" ignore)))

(defun helm-p4locate--parse-query (query)
  (let ((inputs (ignore-errors (split-string-and-unquote query))))
    (if (or (null inputs) (= (length inputs) 1))
        (list query)
      (setq helm-p4locate--last-query (car (last inputs)))
      (append (butlast inputs) (last inputs)))))

(defun helm-p4locate--construct-command (this-file)
  (let* ((commands (split-string helm-p4locate-base-command nil t))
         (command (car commands))
         (args (cdr commands)))
    (when helm-p4locate-command-option
      (let ((ag-options (split-string helm-p4locate-command-option nil t)))
        (setq args (append args ag-options))))
    (when helm-p4locate-use-grep-ignore-list
      (setq args (append args (helm-p4locate--grep-ignore-list-to-options))))
    (setq args (append args (helm-p4locate--parse-query helm-p4locate--last-query)))
    (when this-file
      (setq args (append args (list this-file))))
    (cons command args)))

(defun helm-p4locate--init ()
  (let ((buf-coding buffer-file-coding-system))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((default-directory (or helm-p4locate--default-directory
                                    default-directory))
             (cmds (helm-p4locate--construct-command (helm-attr 'search-this-file)))
             (coding-system-for-read buf-coding)
             (coding-system-for-write buf-coding))
        (let ((ret (apply 'process-file (car cmds) nil t nil (cdr cmds))))
          (if (zerop (length (buffer-string)))
              (error "No output: '%s'" helm-p4locate--last-query)
            (unless (zerop ret)
              (unless (executable-find (car cmds))
                (error "'p4l' is not installed."))
              (error "Failed: '%s'" helm-p4locate--last-query))))
        (helm-p4locate--save-current-context)))))

(defun helm-p4locate--search-only-one-file-p ()
  (when (and helm-do-p4l--default-target (= (length helm-do-p4l--default-target) 1))
    (let ((target (car helm-do-p4l--default-target)))
      (unless (file-directory-p target)
        target))))

(defun helm-p4locate--find-file-action (candidate find-func)
  (let* ((search-this-file (or (helm-attr 'search-this-file)
                               (helm-p4locate--search-only-one-file-p)))
         (filename (or search-this-file candidate))
         (line (string-to-number candidate)))
    (funcall find-func filename)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun helm-p4locate--persistent-action (candidate)
  (let* ((elems (split-string candidate ":"))
         (search-this-file (helm-attr 'search-this-file))
         (filename (or search-this-file (cl-first elems)))
         (line (string-to-number (if search-this-file
                                     (cl-first elems)
                                   (cl-second elems))))
         (default-directory (or helm-p4locate--default-directory
                                helm-p4locate--last-default-directory)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defun helm-p4locate--validate-regexp (regexp)
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun helm-p4locate--highlight-candidate (candidate)
  (let ((limit (1- (length candidate)))
        (last-pos 0))
    (when (helm-p4locate--validate-regexp helm-p4locate--last-query)
      (while (and (< last-pos limit)
                  (string-match helm-p4locate--last-query candidate last-pos))
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'helm-match
                           candidate)
        (setq last-pos (1+ (match-end 0)))))
    candidate))

(defun helm-p4locate--candidate-transform-for-this-file (candidate)
  (when (string-match "\\`\\([^:]+\\):\\(.+\\)" candidate)
    (format "%s:%s"
            (propertize (match-string 1 candidate) 'face 'helm-grep-lineno)
            (helm-p4locate--highlight-candidate (match-string 2 candidate)))))

(defun helm-p4locate--candidate-transform-for-files (candidate)
  (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):\\(.+\\)" candidate)
    (format "%s:%s:%s"
            (propertize (match-string 1 candidate) 'face 'helm-moccur-buffer)
            (propertize (match-string 2 candidate) 'face 'helm-grep-lineno)
            (helm-p4locate--highlight-candidate (match-string 3 candidate)))))

(defun helm-p4locate--candidate-transformer (candidate)
  (if (helm-attr 'search-this-file)
      (helm-p4locate--candidate-transform-for-this-file candidate)
    (helm-p4locate--candidate-transform-for-files candidate)))

(defun helm-p4locate--action-find-file (candidate)
  (helm-p4locate--find-file-action candidate 'find-file))

(defun helm-p4locate--action--find-file-other-window (candidate)
  (helm-p4locate--find-file-action candidate 'find-file-other-window))

(defvar helm-p4locate--actions
  '(("Open file" . helm-p4locate--action-find-file)
    ("Open file other window" . helm-p4locate--action--find-file-other-window)))

(defvar helm-p4locate-source
  (helm-build-in-buffer-source "The Silver Searcher"
    :init 'helm-p4locate--init
    :real-to-display 'helm-p4locate--candidate-transformer
    :persistent-action 'helm-p4locate--persistent-action
    :fuzzy-match helm-p4locate-fuzzy-match
    :action helm-p4locate--actions))

(defvar helm-p4locate-source-grep
  '((name . "The Silver Searcher")
    (init . helm-p4locate--init)
    (candidates-in-buffer)
    (type . file-line)
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-p4locate-pop-stack ()
  (interactive)
  (let ((context (pop helm-p4locate--context-stack)))
    (unless context
      (error "Context stack is empty !"))
    (helm-aif (plist-get context :file)
        (find-file it)
      (let ((buf (plist-get context :buffer)))
        (if (buffer-live-p buf)
            (switch-to-buffer buf)
          (error "The buffer is already killed."))))
    (goto-char (plist-get context :point))))

;;;###autoload
(defun helm-p4locate-clear-stack ()
  (interactive)
  (setq helm-p4locate--context-stack nil))

(defun helm-p4locate--select-source ()
  (if (eq helm-p4locate-source-type 'file-line)
      'helm-p4locate-source-grep
    'helm-p4locate-source))

(defun helm-p4locate--query ()
  (let* ((searched-word (helm-p4locate--searched-word))
         (query (read-string "Pattern: " searched-word 'helm-p4locate--command-history)))
    (when (string= query "")
      (error "Input is empty!!"))
    (setq helm-p4locate--last-query query)))

(defsubst helm-p4locate--clear-variables ()
  (setq helm-p4locate--last-default-directory nil))

;;;###autoload
(defun helm-p4locate-this-file ()
  (interactive)
  (helm-p4locate--clear-variables)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (helm-p4locate--query)
    (let ((source (helm-p4locate--select-source)))
      (helm-attrset 'search-this-file (buffer-file-name) (symbol-value source))
      (helm-attrset 'name (format "Search at %s" filename) (symbol-value source))
      (helm :sources (list source) :buffer "*helm-p4locate*"))))

(defsubst helm-p4locate--has-c-u-preffix-p ()
  (and current-prefix-arg
       (or (equal current-prefix-arg '(4))
           (equal current-prefix-arg '(-4)))))

(defun helm-p4locate--get-default-directory ()
  (if (helm-p4locate--has-c-u-preffix-p)
      (file-name-as-directory
       (read-directory-name "Search directory: " nil nil t))
    default-directory))

(defsubst helm-p4locate--helm-header (dir)
  (concat "Search at " (abbreviate-file-name dir)))

(defun helm-p4locate--run-other-window-action ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-p4locate--action--find-file-other-window)))

(defsubst helm-p4locate--kill-edit-buffer ()
  (kill-buffer (get-buffer "*helm-p4locate-edit*")))

(defun helm-p4locate--edit-commit ()
  (interactive)
  (goto-char (point-min))
  (let ((read-only-files 0)
        (default-directory helm-p4locate--default-directory))
    (while (re-search-forward "^\\([^:]+\\):\\([1-9][0-9]*\\):\\(.*\\)$" nil t)
      (let ((file (match-string-no-properties 1))
            (line (string-to-number (match-string-no-properties 2)))
            (body (match-string-no-properties 3)))
        (with-current-buffer (find-file-noselect file)
          (if buffer-read-only
              (cl-incf read-only-files)
            (goto-char (point-min))
            (forward-line (1- line))
            (delete-region (line-beginning-position) (line-end-position))
            (insert body)
            (when helm-p4locate-edit-save
              (save-buffer))))))
    (select-window helm-p4locate--original-window)
    (helm-p4locate--kill-edit-buffer)
    (if (not (zerop read-only-files))
        (message "%d files are read-only and not editable." read-only-files)
      (message "Success helm-p4locate-edit"))))

(defun helm-p4locate--edit-abort ()
  (interactive)
  (when (y-or-n-p "Discard changes ?")
    (select-window helm-p4locate--original-window)
    (helm-p4locate--kill-edit-buffer)
    (message "Abort edit")))

(defvar helm-p4locate-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-p4locate--edit-commit)
    (define-key map (kbd "C-c C-k") 'helm-p4locate--edit-abort)
    map))

(defun helm-p4locate--edit (_candidate)
  (with-current-buffer (get-buffer-create "*helm-p4locate-edit*")
    (erase-buffer)
    (set (make-local-variable 'helm-p4locate--default-directory) helm-p4locate--default-directory)
    (let (buf-content)
      (with-current-buffer (get-buffer "*helm-p4locate*")
        (goto-char (point-min))
        (forward-line 1)
        (let* ((body-start (point))
               (marked-lines (cl-loop for ov in (overlays-in body-start (point-max))
                                      when (eq 'helm-visible-mark (overlay-get ov 'face))
                                      return (helm-marked-candidates))))
          (if (not marked-lines)
              (setq buf-content (buffer-substring-no-properties
                                 body-start (point-max)))
            (setq buf-content (concat (mapconcat 'identity marked-lines "\n") "\n")))))
      (insert buf-content)
      (add-text-properties (point-min) (point-max)
                           '(read-only t rear-nonsticky t front-sticky t))
      (let ((inhibit-read-only t))
        (setq header-line-format "[C-c C-c] Commit, [C-c C-k] Abort")
        (goto-char (point-min))
        (while (re-search-forward "^\\(\\(?:[^:]+:\\)\\{1,2\\}\\)\\(.*\\)$" nil t)
          (let ((file-line-begin (match-beginning 1))
                (file-line-end (match-end 1))
                (body-begin (match-beginning 2))
                (body-end (match-end 2)))
            (add-text-properties file-line-begin file-line-end
                                 '(face font-lock-function-name-face
                                        intangible t))
            (remove-text-properties body-begin body-end '(read-only t))
            (set-text-properties body-end (1+ body-end)
                                 '(read-only t rear-nonsticky t)))))))
  (other-window 1)
  (switch-to-buffer (get-buffer "*helm-p4locate-edit*"))
  (goto-char (point-min))
  (use-local-map helm-p4locate-edit-map))

(defun helm-p4locate-edit ()
  (interactive)
  (helm-quit-and-execute-action 'helm-p4locate--edit))

(defvar helm-p4locate-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-p4locate--run-other-window-action)
    (define-key map (kbd "C-l") 'helm-p4locate--up-one-level)
    (define-key map (kbd "C-c C-e") 'helm-p4locate-edit)
    map)
  "Keymap for `helm-p4locate'.")

(defsubst helm-p4locate--root-directory-p ()
  (cl-loop for dir in '(".git/" ".hg/")
           thereis (file-directory-p dir)))

(defun helm-p4locate--up-one-level ()
  (interactive)
  (if (or (not (helm-p4locate--root-directory-p))
          (y-or-n-p "Here may be project root. Continue searcing ? "))
      (let ((parent (file-name-directory (directory-file-name default-directory))))
        (helm-run-after-quit
         (lambda ()
           (let ((default-directory parent)
                 (source (helm-p4locate--select-source)))
             (helm-attrset 'name (helm-p4locate--helm-header default-directory)
                           (symbol-value source))
             (helm :sources (list source) :buffer "*helm-p4locate*"
                   :keymap helm-p4locate-map)))))
    (message nil)))

;;;###autoload
(defun helm-p4locate (&optional basedir)
  (interactive)
  (setq helm-p4locate--original-window (selected-window))
  (helm-p4locate--clear-variables)
  (let ((helm-p4locate--default-directory (or basedir (helm-p4locate--get-default-directory))))
    (helm-p4locate--query)
    (let ((source (helm-p4locate--select-source)))
      (helm-attrset 'search-this-file nil
                    (symbol-value source))
      (helm-attrset 'name (helm-p4locate--helm-header helm-p4locate--default-directory)
                    (symbol-value source))
      (helm :sources (list source) :buffer "*helm-p4locate*"
            :keymap helm-p4locate-map))))

(defun helm-p4locate--do-p4l-propertize ()
  (with-helm-window
    (goto-char (point-min))
    (when (helm-p4locate--validate-regexp helm-input)
      (cl-loop with one-file-p = (helm-p4locate--search-only-one-file-p)
               while (not (eobp))
               do
               (progn
                 (let ((start (point))
                       (bound (line-end-position))
                       file-end line-end)
                   (when (or one-file-p (search-forward ":" bound t))
                     (setq file-end (1- (point)))
                     (when (search-forward ":" bound t)
                       (setq line-end (1- (point)))
                       (unless one-file-p
                         (set-text-properties start file-end '(face helm-moccur-buffer)))
                       (set-text-properties (1+ file-end) line-end
                                            '(face helm-grep-lineno))

                       (when (re-search-forward helm-input bound t)
                         (set-text-properties (match-beginning 0) (match-end 0)
                                              '(face helm-match))))))
                 (forward-line 1))))
    (goto-char (point-min))
    (helm-display-mode-line (helm-get-current-source))))

(defun helm-p4locate--construct-extension-options ()
  (cl-loop for ext in helm-do-p4l--extensions
           unless (string= ext "*")
           collect
           (concat "-G" (replace-regexp-in-string
                         "\\*" ""
                         (replace-regexp-in-string "\\." "\\\\." ext)))))

(defun helm-p4locate--construct-targets (targets)
  (cl-loop for target in targets
           collect (file-relative-name target)))

(defun helm-p4locate--construct-do-p4l-command (pattern)
  (let ((cmds (split-string helm-p4locate-base-command nil t)))
    (when helm-p4locate-command-option
      (setq cmds (append cmds (split-string helm-p4locate-command-option nil t))))
    (when helm-p4locate--extra-options
      (setq cmds (append cmds (split-string helm-p4locate--extra-options))))
    (when helm-do-p4l--extensions
      (setq cmds (append cmds (helm-p4locate--construct-extension-options))))
    (setq cmds (append cmds (list "--" pattern)))
    (if helm-do-p4l--default-target
        (append cmds (helm-p4locate--construct-targets helm-do-p4l--default-target))
      cmds)))

(defun helm-p4locate--do-p4l-candidate-process ()
  (let* ((default-directory (or helm-p4locate--default-directory default-directory))
         (proc (apply 'start-file-process "helm-do-p4l" nil
                      (helm-p4locate--construct-do-p4l-command helm-pattern))))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory))
         (when (string= event "finished\n")
           (helm-p4locate--do-p4l-propertize)))))))

(defvar helm-do-p4l-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-p4locate--run-other-window-action)
    (define-key map (kbd "C-l") 'helm-p4locate--do-p4l-up-one-level)
    (define-key map (kbd "C-c C-e") 'helm-p4locate-edit)
    map)
  "Keymap for `helm-do-p4l'.")

(defvar helm-source-do-p4l
  `((name . "The Silver Searcher")
    (candidates-process . helm-p4locate--do-p4l-candidate-process)
    (persistent-action . helm-p4locate--persistent-action)
    (action . ,helm-p4locate--actions)
    (no-matchplugin)
    (nohighlight)
    (requires-pattern . 3)
    (candidate-number-limit . 9999)))

(defun helm-p4locate--do-p4l-up-one-level ()
  (interactive)
  (if (or (not (helm-p4locate--root-directory-p))
          (y-or-n-p "Here may be project root. Continue searcing ? "))
      (let ((parent (file-name-directory (directory-file-name default-directory)))
            (initial-input helm-input))
        (helm-run-after-quit
         (lambda ()
           (let ((default-directory parent))
             (helm-attrset 'name (helm-p4locate--helm-header parent)
                           helm-source-do-p4l)
             (helm :sources '(helm-source-do-p4l) :buffer "*helm-p4locate*"
                   :input initial-input :keymap helm-do-p4l-map)))))
    (message nil)))

(defun helm-p4locate--set-do-p4l-option ()
  (when (or (< (prefix-numeric-value current-prefix-arg) 0)
            helm-p4locate-always-set-extra-option)
    (let ((option (read-string "Extra options: " (or helm-p4locate--extra-options "")
                               'helm-p4locate--extra-options-history)))
      (setq helm-p4locate--extra-options option))))

(defun helm-p4locate--do-p4l-searched-extensions ()
  (when (helm-p4locate--has-c-u-preffix-p)
    (helm-grep-get-file-extensions helm-do-p4l--default-target)))

;;;###autoload
(defun helm-do-p4l (&optional basedir)
  (interactive)
  (require 'helm-mode)
  (setq helm-p4locate--original-window (selected-window))
  (helm-p4locate--clear-variables)
  (let* ((helm-p4locate--default-directory (or basedir default-directory))
         (helm-do-p4l--default-target (or basedir default-directory))
         (helm-do-p4l--extensions (helm-p4locate--do-p4l-searched-extensions)))
    (helm-p4locate--set-do-p4l-option)
    (helm-p4locate--save-current-context)
    (helm-attrset 'name (helm-p4locate--helm-header helm-p4locate--default-directory)
                  helm-source-do-p4l)
    (helm :sources '(helm-source-do-p4l) :buffer "*helm-p4locate*"
          :input (helm-p4locate--insert-thing-at-point helm-p4locate-insert-at-point)
          :keymap helm-do-p4l-map)))

(provide 'helm-p4locate)

;;; helm-p4locate.el ends here
