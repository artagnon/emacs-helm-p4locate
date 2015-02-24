;;; helm-sblocate.el --- the silver searcher with helm interface -*- lexical-binding: t; -*-

;; Copyright (C) 2015 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>
;; URL: https://github.com/syohex/emacs-helm-sblocate
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

(defgroup helm-sblocate nil
  "the silver searcher with helm interface"
  :group 'helm)

(defcustom helm-sblocate-base-command "~/bin/p4locate"
  "Base command of `sbl'"
  :type 'string
  :group 'helm-sblocate)

(defcustom helm-sblocate-command-option nil
  "Command line option of `sbl'. This is appended after `helm-sblocate-base-command'"
  :type 'string
  :group 'helm-sblocate)

(defcustom helm-sblocate-insert-at-point nil
  "Insert thing at point as search pattern.
   You can set value same as `thing-at-point'"
  :type 'symbol
  :group 'helm-sblocate)

(defcustom helm-sblocate-source-type 'one-line
  "Style of candidates"
  :type '(choice (const :tag "Show file:line number:content in one line" one-line)
                 (const :tag "Helm file-line style" file-line))
  :group 'helm-sblocate)

(defcustom helm-sblocate-use-grep-ignore-list nil
  "Use `grep-find-ignored-files' and `grep-find-ignored-directories' as ignore pattern.
They are specified to `--ignore' options."
  :type 'boolean
  :group 'helm-sblocate)

(defcustom helm-sblocate-always-set-extra-option nil
  "Always set `sbl' options of `helm-do-sbl'."
  :type 'boolean
  :group 'helm-sblocate)

(defcustom helm-sblocate-fuzzy-match nil
  "Enable fuzzy match"
  :type 'boolean
  :group 'helm-sblocate)

(defcustom helm-sblocate-edit-save t
  "Save buffers you edit at completed."
  :type 'boolean
  :group 'helm-sblocate)

(defvar helm-sblocate--command-history '())
(defvar helm-sblocate--context-stack nil)
(defvar helm-sblocate--default-directory nil)
(defvar helm-sblocate--last-default-directory nil)
(defvar helm-sblocate--last-query nil)
(defvar helm-sblocate--extra-options nil)
(defvar helm-sblocate--extra-options-history nil)
(defvar helm-sblocate--original-window nil)
(defvar helm-do-sbl--default-target nil)
(defvar helm-do-sbl--extensions nil)

(defun helm-sblocate--save-current-context ()
  (let ((curpoint (with-helm-current-buffer
                    (point))))
    (helm-aif (buffer-file-name helm-current-buffer)
        (push (list :file it :point curpoint) helm-sblocate--context-stack)
      (push (list :buffer helm-current-buffer :point curpoint) helm-sblocate--context-stack))))

(defsubst helm-sblocate--insert-thing-at-point (thing)
  (helm-aif (thing-at-point thing)
      (substring-no-properties it)
    ""))

(defun helm-sblocate--searched-word ()
  (if helm-sblocate-insert-at-point
      (helm-sblocate--insert-thing-at-point helm-sblocate-insert-at-point)
    ""))

(defun helm-sblocate--grep-ignore-list-to-options ()
  (require 'grep)
  (cl-loop for ignore in (append grep-find-ignored-files
                                 grep-find-ignored-directories)
           collect (concat "--ignore=" ignore)))

(defun helm-sblocate--parse-query (query)
  (let ((inputs (ignore-errors (split-string-and-unquote query))))
    (if (or (null inputs) (= (length inputs) 1))
        (list query)
      (setq helm-sblocate--last-query (car (last inputs)))
      (append (butlast inputs) (last inputs)))))

(defun helm-sblocate--construct-command (this-file)
  (let* ((commands (split-string helm-sblocate-base-command nil t))
         (command (car commands))
         (args (cdr commands)))
    (when helm-sblocate-command-option
      (let ((ag-options (split-string helm-sblocate-command-option nil t)))
        (setq args (append args ag-options))))
    (when helm-sblocate-use-grep-ignore-list
      (setq args (append args (helm-sblocate--grep-ignore-list-to-options))))
    (setq args (append args (helm-sblocate--parse-query helm-sblocate--last-query)))
    (when this-file
      (setq args (append args (list this-file))))
    (cons command args)))

(defun helm-sblocate--init ()
  (let ((buf-coding buffer-file-coding-system))
    (helm-attrset 'recenter t)
    (with-current-buffer (helm-candidate-buffer 'global)
      (let* ((default-directory (or helm-sblocate--default-directory
                                    default-directory))
             (cmds (helm-sblocate--construct-command (helm-attr 'search-this-file)))
             (coding-system-for-read buf-coding)
             (coding-system-for-write buf-coding))
        (let ((ret (apply 'process-file (car cmds) nil t nil (cdr cmds))))
          (if (zerop (length (buffer-string)))
              (error "No output: '%s'" helm-sblocate--last-query)
            (unless (zerop ret)
              (unless (executable-find (car cmds))
                (error "'sbl' is not installed."))
              (error "Failed: '%s'" helm-sblocate--last-query))))
        (helm-sblocate--save-current-context)))))

(defun helm-sblocate--search-only-one-file-p ()
  (when (and helm-do-sbl--default-target (= (length helm-do-sbl--default-target) 1))
    (let ((target (car helm-do-sbl--default-target)))
      (unless (file-directory-p target)
        target))))

(defun helm-sblocate--find-file-action (candidate find-func)
  (let* ((search-this-file (or (helm-attr 'search-this-file)
                               (helm-sblocate--search-only-one-file-p)))
         (filename (or search-this-file candidate))
         (line (string-to-number candidate)))
    (funcall find-func filename)
    (goto-char (point-min))
    (forward-line (1- line))))

(defun helm-sblocate--persistent-action (candidate)
  (let* ((elems (split-string candidate ":"))
         (search-this-file (helm-attr 'search-this-file))
         (filename (or search-this-file (cl-first elems)))
         (line (string-to-number (if search-this-file
                                     (cl-first elems)
                                   (cl-second elems))))
         (default-directory (or helm-sblocate--default-directory
                                helm-sblocate--last-default-directory)))
    (find-file filename)
    (goto-char (point-min))
    (forward-line (1- line))
    (helm-highlight-current-line)))

(defun helm-sblocate--validate-regexp (regexp)
  (condition-case nil
      (progn
        (string-match-p regexp "")
        t)
    (invalid-regexp nil)))

(defun helm-sblocate--highlight-candidate (candidate)
  (let ((limit (1- (length candidate)))
        (last-pos 0))
    (when (helm-sblocate--validate-regexp helm-sblocate--last-query)
      (while (and (< last-pos limit)
                  (string-match helm-sblocate--last-query candidate last-pos))
        (put-text-property (match-beginning 0) (match-end 0)
                           'face 'helm-match
                           candidate)
        (setq last-pos (1+ (match-end 0)))))
    candidate))

(defun helm-sblocate--candidate-transform-for-this-file (candidate)
  (when (string-match "\\`\\([^:]+\\):\\(.+\\)" candidate)
    (format "%s:%s"
            (propertize (match-string 1 candidate) 'face 'helm-grep-lineno)
            (helm-sblocate--highlight-candidate (match-string 2 candidate)))))

(defun helm-sblocate--candidate-transform-for-files (candidate)
  (when (string-match "\\`\\([^:]+\\):\\([^:]+\\):\\(.+\\)" candidate)
    (format "%s:%s:%s"
            (propertize (match-string 1 candidate) 'face 'helm-moccur-buffer)
            (propertize (match-string 2 candidate) 'face 'helm-grep-lineno)
            (helm-sblocate--highlight-candidate (match-string 3 candidate)))))

(defun helm-sblocate--candidate-transformer (candidate)
  (if (helm-attr 'search-this-file)
      (helm-sblocate--candidate-transform-for-this-file candidate)
    (helm-sblocate--candidate-transform-for-files candidate)))

(defun helm-sblocate--action-find-file (candidate)
  (helm-sblocate--find-file-action candidate 'find-file))

(defun helm-sblocate--action--find-file-other-window (candidate)
  (helm-sblocate--find-file-action candidate 'find-file-other-window))

(defvar helm-sblocate--actions
  '(("Open file" . helm-sblocate--action-find-file)
    ("Open file other window" . helm-sblocate--action--find-file-other-window)))

(defvar helm-sblocate-source
  (helm-build-in-buffer-source "The Silver Searcher"
    :init 'helm-sblocate--init
    :real-to-display 'helm-sblocate--candidate-transformer
    :persistent-action 'helm-sblocate--persistent-action
    :fuzzy-match helm-sblocate-fuzzy-match
    :action helm-sblocate--actions))

(defvar helm-sblocate-source-grep
  '((name . "The Silver Searcher")
    (init . helm-sblocate--init)
    (candidates-in-buffer)
    (type . file-line)
    (candidate-number-limit . 9999)))

;;;###autoload
(defun helm-sblocate-pop-stack ()
  (interactive)
  (let ((context (pop helm-sblocate--context-stack)))
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
(defun helm-sblocate-clear-stack ()
  (interactive)
  (setq helm-sblocate--context-stack nil))

(defun helm-sblocate--select-source ()
  (if (eq helm-sblocate-source-type 'file-line)
      'helm-sblocate-source-grep
    'helm-sblocate-source))

(defun helm-sblocate--query ()
  (let* ((searched-word (helm-sblocate--searched-word))
         (query (read-string "Pattern: " searched-word 'helm-sblocate--command-history)))
    (when (string= query "")
      (error "Input is empty!!"))
    (setq helm-sblocate--last-query query)))

(defsubst helm-sblocate--clear-variables ()
  (setq helm-sblocate--last-default-directory nil))

;;;###autoload
(defun helm-sblocate-this-file ()
  (interactive)
  (helm-sblocate--clear-variables)
  (let ((filename (file-name-nondirectory (buffer-file-name))))
    (helm-sblocate--query)
    (let ((source (helm-sblocate--select-source)))
      (helm-attrset 'search-this-file (buffer-file-name) (symbol-value source))
      (helm-attrset 'name (format "Search at %s" filename) (symbol-value source))
      (helm :sources (list source) :buffer "*helm-sblocate*"))))

(defsubst helm-sblocate--has-c-u-preffix-p ()
  (and current-prefix-arg
       (or (equal current-prefix-arg '(4))
           (equal current-prefix-arg '(-4)))))

(defun helm-sblocate--get-default-directory ()
  (if (helm-sblocate--has-c-u-preffix-p)
      (file-name-as-directory
       (read-directory-name "Search directory: " nil nil t))
    default-directory))

(defsubst helm-sblocate--helm-header (dir)
  (concat "Search at " (abbreviate-file-name dir)))

(defun helm-sblocate--run-other-window-action ()
  (interactive)
  (with-helm-alive-p
    (helm-quit-and-execute-action 'helm-sblocate--action--find-file-other-window)))

(defsubst helm-sblocate--kill-edit-buffer ()
  (kill-buffer (get-buffer "*helm-sblocate-edit*")))

(defun helm-sblocate--edit-commit ()
  (interactive)
  (goto-char (point-min))
  (let ((read-only-files 0)
        (default-directory helm-sblocate--default-directory))
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
            (when helm-sblocate-edit-save
              (save-buffer))))))
    (select-window helm-sblocate--original-window)
    (helm-sblocate--kill-edit-buffer)
    (if (not (zerop read-only-files))
        (message "%d files are read-only and not editable." read-only-files)
      (message "Success helm-sblocate-edit"))))

(defun helm-sblocate--edit-abort ()
  (interactive)
  (when (y-or-n-p "Discard changes ?")
    (select-window helm-sblocate--original-window)
    (helm-sblocate--kill-edit-buffer)
    (message "Abort edit")))

(defvar helm-sblocate-edit-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-c") 'helm-sblocate--edit-commit)
    (define-key map (kbd "C-c C-k") 'helm-sblocate--edit-abort)
    map))

(defun helm-sblocate--edit (_candidate)
  (with-current-buffer (get-buffer-create "*helm-sblocate-edit*")
    (erase-buffer)
    (set (make-local-variable 'helm-sblocate--default-directory) helm-sblocate--default-directory)
    (let (buf-content)
      (with-current-buffer (get-buffer "*helm-sblocate*")
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
  (switch-to-buffer (get-buffer "*helm-sblocate-edit*"))
  (goto-char (point-min))
  (use-local-map helm-sblocate-edit-map))

(defun helm-sblocate-edit ()
  (interactive)
  (helm-quit-and-execute-action 'helm-sblocate--edit))

(defvar helm-sblocate-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-sblocate--run-other-window-action)
    (define-key map (kbd "C-l") 'helm-sblocate--up-one-level)
    (define-key map (kbd "C-c C-e") 'helm-sblocate-edit)
    map)
  "Keymap for `helm-sblocate'.")

(defsubst helm-sblocate--root-directory-p ()
  (cl-loop for dir in '(".git/" ".hg/")
           thereis (file-directory-p dir)))

(defun helm-sblocate--up-one-level ()
  (interactive)
  (if (or (not (helm-sblocate--root-directory-p))
          (y-or-n-p "Here may be project root. Continue searcing ? "))
      (let ((parent (file-name-directory (directory-file-name default-directory))))
        (helm-run-after-quit
         (lambda ()
           (let ((default-directory parent)
                 (source (helm-sblocate--select-source)))
             (helm-attrset 'name (helm-sblocate--helm-header default-directory)
                           (symbol-value source))
             (helm :sources (list source) :buffer "*helm-sblocate*"
                   :keymap helm-sblocate-map)))))
    (message nil)))

;;;###autoload
(defun helm-sblocate (&optional basedir)
  (interactive)
  (setq helm-sblocate--original-window (selected-window))
  (helm-sblocate--clear-variables)
  (let ((helm-sblocate--default-directory (or basedir (helm-sblocate--get-default-directory))))
    (helm-sblocate--query)
    (let ((source (helm-sblocate--select-source)))
      (helm-attrset 'search-this-file nil
                    (symbol-value source))
      (helm-attrset 'name (helm-sblocate--helm-header helm-sblocate--default-directory)
                    (symbol-value source))
      (helm :sources (list source) :buffer "*helm-sblocate*"
            :keymap helm-sblocate-map))))

(defun helm-sblocate--do-sbl-propertize ()
  (with-helm-window
    (goto-char (point-min))
    (when (helm-sblocate--validate-regexp helm-input)
      (cl-loop with one-file-p = (helm-sblocate--search-only-one-file-p)
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

(defun helm-sblocate--construct-extension-options ()
  (cl-loop for ext in helm-do-sbl--extensions
           unless (string= ext "*")
           collect
           (concat "-G" (replace-regexp-in-string
                         "\\*" ""
                         (replace-regexp-in-string "\\." "\\\\." ext)))))

(defun helm-sblocate--construct-targets (targets)
  (cl-loop for target in targets
           collect (file-relative-name target)))

(defun helm-sblocate--construct-do-sbl-command (pattern)
  (let ((cmds (split-string helm-sblocate-base-command nil t)))
    (when helm-sblocate-command-option
      (setq cmds (append cmds (split-string helm-sblocate-command-option nil t))))
    (when helm-sblocate--extra-options
      (setq cmds (append cmds (split-string helm-sblocate--extra-options))))
    (when helm-do-sbl--extensions
      (setq cmds (append cmds (helm-sblocate--construct-extension-options))))
    (setq cmds (append cmds (list "--" pattern)))
    (if helm-do-sbl--default-target
        (append cmds (helm-sblocate--construct-targets helm-do-sbl--default-target))
      cmds)))

(defun helm-sblocate--do-sbl-candidate-process ()
  (let* ((default-directory (or helm-sblocate--default-directory default-directory))
         (proc (apply 'start-file-process "helm-do-sbl" nil
                      (helm-sblocate--construct-do-sbl-command helm-pattern))))
    (prog1 proc
      (set-process-sentinel
       proc
       (lambda (process event)
         (helm-process-deferred-sentinel-hook
          process event (helm-default-directory))
         (when (string= event "finished\n")
           (helm-sblocate--do-sbl-propertize)))))))

(defvar helm-do-sbl-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "C-c o") 'helm-sblocate--run-other-window-action)
    (define-key map (kbd "C-l") 'helm-sblocate--do-sbl-up-one-level)
    (define-key map (kbd "C-c C-e") 'helm-sblocate-edit)
    map)
  "Keymap for `helm-do-sbl'.")

(defvar helm-source-do-sbl
  `((name . "The Silver Searcher")
    (candidates-process . helm-sblocate--do-sbl-candidate-process)
    (persistent-action . helm-sblocate--persistent-action)
    (action . ,helm-sblocate--actions)
    (no-matchplugin)
    (nohighlight)
    (requires-pattern . 3)
    (candidate-number-limit . 9999)))

(defun helm-sblocate--do-sbl-up-one-level ()
  (interactive)
  (if (or (not (helm-sblocate--root-directory-p))
          (y-or-n-p "Here may be project root. Continue searcing ? "))
      (let ((parent (file-name-directory (directory-file-name default-directory)))
            (initial-input helm-input))
        (helm-run-after-quit
         (lambda ()
           (let ((default-directory parent))
             (helm-attrset 'name (helm-sblocate--helm-header parent)
                           helm-source-do-sbl)
             (helm :sources '(helm-source-do-sbl) :buffer "*helm-sblocate*"
                   :input initial-input :keymap helm-do-sbl-map)))))
    (message nil)))

(defun helm-sblocate--set-do-sbl-option ()
  (when (or (< (prefix-numeric-value current-prefix-arg) 0)
            helm-sblocate-always-set-extra-option)
    (let ((option (read-string "Extra options: " (or helm-sblocate--extra-options "")
                               'helm-sblocate--extra-options-history)))
      (setq helm-sblocate--extra-options option))))

(defun helm-sblocate--do-sbl-searched-extensions ()
  (when (helm-sblocate--has-c-u-preffix-p)
    (helm-grep-get-file-extensions helm-do-sbl--default-target)))

;;;###autoload
(defun helm-do-sbl (&optional basedir)
  (interactive)
  (require 'helm-mode)
  (setq helm-sblocate--original-window (selected-window))
  (helm-sblocate--clear-variables)
  (let* ((helm-sblocate--default-directory (or basedir default-directory))
         (helm-do-sbl--default-target (or basedir default-directory))
         (helm-do-sbl--extensions (helm-sblocate--do-sbl-searched-extensions)))
    (helm-sblocate--set-do-sbl-option)
    (helm-sblocate--save-current-context)
    (helm-attrset 'name (helm-sblocate--helm-header helm-sblocate--default-directory)
                  helm-source-do-sbl)
    (helm :sources '(helm-source-do-sbl) :buffer "*helm-sblocate*"
          :input (helm-sblocate--insert-thing-at-point helm-sblocate-insert-at-point)
          :keymap helm-do-sbl-map)))

(provide 'helm-sblocate)

;;; helm-sblocate.el ends here
