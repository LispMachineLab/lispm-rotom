;;; lispm-rotom-ui.el --- Rotom's window  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-rotom
;; SPDX-License-Identifier: AGPL-3.0-or-later

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Affero General Public License as published
;; by the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Affero General Public License for more details.

;; You should have received a copy of the GNU Affero General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; M-x lispm-rotom opens Rotom's window: its daemon, running or not, and
;; where; the model LispM's agents use; the accounts, each signed in or
;; not; the models they offer.  Its keys:
;;
;;   RET  on an account, sign in to it; on a model, have LispM's agents
;;        use it; on the daemon, start or stop it
;;   s    start the daemon, or stop it
;;   r    restart the daemon
;;   l    sign in to an account, the one on the line or one asked for
;;   P    paste the address the browser shows, while signing in
;;   c    stop signing in
;;   R    refresh the accounts' tokens
;;   u    have LispM's agents use the model on the line, or one asked for
;;   x    have LispM's agents no longer use Rotom
;;   w    copy the model on the line, or the daemon's address
;;   v    view what the daemon wrote to its log of errors
;;   U    update Rotom, built anew with Cargo
;;   g    read Rotom again
;;   n p  the next and the previous line

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'text-property-search)
(require 'lispm-rotom)

(defface lispm-rotom-heading '((t :inherit bold))
  "Face of the headings of Rotom's window."
  :group 'lispm-rotom)

(defface lispm-rotom-good '((t :inherit success))
  "Face of what is well: a daemon running, an account signed in."
  :group 'lispm-rotom)

(defface lispm-rotom-bad '((t :inherit error))
  "Face of what is wrong: a token expired, a model Rotom does not offer."
  :group 'lispm-rotom)

(defface lispm-rotom-dim '((t :inherit shadow))
  "Face of what is not there: a daemon stopped, an account not signed in."
  :group 'lispm-rotom)

(defconst lispm-rotom-buffer-name "*Rotom*"
  "Name of the buffer of Rotom's window.")

(defconst lispm-rotom-column 24
  "Column the second column of Rotom's window starts at.")

(defconst lispm-rotom-third-column 46
  "Column the third column of the models starts at.")

;;;; What the line is about

(defun lispm-rotom-item ()
  "Return what the line at point is about: (account . PROVIDER),
\(model . ID), (daemon) or (lispm), or nil."
  (get-text-property (line-beginning-position) 'lispm-rotom-item))

(defun lispm-rotom--item-of (kind)
  "Return the thing of KIND the line at point is about, or nil."
  (let ((item (lispm-rotom-item)))
    (and (eq (car item) kind) (or (cdr item) t))))

(defun lispm-rotom-next-line (&optional n)
  "Go to the Nth next line about something."
  (interactive "p")
  (let ((n (or n 1)))
    (dotimes (_ (abs n))
      (let ((start (point)))
        (forward-line (if (< n 0) -1 1))
        (while (and (not (lispm-rotom-item)) (not (if (< n 0) (bobp) (eobp))))
          (forward-line (if (< n 0) -1 1)))
        (unless (lispm-rotom-item)
          (goto-char start))))))

(defun lispm-rotom-previous-line (&optional n)
  "Go to the Nth previous line about something."
  (interactive "p")
  (lispm-rotom-next-line (- (or n 1))))

;;;; Drawing

(defun lispm-rotom--column (column)
  "Return a space reaching COLUMN."
  (propertize " " 'display `(space :align-to ,column)))

(defun lispm-rotom--line (item first &rest more)
  "Insert a line about ITEM: FIRST, then MORE, each in its column."
  (let ((start (point)))
    (insert "  " first)
    (when more
      (insert (lispm-rotom--column lispm-rotom-column) (car more)))
    (when (cdr more)
      (insert (lispm-rotom--column lispm-rotom-third-column) (string-join (cdr more) "  ")))
    (insert "\n")
    (when item
      (put-text-property start (point) 'lispm-rotom-item item))))

(defun lispm-rotom--heading (text)
  "Insert the heading TEXT."
  (insert "\n" (propertize text 'face 'lispm-rotom-heading) "\n"))

(defun lispm-rotom--mark (good)
  "Return the mark of a line: filled when GOOD, else hollow."
  (if good (propertize "●" 'face 'lispm-rotom-good) (propertize "○" 'face 'lispm-rotom-dim)))

(defun lispm-rotom--insert-missing ()
  "Say that Rotom is not installed, and how it is."
  (insert (propertize "Rotom is not installed" 'face 'lispm-rotom-heading) "\n\n"
          "Rotom is a gateway on this machine to the models of your accounts: OpenAI's\n"
          "Codex, xAI's Grok, Kiro and Vercel's AI Gateway, for any program speaking\n"
          "OpenAI's API or Anthropic's, LispM's agents among them.\n")
  (when lispm-rotom-install-command
    (insert "\nIt is installed with Cargo, Rust's package manager:\n\n    "
            lispm-rotom-install-command "\n"))
  (insert "\nThen g reads it again.\n"))

(defun lispm-rotom--insert-daemon ()
  "Insert the line of the daemon."
  (let ((url (or (plist-get lispm-rotom-daemon :url) (lispm-rotom-url))))
    (lispm-rotom--line '(daemon) "Daemon"
                       (if (lispm-rotom-running-p)
                           (concat (lispm-rotom--mark t) " Running at " url)
                         (concat (lispm-rotom--mark nil)
                                 (propertize (if (plist-get lispm-rotom-daemon :installed)
                                                 " Stopped: s starts it"
                                               " Not running: s starts it")
                                             'face 'lispm-rotom-dim))))
    (when (lispm-rotom-running-p)
      (lispm-rotom--line nil ""
                         (propertize "OpenAI's API under /v1, Anthropic's at the root"
                                     'face 'lispm-rotom-dim)))))

(defun lispm-rotom--insert-lispm ()
  "Insert the line of LispM's agents, when LispM is there."
  (when-let* ((lispm (lispm-rotom-lispm)))
    (lispm-rotom--line
     '(lispm) "LispM's agents"
     (cond ((not (plist-get lispm :rotom))
            (propertize (if (plist-get lispm :backend)
                            (format "Use %s, not Rotom: u chooses one of its models" (plist-get lispm :backend))
                          "No backend yet: u chooses one of Rotom's models")
                        'face 'lispm-rotom-dim))
           ((plist-get lispm :listed)
            (format "Use %s through Rotom" (plist-get lispm :model)))
           (t (concat (format "Use %s through Rotom" (plist-get lispm :model))
                      (propertize ", which it does not offer" 'face 'lispm-rotom-bad)))))))

(defun lispm-rotom--insert-accounts ()
  "Insert the accounts, signed in or not."
  (lispm-rotom--heading "Accounts")
  (pcase-dolist (`(,provider . ,properties) lispm-rotom-providers)
    (let ((account (lispm-rotom-account provider)))
      (lispm-rotom--line
       (cons 'account provider)
       (concat (lispm-rotom--mark account) " " (plist-get properties :name))
       (cond ((null account) (propertize "Not signed in: l signs in" 'face 'lispm-rotom-dim))
             ((or (eq (plist-get account :status) 'refresh-failed)
                  (eql (plist-get account :expires) 0))
              (propertize (concat "Signed in, " (lispm-rotom-account-text account)) 'face 'lispm-rotom-bad))
             (t (concat "Signed in, " (lispm-rotom-account-text account)))))
      (when (and account (equal (symbol-name provider) (plist-get lispm-rotom-served :provider)))
        (when-let* ((limits (lispm-rotom-limits-text)))
          (lispm-rotom--line (cons 'account provider) "" (propertize limits 'face 'lispm-rotom-dim)))))))

(defun lispm-rotom--insert-models ()
  "Insert the models, by account, those of accounts signed in first."
  (lispm-rotom--heading "Models")
  (let* ((lispm (lispm-rotom-lispm))
         (used (and (plist-get lispm :rotom) (plist-get lispm :model)))
         (entries (append (seq-filter (lambda (entry) (lispm-rotom-signed-in-p (car entry))) lispm-rotom-models)
                          (seq-remove (lambda (entry) (lispm-rotom-signed-in-p (car entry))) lispm-rotom-models))))
    (if (null entries)
        (insert (propertize "  Rotom lists no models.\n" 'face 'lispm-rotom-dim))
      (pcase-dolist (`(,provider . ,models) entries)
        (let ((served (lispm-rotom-signed-in-p provider)))
          (dolist (model models)
            (lispm-rotom--line
             (cons 'model model)
             (propertize model 'face (if served 'default 'lispm-rotom-dim))
             (propertize (lispm-rotom-provider-name provider) 'face 'lispm-rotom-dim)
             (cond ((equal model used) (propertize "LispM's agents use it" 'face 'lispm-rotom-good))
                   ((not served) (propertize "not signed in" 'face 'lispm-rotom-dim))
                   (t "")))))))))

(defun lispm-rotom--print ()
  "Show where Rotom stands in the current buffer, point kept on its line."
  (let ((item (lispm-rotom-item))
        (column (current-column))
        (inhibit-read-only t))
    (erase-buffer)
    (pcase lispm-rotom-state
      ('missing (lispm-rotom--insert-missing))
      ((or 'nil 'reading) (insert (propertize "Reading Rotom…" 'face 'lispm-rotom-dim) "\n"))
      (_
       (insert (propertize (concat "Rotom " (or lispm-rotom-version "")) 'face 'lispm-rotom-heading)
               "\n")
       (when lispm-rotom-busy
         (insert (propertize lispm-rotom-busy 'face 'lispm-rotom-good)
                 (if (and lispm-rotom-signing-in
                          (eq (lispm-rotom-sign-in-state lispm-rotom-signing-in) 'browser))
                     (propertize "  P pastes the address your browser shows, c stops" 'face 'lispm-rotom-dim)
                   "")
                 "\n"))
       (when lispm-rotom-message
         (insert (propertize lispm-rotom-message 'face 'lispm-rotom-bad) "\n"))
       (insert "\n")
       (lispm-rotom--insert-daemon)
       (lispm-rotom--insert-lispm)
       (when (eq lispm-rotom-state 'no-account)
         (insert "\n" (propertize "No account is signed in yet: l signs in to one." 'face 'lispm-rotom-bad) "\n"))
       (lispm-rotom--insert-accounts)
       (lispm-rotom--insert-models)))
    (goto-char (point-min))
    (if-let* ((match (and item (text-property-search-forward 'lispm-rotom-item item #'equal))))
        (progn (goto-char (prop-match-beginning match))
               (move-to-column column))
      (goto-char (point-min))
      (lispm-rotom-next-line))))

(defun lispm-rotom--refresh ()
  "Show what Rotom knows now in its window.
This is one of `lispm-rotom-changed-hook'."
  (when-let* ((buffer (get-buffer lispm-rotom-buffer-name)))
    (with-current-buffer buffer
      (let ((windows (get-buffer-window-list buffer nil t)))
        (lispm-rotom--print)
        (dolist (window windows)
          (set-window-point window (point)))))))

(add-hook 'lispm-rotom-changed-hook #'lispm-rotom--refresh)

;;;; Commands

(defun lispm-rotom-reload (&rest _)
  "Read Rotom again."
  (interactive)
  (lispm-rotom-read))

(defun lispm-rotom-start-or-stop ()
  "Start Rotom's daemon, or stop it while it runs."
  (interactive)
  (cond ((not (lispm-rotom-running-p)) (lispm-rotom-start))
        ((yes-or-no-p "Stop Rotom's daemon?  Programs using it, LispM's agents among them, reach no model then. ")
         (lispm-rotom-stop))))

(defun lispm-rotom-sign-in-here ()
  "Sign in to the account on the line, or to one asked for."
  (interactive)
  (if-let* ((provider (lispm-rotom--item-of 'account)))
      (lispm-rotom-sign-in provider)
    (call-interactively #'lispm-rotom-sign-in)))

(defun lispm-rotom-use-here ()
  "Have LispM's agents use the model on the line, or one asked for."
  (interactive)
  (let ((model (lispm-rotom--item-of 'model)))
    (if (and (stringp model)
             (yes-or-no-p (format "Have LispM's agents use %s through Rotom? " model)))
        (lispm-rotom-use model)
      (unless model
        (call-interactively #'lispm-rotom-use)))))

(defun lispm-rotom-act ()
  "Do what the line at point is for: sign in to its account, have
LispM's agents use its model, start or stop the daemon."
  (interactive)
  (pcase (lispm-rotom-item)
    (`(account . ,provider)
     (when (or (not (lispm-rotom-signed-in-p provider))
               (yes-or-no-p (format "Sign in to %s again? " (lispm-rotom-provider-name provider))))
       (lispm-rotom-sign-in provider)))
    (`(model . ,_) (lispm-rotom-use-here))
    ('(daemon) (lispm-rotom-start-or-stop))
    ('(lispm) (call-interactively #'lispm-rotom-use))
    (_ (user-error "Nothing to do on this line"))))

(defun lispm-rotom-copy ()
  "Copy the model on the line, or the daemon's address."
  (interactive)
  (let ((text (or (and (stringp (lispm-rotom--item-of 'model)) (lispm-rotom--item-of 'model))
                  (plist-get lispm-rotom-daemon :url)
                  (lispm-rotom-url))))
    (kill-new text)
    (message "Copied %s" text)))

(defun lispm-rotom-show-log ()
  "Show what Rotom's daemon wrote to its log of errors, following it."
  (interactive)
  (let ((file (lispm-rotom-log-file)))
    (unless (file-readable-p file)
      (user-error "Rotom's daemon has written no log at %s" (abbreviate-file-name file)))
    (with-current-buffer (find-file-noselect file t)
      (setq buffer-read-only t)
      (auto-revert-tail-mode 1)
      (goto-char (point-max))
      (pop-to-buffer (current-buffer)))))

(defun lispm-rotom-update ()
  "Update Rotom to its latest release, built anew with Cargo, then restart
its daemon.  It takes some minutes; what Cargo says shows in a buffer."
  (interactive)
  (unless (lispm-rotom-installed-p)
    (user-error "Rotom is not installed"))
  (unless (executable-find "cargo")
    (user-error "Rotom is updated with Cargo, which is not installed"))
  (when (yes-or-no-p "Update Rotom?  Cargo builds its latest release, which takes some minutes. ")
    (let ((buffer (get-buffer-create "*Rotom Update*")))
      (with-current-buffer buffer
        (let ((inhibit-read-only t)) (erase-buffer))
        (special-mode))
      (display-buffer buffer)
      (lispm-rotom--busy "Updating Rotom…")
      (make-process
       :name "lispm-rotom-update" :buffer buffer :noquery t :connection-type 'pipe
       :command (list (lispm-rotom-installed-p) "update")
       :filter (lambda (process text)
                 (when (buffer-live-p (process-buffer process))
                   (with-current-buffer (process-buffer process)
                     (let ((inhibit-read-only t))
                       (goto-char (point-max))
                       (insert (replace-regexp-in-string "\r" "\n" text))))))
       :sentinel (lambda (process _event)
                   (unless (process-live-p process)
                     (lispm-rotom--busy nil)
                     (if (eql (process-exit-status process) 0)
                         (if (lispm-rotom-running-p)
                             (lispm-rotom-restart)
                           (lispm-rotom-read))
                       (lispm-rotom--failed "Rotom could not be updated: see *Rotom Update*"))))))))

;;;; The window

(defvar-keymap lispm-rotom-mode-map
  :doc "Keys of Rotom's window."
  "RET" #'lispm-rotom-act
  "s" #'lispm-rotom-start-or-stop
  "r" #'lispm-rotom-restart
  "l" #'lispm-rotom-sign-in-here
  "P" #'lispm-rotom-paste-address
  "c" #'lispm-rotom-cancel-sign-in
  "R" #'lispm-rotom-refresh-tokens
  "u" #'lispm-rotom-use-here
  "x" #'lispm-rotom-stop-using
  "w" #'lispm-rotom-copy
  "v" #'lispm-rotom-show-log
  "U" #'lispm-rotom-update
  "g" #'lispm-rotom-reload
  "n" #'lispm-rotom-next-line
  "p" #'lispm-rotom-previous-line)

(defun lispm-rotom--signing-in-p ()
  "Return non-nil while an account is being signed in to."
  (and lispm-rotom-signing-in
       (process-live-p (lispm-rotom-sign-in-process lispm-rotom-signing-in))))

(easy-menu-define lispm-rotom-menu lispm-rotom-mode-map
  "Rotom's menu."
  '("Rotom"
    ["Start Daemon" lispm-rotom-start
     :active (and (lispm-rotom-installed-p) (not (lispm-rotom-running-p)) (not lispm-rotom-busy))]
    ["Stop Daemon" lispm-rotom-start-or-stop
     :active (and (lispm-rotom-running-p) (not lispm-rotom-busy))]
    ["Restart Daemon" lispm-rotom-restart
     :active (and (lispm-rotom-installed-p) (not lispm-rotom-busy))]
    "---"
    ["Sign In…" lispm-rotom-sign-in-here
     :active (and (lispm-rotom-installed-p) (not (lispm-rotom--signing-in-p)))]
    ["Paste Address…" lispm-rotom-paste-address :active (lispm-rotom--signing-in-p)]
    ["Stop Signing In" lispm-rotom-cancel-sign-in :active (lispm-rotom--signing-in-p)]
    ["Refresh Tokens" lispm-rotom-refresh-tokens
     :active (and lispm-rotom-accounts (not lispm-rotom-busy))]
    "---"
    ["Use Model for LispM…" lispm-rotom-use-here
     :active (and (featurep 'lispm-core) (lispm-rotom-model-ids))]
    ["Stop Using Rotom for LispM" lispm-rotom-stop-using
     :active (plist-get (lispm-rotom-lispm) :rotom)]
    "---"
    ["Copy" lispm-rotom-copy :active (memq lispm-rotom-state '(ready no-account))]
    ["Show Log" lispm-rotom-show-log :active (file-readable-p (lispm-rotom-log-file))]
    ["Update Rotom…" lispm-rotom-update :active (and (lispm-rotom-installed-p) (not lispm-rotom-busy))]
    ["Read Again" lispm-rotom-reload]))

(define-derived-mode lispm-rotom-mode special-mode "Rotom"
  "Rotom's window: its daemon, the accounts signed in to it, its models,
and the model LispM's agents use.
\\{lispm-rotom-mode-map}"
  (setq truncate-lines t
        buffer-read-only t)
  ;; A line cut at the window's edge ends in an ellipsis, not a dollar.
  (setq buffer-display-table (or (copy-sequence buffer-display-table) (make-display-table)))
  (set-display-table-slot buffer-display-table 'truncation ?…)
  (setq-local revert-buffer-function #'lispm-rotom-reload))

;;;###autoload
(defun lispm-rotom ()
  "Open Rotom's window: its daemon, accounts and models, read anew."
  (interactive)
  (let ((buffer (or (get-buffer lispm-rotom-buffer-name)
                    (with-current-buffer (get-buffer-create lispm-rotom-buffer-name)
                      (lispm-rotom-mode)
                      (current-buffer)))))
    (switch-to-buffer buffer)
    (lispm-rotom--print)
    (lispm-rotom-read)))

(provide 'lispm-rotom-ui)
;;; lispm-rotom-ui.el ends here
