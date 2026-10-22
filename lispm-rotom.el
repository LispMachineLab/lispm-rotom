;;; lispm-rotom.el --- Rotom, a gateway to models on this machine  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
;; URL: https://github.com/LispMachineLab/lispm-rotom
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: tools, comm
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

;; Rotom (https://github.com/RyanKung/rotom) is a gateway on this
;; machine to the models of the accounts signed in to it: OpenAI's
;; Codex, xAI's Grok, Kiro and Vercel's AI Gateway.  Programs speaking
;; OpenAI's API, Chat Completions or Responses, or Anthropic's,
;; Messages, talk to its daemon, at http://127.0.0.1:14550 unless
;; configured otherwise, and it passes each request on to the account
;; serving the model asked for.
;;
;; M-x lispm-rotom opens its window: whether the daemon runs, and
;; where; the accounts, each signed in or not and how long its token
;; lasts; the models, by account; and the model LispM's agents use.
;; From there the daemon is started, stopped and restarted, accounts
;; are signed in to and their tokens refreshed, and a model is chosen
;; for LispM.
;;
;; Codex, Grok and Kiro are signed in to in the browser.  Rotom then
;; asks for the address the browser lands on once signed in: a page of
;; this machine that nothing serves.  While it asks, Emacs serves that
;; address, so the browser lands on Emacs and Rotom gets the address
;; without a copy and paste.  Where Emacs cannot, as when another
;; program holds the port, the address is asked in the minibuffer.
;; Vercel's AI Gateway takes a key, read in the minibuffer and handed to
;; Rotom on its standard input, never kept.
;;
;; Rotom's two sides are backends of LispM Agent: `rotom', Chat
;; Completions, and `rotom-messages', Messages.  They are added to
;; `lispm-backends' once LispM is loaded, unless entries of those names
;; are there already, and nothing more: `lispm-rotom-use' makes LispM's
;; agents use one of Rotom's models, `lispm-rotom-stop-using' stops it.
;;
;; This file runs Rotom's program, rotom, reads its daemon over HTTP
;; with curl and keeps what they said; lispm-rotom-ui.el is the window.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'browse-url)
(require 'url-parse)
(require 'url-util)

(defvar lispm-backends)
(defvar lispm-backend)
(defvar lispm-model)
(declare-function lispm-backend-name "lispm-core" (&optional agent))

(defgroup lispm-rotom nil
  "Rotom, a gateway to models on this machine."
  :group 'applications
  :prefix "lispm-rotom-")

(defcustom lispm-rotom-program "rotom"
  "Rotom's program: its name on `exec-path', or its file."
  :type 'string)

(defcustom lispm-rotom-curl-program "curl"
  "The curl program, reading Rotom's daemon over HTTP."
  :type 'string)

(defcustom lispm-rotom-directory nil
  "Rotom's directory, holding its configuration and the daemon's logs.
nil means where Rotom keeps them: $ROTOM_HOME, else ~/.rotom."
  :type '(choice (const :tag "Rotom's own" nil) directory))

(defcustom lispm-rotom-install-command "cargo install --locked rotom"
  "The command installing Rotom, shown while it is missing; nil for none."
  :type '(choice (const :tag "None" nil) string))

(defcustom lispm-rotom-browse-function #'browse-url-default-browser
  "Function opening the page that signs in to an account, given its URL.
The system's browser by default, where the accounts' sites know you."
  :type 'function)

(defcustom lispm-rotom-timeout 60
  "Seconds Rotom's program may take before it is stopped.
Signing in is not timed: it waits for the browser."
  :type 'natnum)

(defconst lispm-rotom-providers
  `((codex :name "OpenAI (Codex)" :sign-in browser)
    (grok :name "Grok (xAI)" :sign-in browser)
    (kiro :name "Kiro" :sign-in browser)
    (vercel :name "Vercel AI Gateway" :sign-in key))
  "The accounts Rotom serves: (PROVIDER . PROPERTIES), PROVIDER as Rotom
names it.  :name is what it is called, :sign-in `browser' for one signed
in to in the browser, `key' for one taking a key.")

(defconst lispm-rotom-default-url "http://127.0.0.1:14550"
  "Where Rotom's daemon listens unless configured otherwise.")

(defconst lispm-rotom-fallback-model "gpt-6-sol"
  "The model Rotom falls back to unless configured otherwise.")

;;;; What Rotom is

(defvar lispm-rotom-state nil
  "Where Rotom stands: nil before it is read, `reading' while it is,
`missing' without its program, `no-account' while no account is signed
in, or `ready'.")

(defvar lispm-rotom-version nil
  "The version of Rotom's program, a string.")

(defvar lispm-rotom-accounts nil
  "The accounts signed in to Rotom, as it reports them: plists of
:provider, a symbol, :status, `authenticated' or `refresh-failed', :token,
what Rotom says of the token, :expires, the seconds it lasts, nil for a
key that does not expire, and :highlights, its strongest models.")

(defvar lispm-rotom-models nil
  "The models Rotom offers, by account: ((PROVIDER . MODELS) ...).")

(defvar lispm-rotom-daemon nil
  "Rotom's daemon: a plist of :running, :url where it listens, and
:installed, non-nil when it is a service of the system.")

(defvar lispm-rotom-served nil
  "What the daemon says of the account it serves first: the plist of its
/v1/status, :provider, :credits_balance, :rate_limits and :warnings.")

(defvar lispm-rotom-config nil
  "Rotom's configuration, from its config.json: a plist.")

(defvar lispm-rotom-message nil
  "What Rotom said when it last failed, for the window to show.")

(defvar lispm-rotom-read-at nil
  "When Rotom was last read.")

(defvar lispm-rotom-changed-hook nil
  "Hook run when what Rotom shows changed.")

(defun lispm-rotom--changed ()
  "Say that what Rotom shows changed."
  (run-hooks 'lispm-rotom-changed-hook))

(defun lispm-rotom-provider-name (provider)
  "Return what PROVIDER, a symbol or a string, is called."
  (let ((provider (if (stringp provider) (intern provider) provider)))
    (or (plist-get (alist-get provider lispm-rotom-providers) :name)
        (symbol-name provider))))

(defun lispm-rotom-account (provider)
  "Return the account of PROVIDER signed in to Rotom, or nil."
  (seq-find (lambda (account) (eq (plist-get account :provider) provider))
            lispm-rotom-accounts))

(defun lispm-rotom-signed-in-p (provider)
  "Return non-nil when PROVIDER's account is signed in to Rotom."
  (and (lispm-rotom-account provider) t))

(defun lispm-rotom-model-ids (&optional all)
  "Return the models Rotom offers through the accounts signed in to it.
With ALL, those of every account."
  (seq-uniq
   (mapcan (pcase-lambda (`(,provider . ,models))
             (and (or all (lispm-rotom-signed-in-p provider))
                  (copy-sequence models)))
           lispm-rotom-models)))

(defun lispm-rotom-model-provider (model)
  "Return the account offering MODEL, or nil."
  (car (seq-find (lambda (entry) (member model (cdr entry))) lispm-rotom-models)))

;;;; Where Rotom is

(defun lispm-rotom-installed-p ()
  "Return Rotom's program file, or nil when it is not installed: on
`exec-path', or where `cargo install rotom' leaves it, $CARGO_HOME/bin or
~/.cargo/bin, which a shell may not have put on PATH."
  (or (executable-find lispm-rotom-program)
      (let ((file (expand-file-name lispm-rotom-program
                                    (expand-file-name "bin" (or (getenv "CARGO_HOME") "~/.cargo")))))
        (and (not (file-name-absolute-p lispm-rotom-program))
             (file-executable-p file)
             file))))

(defun lispm-rotom-home ()
  "Return Rotom's directory."
  (file-name-as-directory
   (expand-file-name (or lispm-rotom-directory
                         (getenv "ROTOM_HOME")
                         "~/.rotom"))))

(defun lispm-rotom-log-file ()
  "Return the file Rotom's daemon writes what goes wrong to."
  (expand-file-name "rotom.err.log" (lispm-rotom-home)))

(defun lispm-rotom--read-config ()
  "Return Rotom's configuration, from its config.json, as a plist; nil
without one."
  (let ((file (expand-file-name "config.json" (lispm-rotom-home))))
    (when (file-readable-p file)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents file)
          (json-parse-buffer :object-type 'plist :array-type 'list
                             :null-object nil :false-object nil))))))

(defun lispm-rotom-config ()
  "Return Rotom's configuration, read anew when not read yet."
  (or lispm-rotom-config
      (setq lispm-rotom-config (lispm-rotom--read-config))))

(defun lispm-rotom-url ()
  "Return where Rotom's daemon listens: its base URL, without a slash.
An address listening everywhere is reached on this machine's."
  (let* ((config (lispm-rotom-config))
         (host (plist-get config :bind_host))
         (port (plist-get config :bind_port)))
    (if (and host port)
        (format "http://%s:%s"
                (if (member host '("0.0.0.0" "::" "[::]")) "127.0.0.1" host)
                port)
      lispm-rotom-default-url)))

(defun lispm-rotom-api-key ()
  "Return the key Rotom's daemon asks of the programs using it, or nil.
It is Rotom's own, kept in its configuration or in $ROTOM_API_KEY, not
a key of any account."
  (let ((key (or (plist-get (lispm-rotom-config) :api_key)
                 (getenv "ROTOM_API_KEY"))))
    (and (stringp key) (not (string-empty-p key)) key)))

(defun lispm-rotom-fallback-model ()
  "Return the model Rotom answers with for a model id it does not know."
  (or (plist-get (lispm-rotom-config) :model_fallback)
      lispm-rotom-fallback-model))

;;;; Running rotom

(defun lispm-rotom--said (text)
  "Return what rotom said in TEXT, without its colours and labels."
  (let ((said (string-trim
               (replace-regexp-in-string
                "^\\(?:error\\|warning\\): " ""
                (replace-regexp-in-string "\e\\[[0-9;]*m" "" text)))))
    (if (string-empty-p said)
        said
      (concat (upcase (substring said 0 1)) (substring said 1)))))

(defun lispm-rotom--failed (said)
  "Say that what Rotom was asked to do failed, saying SAID."
  (setq lispm-rotom-message said)
  (message "Rotom: %s" said)
  (lispm-rotom--changed))

(defun lispm-rotom--process (name command done &optional failed input timeout)
  "Run COMMAND, a list, as the process NAME; then call DONE with what it
printed.  When it fails, call FAILED with what it said, by default
`lispm-rotom--failed'.  INPUT, a string, is handed to it on its standard
input, which is closed after.  It is stopped after TIMEOUT seconds,
`lispm-rotom-timeout' by default."
  (let* ((stdout (generate-new-buffer (format " *%s*" name) t))
         (stderr (generate-new-buffer (format " *%s errors*" name) t))
         (left 2)
         (status nil)
         (timer nil)
         (finish
          (lambda ()
            (when (zerop (cl-decf left))
              (when timer (cancel-timer timer))
              (let ((output (with-current-buffer stdout (buffer-string)))
                    (said (lispm-rotom--said (with-current-buffer stderr (buffer-string)))))
                (kill-buffer stdout)
                (kill-buffer stderr)
                (if (eql status 0)
                    (funcall done output)
                  (funcall (or failed #'lispm-rotom--failed)
                           (cond ((eq status 'timeout)
                                  (format "%s took longer than %d seconds" (car command)
                                          (or timeout lispm-rotom-timeout)))
                                 ((string-empty-p said)
                                  (let ((printed (lispm-rotom--said output)))
                                    (if (string-empty-p printed)
                                        (format "%s failed, with status %s" (car command) status)
                                      printed)))
                                 (t said))))))))
         (errors (make-pipe-process :name (concat name " errors") :buffer stderr :noquery t
                                    :coding 'utf-8
                                    :sentinel (lambda (process _event)
                                                (unless (process-live-p process)
                                                  (funcall finish)))))
         (process (make-process :name name :buffer stdout :noquery t
                                :connection-type 'pipe :coding 'utf-8
                                :command command
                                :stderr errors
                                :sentinel (lambda (process _event)
                                            (unless (process-live-p process)
                                              (unless status
                                                (setq status (process-exit-status process)))
                                              (funcall finish))))))
    (setq timer (run-at-time (or timeout lispm-rotom-timeout) nil
                             (lambda ()
                               (when (process-live-p process)
                                 (setq status 'timeout)
                                 (delete-process process)))))
    ;; A program quick to end may have ended already.
    (ignore-error error
      (when input
        (process-send-string process input))
      (process-send-eof process))
    process))

(defun lispm-rotom--run (arguments done &optional failed input)
  "Run rotom with ARGUMENTS, then call DONE with what it printed.
FAILED and INPUT are as `lispm-rotom--process' takes them."
  (lispm-rotom--process "lispm-rotom"
                        (cons (or (lispm-rotom-installed-p) lispm-rotom-program) arguments)
                        done failed input))

;;;; Reading the daemon

(defconst lispm-rotom--status-marker "\n__LISPM_ROTOM_STATUS__:"
  "What curl prints before the HTTP status.")

(defun lispm-rotom--get (path done &optional failed)
  "GET PATH of Rotom's daemon; call DONE with the JSON it answers, objects
as plists, or FAILED with why not.  Rotom's key, when it has one, goes to
curl on its standard input."
  (let ((key (lispm-rotom-api-key)))
    (lispm-rotom--process
     "lispm-rotom-http"
     (list lispm-rotom-curl-program "--silent" "--show-error" "--max-time" "10"
           "--noproxy" "*" "--config" "-"
           "--write-out" (concat lispm-rotom--status-marker "%{http_code}")
           (concat (lispm-rotom-url) path))
     (lambda (output)
       (let* ((at (string-search lispm-rotom--status-marker output))
              (body (if at (substring output 0 at) output))
              (code (and at (string-to-number (substring output (+ at (length lispm-rotom--status-marker))))))
              (json (ignore-errors
                      (json-parse-string body :object-type 'plist :array-type 'list
                                         :null-object nil :false-object nil))))
         (cond ((and code (<= 200 code 299)) (funcall done json))
               (t (funcall (or failed #'ignore)
                           (or (and (plistp json)
                                    (let ((error (plist-get json :error)))
                                      (if (plistp error) (plist-get error :message) error)))
                               (format "HTTP %s" code)))))))
     (or failed #'ignore)
     (if key
         (format "header = \"authorization: Bearer %s\"\n"
                 (replace-regexp-in-string "[\"\\\\]" "\\\\\\&" key))
       "")
     15)))

;;;; What rotom prints

(defun lispm-rotom--duration (text)
  "Return the seconds TEXT says, as \"7d 00h 18m 57s\", or nil."
  (let ((seconds 0) (found nil) (start 0))
    (while (string-match "\\([0-9]+\\)\\([dhms]\\)" text start)
      (setq found t
            start (match-end 0)
            seconds (+ seconds (* (string-to-number (match-string 1 text))
                                  (pcase (match-string 2 text)
                                    ("d" 86400) ("h" 3600) ("m" 60) (_ 1))))))
    (and found seconds)))

(defun lispm-rotom-parse-status (text)
  "Return what TEXT, the output of `rotom status', says: a plist of
:version, :accounts, as `lispm-rotom-accounts' holds them, and :daemon,
where it runs, or nil."
  (let ((version nil) (accounts nil) (account nil) (daemon nil) (highlights nil))
    (dolist (line (split-string text "\n"))
      (cond
       ((string-match "\\`rotom: \\(.+\\)" line)
        (setq version (string-trim (match-string 1 line))))
       ((string-match "\\`provider: \\(.+\\)" line)
        (when account (push account accounts))
        (setq account (list :provider (intern (string-trim (match-string 1 line))))
              highlights nil))
       ((and account (string-match "\\`token: \\(.+\\)" line))
        (let* ((token (string-trim (replace-regexp-in-string " (.* credentials)\\'" ""
                                                             (match-string 1 line))))
               (expires (cond ((string-match-p "does not expire" token) nil)
                              ((string-match-p "\\`token expired" token) 0)
                              ((string-match "expires in \\(.+\\)" token)
                               (lispm-rotom--duration (match-string 1 token))))))
          (setq account (plist-put (plist-put account :token token) :expires expires))))
       ((and account (string-match "\\`status: \\(.+\\)" line))
        (setq account (plist-put account :status
                                 (intern (replace-regexp-in-string " " "-" (string-trim (match-string 1 line)))))))
       ((and account (string-match "\\`  \\([^ ].*\\)" line) (not daemon))
        (setq highlights (append highlights (list (string-trim (match-string 1 line)))))
        (setq account (plist-put account :highlights highlights)))
       ((string-match "\\`daemon: running at \\(.+\\)" line)
        (when account (push account accounts) (setq account nil))
        (setq daemon (string-trim (match-string 1 line))))))
    (when account (push account accounts))
    (list :version version :accounts (nreverse accounts) :daemon daemon)))

(defun lispm-rotom-parse-models (text)
  "Return the models TEXT, the output of `rotom models', lists by account:
((PROVIDER . MODELS) ...)."
  (let ((models nil) (provider nil))
    (dolist (line (split-string text "\n"))
      (cond ((string-match "\\`[^ ].*(\\([a-z]+\\))[ \t]*\\'" line)
             (setq provider (intern (match-string 1 line)))
             (push (list provider) models))
            ((and provider (string-match "\\`[ \t]+\\([^ \t].*?\\)[ \t]*\\'" line))
             (setcdr (assq provider models)
                     (append (cdr (assq provider models)) (list (match-string 1 line)))))))
    (nreverse models)))

(defun lispm-rotom-parse-daemon (text)
  "Return what TEXT, the output of `rotom daemon status', says: a plist of
:installed and :loaded."
  (let ((text (replace-regexp-in-string "\e\\[[0-9;]*m" "" text)))
    (list :installed (and (string-match "^ *Installed +\\(yes\\|no\\)" text)
                          (equal (match-string 1 text) "yes"))
          :loaded (and (string-match "^ *Loaded +\\(yes\\|no\\)" text)
                       (equal (match-string 1 text) "yes")))))

;;;; Reading Rotom

(defvar lispm-rotom--reading nil
  "The functions waiting for Rotom being read: non-nil while it is.")

(defun lispm-rotom-read (&optional callback)
  "Read where Rotom stands anew, in the background; then call CALLBACK.
Its program says what accounts are signed in and the models they offer,
its daemon whether it runs and what the account it serves first allows."
  (interactive)
  (if lispm-rotom--reading
      (when callback (nconc lispm-rotom--reading (list callback)))
    (setq lispm-rotom--reading (list (or callback #'ignore))
          lispm-rotom-config (lispm-rotom--read-config)
          lispm-rotom-message nil)
    (if (not (lispm-rotom-installed-p))
        (lispm-rotom--read-done 'missing)
      (unless (memq lispm-rotom-state '(ready no-account))
        (setq lispm-rotom-state 'reading)
        (lispm-rotom--changed))
      (let* ((left 4)
             (state 'ready)
             (running nil)
             (installed nil)
             (served nil)
             (finish (lambda ()
                       (when (zerop (cl-decf left))
                         (setq lispm-rotom-daemon (list :running running :url (lispm-rotom-url)
                                                        :installed installed)
                               lispm-rotom-served served)
                         (lispm-rotom--read-done state)))))
        (lispm-rotom--run '("status")
                          (lambda (output)
                            (let ((status (lispm-rotom-parse-status output)))
                              (setq lispm-rotom-version (plist-get status :version)
                                    lispm-rotom-accounts (plist-get status :accounts)))
                            (funcall finish))
                          (lambda (said)
                            (setq lispm-rotom-accounts nil)
                            (if (string-match-p "not logged in" said)
                                (setq state 'no-account)
                              (setq lispm-rotom-message said))
                            (funcall finish)))
        (lispm-rotom--run '("models")
                          (lambda (output)
                            (setq lispm-rotom-models (lispm-rotom-parse-models output))
                            (funcall finish))
                          (lambda (said)
                            (setq lispm-rotom-message (or lispm-rotom-message said))
                            (funcall finish)))
        (lispm-rotom--run '("daemon" "status")
                          (lambda (output)
                            (setq installed (plist-get (lispm-rotom-parse-daemon output) :installed))
                            (funcall finish))
                          (lambda (_said) (funcall finish)))
        (lispm-rotom--get "/health"
                          (lambda (_json)
                            (setq running t)
                            (lispm-rotom--get "/v1/status"
                                              (lambda (json) (setq served json) (funcall finish))
                                              (lambda (_why) (funcall finish))))
                          (lambda (_why) (funcall finish)))))))

(defun lispm-rotom--read-done (state)
  "Say that Rotom was read, standing at STATE; call those waiting."
  (unless (lispm-rotom-installed-p)
    (setq state 'missing))
  (setq lispm-rotom-state state
        lispm-rotom-read-at (current-time))
  (let ((waiting lispm-rotom--reading))
    (setq lispm-rotom--reading nil)
    (lispm-rotom--changed)
    (mapc #'funcall waiting)))

;;;; The daemon

(defun lispm-rotom-running-p ()
  "Return non-nil when Rotom's daemon ran when Rotom was last read."
  (plist-get lispm-rotom-daemon :running))

(defvar lispm-rotom-busy nil
  "What Rotom is doing for the window, a sentence, or nil.")

(defun lispm-rotom--busy (text)
  "Say that Rotom does TEXT, or nothing for nil."
  (setq lispm-rotom-busy text)
  (lispm-rotom--changed))

(defun lispm-rotom--wait-for-daemon (running then &optional tries)
  "Read Rotom until its daemon runs, or no longer when RUNNING is nil, for
TRIES more times; then call THEN."
  (let ((tries (or tries 10)))
    (lispm-rotom--get "/health"
                      (lambda (_json)
                        (if (or running (<= tries 0))
                            (funcall then)
                          (run-at-time 0.5 nil #'lispm-rotom--wait-for-daemon running then (1- tries))))
                      (lambda (_why)
                        (if (or (not running) (<= tries 0))
                            (funcall then)
                          (run-at-time 0.5 nil #'lispm-rotom--wait-for-daemon running then (1- tries)))))))

(defun lispm-rotom--daemon (steps doing running &optional callback)
  "Run rotom's daemon STEPS, lists of arguments, one after another, saying
DOING meanwhile; then wait for the daemon to run, or to stop when RUNNING
is nil, read Rotom anew and call CALLBACK with nil, or with why it failed."
  (lispm-rotom--busy doing)
  (cl-labels ((fail (said)
                (lispm-rotom--busy nil)
                (lispm-rotom-read (lambda ()
                                    (lispm-rotom--failed said)
                                    (when callback (funcall callback said)))))
              (next (steps)
                (if steps
                    (lispm-rotom--run (car steps) (lambda (_output) (next (cdr steps))) #'fail)
                  (lispm-rotom--wait-for-daemon
                   running
                   (lambda ()
                     (lispm-rotom-read (lambda ()
                                         (lispm-rotom--busy nil)
                                         (when callback (funcall callback nil)))))))))
    (next steps)))

(defun lispm-rotom-start (&optional callback)
  "Start Rotom's daemon, made a service of the system first if it is not.
CALLBACK is called after, with nil or why it failed."
  (interactive)
  (cond ((not (lispm-rotom-installed-p))
         (if callback
             (funcall callback "Rotom is not installed")
           (user-error "Rotom is not installed")))
        ;; Whether it is a service yet is known once Rotom was read.
        ((null lispm-rotom-daemon)
         (lispm-rotom-read (lambda () (lispm-rotom-start callback))))
        (t
         (lispm-rotom--daemon (if (plist-get lispm-rotom-daemon :installed)
                                  '(("daemon" "start"))
                                '(("daemon" "install") ("daemon" "start")))
                              "Starting the daemon…" t callback))))

(defun lispm-rotom-stop (&optional callback)
  "Stop Rotom's daemon.  CALLBACK is called after, with nil or why it failed.
Programs using it, LispM's agents among them, then reach no model."
  (interactive)
  (lispm-rotom--daemon '(("daemon" "stop")) "Stopping the daemon…" nil callback))

(defun lispm-rotom-restart (&optional callback)
  "Restart Rotom's daemon, as after an account was signed in to.
CALLBACK is called after, with nil or why it failed."
  (interactive)
  (lispm-rotom--daemon '(("daemon" "restart")) "Restarting the daemon…" t callback))

(defun lispm-rotom-refresh-tokens (&optional provider callback)
  "Have Rotom renew the tokens of the accounts signed in to it, or of
PROVIDER's only.  CALLBACK is called after, with what Rotom said, and
non-nil as second argument when it failed."
  (interactive)
  (lispm-rotom--busy "Refreshing tokens…")
  (lispm-rotom--run (append '("refresh") (and provider (list "--provider" (symbol-name provider))))
                    (lambda (output)
                      (lispm-rotom-read (lambda ()
                                          (lispm-rotom--busy nil)
                                          (when callback (funcall callback (string-trim output))))))
                    (lambda (said)
                      (lispm-rotom-read (lambda ()
                                          (lispm-rotom--busy nil)
                                          (lispm-rotom--failed said)
                                          (when callback (funcall callback said t)))))))

;;;; Signing in

(cl-defstruct (lispm-rotom-sign-in (:constructor lispm-rotom-sign-in--create)
                                   (:copier nil))
  "Signing in to an account of Rotom."
  provider process (output "") url state servers asked)

(defvar lispm-rotom-signing-in nil
  "The account being signed in to, a `lispm-rotom-sign-in', or nil.")

(defun lispm-rotom--redirect (url)
  "Return the address URL, the page signing in, sends the browser back to:
(HOST PORT), or nil."
  (when-let* ((query (cdr (url-path-and-query (url-generic-parse-url url))))
              (redirect (cadr (assoc "redirect_uri" (url-parse-query-string query))))
              (parsed (url-generic-parse-url redirect))
              (port (url-port parsed)))
    (list (url-host parsed) port)))

(defconst lispm-rotom--landed-page
  "<!doctype html><meta charset=\"utf-8\"><title>Rotom</title>
<body style=\"font: 15px -apple-system, sans-serif; margin: 4em; text-align: center\">
<h1 style=\"font-size: 20px\">Signed in</h1>
<p>Rotom has what it needs.  You may close this page and go back to Emacs.</p>"
  "The page the browser shows once it brought the address back to Emacs.")

(defun lispm-rotom--serve-landing (sign-in host port)
  "Serve HOST's PORT for SIGN-IN while it waits: the address the browser
lands on, which goes to Rotom.  Return non-nil when Emacs listens."
  (dolist (family (if (member host '("localhost" "::1" "[::1]")) '(ipv4 ipv6) '(ipv4)))
    (when-let* ((server
                 (ignore-errors
                   (make-network-process
                    :name "lispm-rotom-landing" :server t :noquery t
                    :host (if (eq family 'ipv6) "::1" "127.0.0.1") :service port :family family
                    :coding 'binary
                    :filter (lambda (connection text)
                              (lispm-rotom--landing-request sign-in host port connection text))))))
      (push server (lispm-rotom-sign-in-servers sign-in))))
  (lispm-rotom-sign-in-servers sign-in))

(defun lispm-rotom--landing-request (sign-in host port connection text)
  "Answer CONNECTION, which sent TEXT, at HOST's PORT for SIGN-IN.
The address the browser asks for, carrying the account's code, goes to
Rotom; other requests, as for an icon, are not found."
  (let ((request (concat (or (process-get connection 'request) "") text)))
    (process-put connection 'request request)
    (when (string-match "\\`GET \\([^ ]+\\) HTTP/[0-9.]+\r?\n\\(?:.*\r?\n\\)*?\r?\n" request)
      (let* ((target (match-string 1 request))
             (landed (string-match-p "[?&]\\(?:code\\|error\\)=" target))
             (body (if landed lispm-rotom--landed-page "Not found")))
        (process-send-string
         connection
         (format "HTTP/1.1 %s\r\nContent-Type: text/html; charset=utf-8\r\nContent-Length: %d\r\nConnection: close\r\n\r\n%s"
                 (if landed "200 OK" "404 Not Found") (string-bytes body) body))
        (delete-process connection)
        (when landed
          (lispm-rotom--answer sign-in (format "http://%s:%s%s" host port target)))))))

(defun lispm-rotom--answer (sign-in address)
  "Give Rotom ADDRESS, the one the browser landed on, for SIGN-IN."
  (when (and (process-live-p (lispm-rotom-sign-in-process sign-in))
             (not (eq (lispm-rotom-sign-in-state sign-in) 'answered)))
    (setf (lispm-rotom-sign-in-state sign-in) 'answered)
    (lispm-rotom--stop-serving sign-in)
    (process-send-string (lispm-rotom-sign-in-process sign-in) (concat address "\n"))
    (lispm-rotom--busy (format "Signing in to %s…"
                               (lispm-rotom-provider-name (lispm-rotom-sign-in-provider sign-in))))))

(defun lispm-rotom--stop-serving (sign-in)
  "Stop serving the address SIGN-IN's browser lands on."
  (mapc #'delete-process (lispm-rotom-sign-in-servers sign-in))
  (setf (lispm-rotom-sign-in-servers sign-in) nil))

(defun lispm-rotom-paste-address (&optional address)
  "Give Rotom ADDRESS, the one the browser landed on once signed in.
Asked in the minibuffer, for when the browser did not come back to Emacs
by itself."
  (interactive)
  (let ((sign-in lispm-rotom-signing-in))
    (unless (and sign-in (process-live-p (lispm-rotom-sign-in-process sign-in)))
      (user-error "No account is being signed in to"))
    (let ((address (or address
                       (read-string (format "Address your browser shows, once signed in to %s: "
                                            (lispm-rotom-provider-name
                                             (lispm-rotom-sign-in-provider sign-in)))))))
      (when (string-blank-p address)
        (user-error "No address given"))
      (lispm-rotom--answer sign-in (string-trim address)))))

(defun lispm-rotom-cancel-sign-in ()
  "Stop signing in to an account."
  (interactive)
  (when-let* ((sign-in lispm-rotom-signing-in))
    (setf (lispm-rotom-sign-in-state sign-in) 'cancelled)
    (lispm-rotom--stop-serving sign-in)
    (when (process-live-p (lispm-rotom-sign-in-process sign-in))
      (delete-process (lispm-rotom-sign-in-process sign-in)))
    (setq lispm-rotom-signing-in nil)
    (lispm-rotom--busy nil)))

(defun lispm-rotom--sign-in-output (sign-in text)
  "Note TEXT, which rotom printed while SIGN-IN: open the page signing in
in the browser, and serve the address it lands on, or ask for it."
  (setf (lispm-rotom-sign-in-output sign-in) (concat (lispm-rotom-sign-in-output sign-in) text))
  (let ((output (lispm-rotom-sign-in-output sign-in)))
    (when (and (null (lispm-rotom-sign-in-url sign-in))
               (string-match "Open this URL[^\n]*:\n\\(https?://[^ \t\n]+\\)" output))
      (let ((url (match-string 1 output)))
        (setf (lispm-rotom-sign-in-url sign-in) url
              (lispm-rotom-sign-in-state sign-in) 'browser)
        (pcase (lispm-rotom--redirect url)
          (`(,host ,port) (lispm-rotom--serve-landing sign-in host port)))
        (lispm-rotom--busy (format "Signing in to %s in your browser…"
                                   (lispm-rotom-provider-name (lispm-rotom-sign-in-provider sign-in))))
        (funcall lispm-rotom-browse-function url)))
    ;; Without a server of its own, the address is asked, once.
    (when (and (string-match-p "Paste the full [^\n]*: *\\'" output)
               (not (lispm-rotom-sign-in-servers sign-in))
               (not (lispm-rotom-sign-in-asked sign-in))
               (eq (lispm-rotom-sign-in-state sign-in) 'browser))
      (setf (lispm-rotom-sign-in-asked sign-in) t)
      (run-at-time 0 nil (lambda ()
                           (when (eq lispm-rotom-signing-in sign-in)
                             (condition-case nil
                                 (lispm-rotom-paste-address)
                               (quit (lispm-rotom-cancel-sign-in)))))))))

(defun lispm-rotom--sign-in-ended (sign-in process callback)
  "End SIGN-IN, whose rotom PROCESS ended; call CALLBACK with nil, or with
why it failed.  A daemon running restarts to serve an account new to it."
  (lispm-rotom--stop-serving sign-in)
  (when (eq lispm-rotom-signing-in sign-in)
    (setq lispm-rotom-signing-in nil))
  (let* ((output (lispm-rotom-sign-in-output sign-in))
         (name (lispm-rotom-provider-name (lispm-rotom-sign-in-provider sign-in)))
         (ok (eql (process-exit-status process) 0))
         (why (unless ok
                (let ((lines (split-string (lispm-rotom--said output) "\n" t "[ \t\r]+")))
                  (or (car (last (seq-filter (lambda (line) (string-match-p "\\`[Ee]rror\\|failed\\|mismatch" line))
                                             lines)))
                      (car (last lines))
                      "Rotom could not sign in")))))
    (cond
     ((eq (lispm-rotom-sign-in-state sign-in) 'cancelled)
      (lispm-rotom--busy nil)
      (when callback (funcall callback "Cancelled")))
     ((not ok)
      (lispm-rotom--busy nil)
      (lispm-rotom--failed why)
      (when callback (funcall callback why)))
     ((and (string-match-p "rotom daemon restart" output) (lispm-rotom-running-p))
      (message "Rotom: signed in to %s" name)
      (lispm-rotom-restart callback))
     (t
      (message "Rotom: signed in to %s" name)
      (lispm-rotom-read (lambda ()
                          (lispm-rotom--busy nil)
                          (when callback (funcall callback nil))))))))

(defun lispm-rotom-sign-in (provider &optional callback)
  "Sign in to PROVIDER's account, in the browser, or with its key.
CALLBACK is called after, with nil, or with why it failed."
  (interactive
   (list (intern (completing-read "Sign in to: "
                                  (mapcar (lambda (entry) (symbol-name (car entry))) lispm-rotom-providers)
                                  nil t))))
  (unless (lispm-rotom-installed-p)
    (user-error "Rotom is not installed"))
  (when (and lispm-rotom-signing-in
             (process-live-p (lispm-rotom-sign-in-process lispm-rotom-signing-in)))
    (user-error "%s is being signed in to"
                (lispm-rotom-provider-name (lispm-rotom-sign-in-provider lispm-rotom-signing-in))))
  (let ((properties (alist-get provider lispm-rotom-providers)))
    (unless properties
      (user-error "Rotom serves no account of %s" provider))
    (if (eq (plist-get properties :sign-in) 'key)
        (let ((key (read-passwd (format "%s key: " (plist-get properties :name)))))
          (when (string-empty-p key)
            (user-error "No key given"))
          (lispm-rotom--busy (format "Signing in to %s…" (plist-get properties :name)))
          (let ((input (concat key "\n")))
            (clear-string key)
            (lispm-rotom--run (list "login" "--provider" (symbol-name provider))
                              (lambda (_output)
                                (lispm-rotom-read (lambda ()
                                                    (lispm-rotom--busy nil)
                                                    (when callback (funcall callback nil)))))
                              (lambda (said)
                                (lispm-rotom--busy nil)
                                (lispm-rotom--failed said)
                                (when callback (funcall callback said)))
                              input)
            (clear-string input)))
      (let* ((sign-in (lispm-rotom-sign-in--create :provider provider :state 'starting))
             (process (make-process
                       :name "lispm-rotom-sign-in" :noquery t :connection-type 'pipe :coding 'utf-8
                       :command (list (lispm-rotom-installed-p) "login" "--provider" (symbol-name provider))
                       :filter (lambda (_process text) (lispm-rotom--sign-in-output sign-in text))
                       :sentinel (lambda (process _event)
                                   (unless (process-live-p process)
                                     (lispm-rotom--sign-in-ended sign-in process callback))))))
        (setf (lispm-rotom-sign-in-process sign-in) process)
        (setq lispm-rotom-signing-in sign-in)
        (lispm-rotom--busy (format "Signing in to %s…" (plist-get properties :name)))
        sign-in))))

;;;; LispM's agents

(defun lispm-rotom-backends ()
  "Return LispM's backends for Rotom: its Chat Completions side, `rotom',
and its Messages side, `rotom-messages', as `lispm-backends' holds them."
  (let ((url (lispm-rotom-url))
        (model (lispm-rotom-fallback-model))
        (key (and (lispm-rotom-api-key) #'lispm-rotom-api-key)))
    `((rotom :api openai :url ,(concat url "/v1") :model ,model :key ,key
             :features (streaming reasoning-effort))
      (rotom-messages :api anthropic :url ,url :model ,model :key ,key
                      :features (streaming tool-errors)))))

;;;###autoload
(defun lispm-rotom-add-backends ()
  "Add Rotom's two sides to `lispm-backends', those not there already.
Which backend LispM's agents use does not change."
  (when (boundp 'lispm-backends)
    (dolist (backend (lispm-rotom-backends))
      (unless (assq (car backend) lispm-backends)
        (setq lispm-backends (append lispm-backends (list backend)))))))

;;;###autoload
(with-eval-after-load 'lispm-core
  (lispm-rotom-add-backends))

(defun lispm-rotom-lispm ()
  "Return what LispM's agents use, for Rotom: nil without LispM, else a
plist of :backend, the backend they use by default, :rotom, non-nil when
it is Rotom's, :model, the model, and :listed, non-nil when Rotom offers
it through an account signed in to."
  (when (and (featurep 'lispm-core) (fboundp 'lispm-backend-name))
    (let* ((backend (lispm-backend-name))
           (rotom (and (memq backend '(rotom rotom-messages)) t))
           (model (or (and (boundp 'lispm-model) lispm-model)
                      (plist-get (alist-get backend lispm-backends) :model))))
      (list :backend backend :rotom rotom :model model
            :listed (and rotom model (member model (lispm-rotom-model-ids)) t)))))

(defun lispm-rotom--read-model (prompt)
  "Read a model Rotom offers, with PROMPT."
  (let* ((lispm (lispm-rotom-lispm))
         (default (or (and (plist-get lispm :rotom) (plist-get lispm :model))
                      (lispm-rotom-fallback-model))))
    (completing-read (format-prompt prompt default) (lispm-rotom-model-ids)
                     nil nil nil nil default)))

(defun lispm-rotom-use (model &optional messages)
  "Have LispM's agents use MODEL through Rotom, from now on.
They use its Chat Completions side, or with MESSAGES, its Messages side.
It is saved with Customize, as `lispm-backend' and `lispm-model'.  An
agent's conversation already open goes on with its model; its /model
switches it."
  (interactive (list (lispm-rotom--read-model "LispM's agents use") current-prefix-arg))
  (unless (require 'lispm-core nil t)
    (user-error "LispM Agent is not installed"))
  (when (string-blank-p model)
    (user-error "No model given"))
  (lispm-rotom-add-backends)
  (customize-save-variable 'lispm-backend (if messages 'rotom-messages 'rotom))
  (customize-save-variable 'lispm-model model)
  (lispm-rotom--changed)
  (message "LispM's agents now use %s through Rotom" model))

(defun lispm-rotom-stop-using ()
  "Have LispM's agents no longer use Rotom: LispM chooses their backend
again, as `lispm-backend' says for nil."
  (interactive)
  (unless (plist-get (lispm-rotom-lispm) :rotom)
    (user-error "LispM's agents do not use Rotom"))
  (customize-save-variable 'lispm-backend nil)
  (customize-save-variable 'lispm-model nil)
  (lispm-rotom--changed)
  (message "LispM's agents no longer use Rotom"))

;;;; Saying where Rotom stands

(defun lispm-rotom-duration-text (seconds)
  "Return SECONDS as a few words: \"7 days\", \"3 hours\", \"12 minutes\"."
  (cond ((null seconds) "no expiry")
        ((<= seconds 0) "expired")
        ((>= seconds 86400) (let ((n (/ seconds 86400))) (format "%d day%s" n (if (= n 1) "" "s"))))
        ((>= seconds 3600) (let ((n (/ seconds 3600))) (format "%d hour%s" n (if (= n 1) "" "s"))))
        (t (let ((n (max 1 (/ seconds 60)))) (format "%d minute%s" n (if (= n 1) "" "s"))))))

(defun lispm-rotom-account-text (account)
  "Return what is said of ACCOUNT, signed in to Rotom, in a few words."
  (let ((expires (plist-get account :expires)))
    (cond ((eq (plist-get account :status) 'refresh-failed) "its token could not be renewed")
          ((and expires (<= expires 0)) "its token expired")
          (expires (format "token for %s" (lispm-rotom-duration-text expires)))
          (t "a key, no expiry"))))

(defun lispm-rotom-limits-text ()
  "Return what the account Rotom serves first allows, or nil: its credits
and its rate limits."
  (when-let* ((served lispm-rotom-served))
    (let ((parts nil))
      (when-let* ((credits (plist-get served :credits_balance)))
        (push (format "credits %s" (if (floatp credits) (format "%.0f" credits) credits)) parts))
      (dolist (limit (plist-get served :rate_limits))
        (push (format "%s limit %s%% left%s"
                      (plist-get limit :name)
                      (let ((left (plist-get limit :remaining_percent)))
                        (if (numberp left) (format "%.0f" left) "?"))
                      (if-let* ((reset (plist-get limit :reset_at_local)))
                          (format ", until %s" (substring reset 0 (min 16 (length reset))))
                        ""))
              parts))
      (and parts (string-join (nreverse parts) "; ")))))

(defun lispm-rotom-report ()
  "Return where Rotom stands, as text, for an agent or a buffer."
  (pcase lispm-rotom-state
    ('missing (format "Rotom is not installed%s."
                      (if lispm-rotom-install-command
                          (format "; it is installed with %s" lispm-rotom-install-command)
                        "")))
    (_
     (let ((lispm (lispm-rotom-lispm)))
       (string-join
        (delq nil
              (list
               (format "Rotom %s.  Its daemon %s."
                       (or lispm-rotom-version "")
                       (cond ((lispm-rotom-running-p) (format "runs at %s" (plist-get lispm-rotom-daemon :url)))
                             ((plist-get lispm-rotom-daemon :installed) "is installed as a service but is not running")
                             (t "is not running")))
               (and (lispm-rotom-running-p)
                    (format "Chat Completions and Responses at %s/v1; Messages at %s."
                            (plist-get lispm-rotom-daemon :url) (plist-get lispm-rotom-daemon :url)))
               (and (lispm-rotom-api-key) "It asks for its own key (its configuration's api_key).")
               ""
               "Accounts:"
               (mapconcat (pcase-lambda (`(,provider . ,properties))
                            (let ((account (lispm-rotom-account provider)))
                              (format "- %s (%s): %s%s"
                                      (plist-get properties :name) provider
                                      (if account (concat "signed in, " (lispm-rotom-account-text account))
                                        "not signed in")
                                      (if (and account (equal (symbol-name provider)
                                                              (plist-get lispm-rotom-served :provider)))
                                          (if-let* ((limits (lispm-rotom-limits-text)))
                                              (format "; %s" limits)
                                            "")
                                        ""))))
                          lispm-rotom-providers "\n")
               (and (plist-get lispm-rotom-served :warnings)
                    (concat "Warnings of the account served first: "
                            (string-join (mapcar (lambda (warning)
                                                   (truncate-string-to-width
                                                    (replace-regexp-in-string "[\n\r]+" " " warning) 160 nil nil "…"))
                                                 (plist-get lispm-rotom-served :warnings))
                                         "; ")))
               (format "Models offered: %d, see rotom_models.  Unknown Anthropic model ids fall back to %s."
                       (length (lispm-rotom-model-ids)) (lispm-rotom-fallback-model))
               (cond ((null lispm) nil)
                     ((plist-get lispm :rotom)
                      (format "LispM's agents use %s through Rotom (backend %s)%s."
                              (plist-get lispm :model) (plist-get lispm :backend)
                              (if (plist-get lispm :listed) "" ", a model Rotom does not offer through an account signed in to")))
                     (t (format "LispM's agents use the backend %s, not Rotom." (or (plist-get lispm :backend) "none"))))
               (and lispm-rotom-message (format "Rotom last said: %s" lispm-rotom-message))))
        "\n")))))

(defun lispm-rotom-models-report (&optional provider)
  "Return the models Rotom offers, by account, as text; of PROVIDER only
when given."
  (let ((entries (if provider
                     (list (assq provider lispm-rotom-models))
                   lispm-rotom-models)))
    (if (not (car entries))
        (format "Rotom lists no models%s." (if provider (format " of %s" provider) ""))
      (mapconcat (pcase-lambda (`(,provider . ,models))
                   (format "%s (%s), %s:\n%s"
                           (lispm-rotom-provider-name provider) provider
                           (if (lispm-rotom-signed-in-p provider) "signed in" "not signed in, so not served")
                           (mapconcat (lambda (model) (concat "- " model)) models "\n")))
                 entries "\n\n"))))

(defun lispm-rotom-log-tail (&optional lines)
  "Return the last LINES lines, 40 by default, Rotom's daemon wrote to its
log of errors."
  (let ((file (lispm-rotom-log-file))
        (lines (or lines 40)))
    (if (not (file-readable-p file))
        (format "Rotom's daemon has written no log at %s." (abbreviate-file-name file))
      (with-temp-buffer
        (let ((size (file-attribute-size (file-attributes file))))
          (insert-file-contents file nil (max 0 (- size (* lines 400))) size))
        (goto-char (point-max))
        (skip-chars-backward " \t\n")
        (forward-line (- 1 lines))
        (string-trim (buffer-substring-no-properties (line-beginning-position) (point-max)))))))

(provide 'lispm-rotom)
;;; lispm-rotom.el ends here
