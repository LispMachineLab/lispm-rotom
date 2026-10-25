;;; lispm-rotom-test.el --- Tests of lispm-rotom  -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Ryan.K

;; Author: Ryan.K
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

;; The tests run test/fake-rotom in place of rotom and test/fake-curl in
;; place of curl: they answer as Rotom does from a directory of their own,
;; so no daemon is started and no account signed in to.  With LispM
;; Agent's lisp/ on the load path, the tests of its backends run too.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'lispm-rotom)
(require 'lispm-rotom-ui)

(defvar lispm-backends)
(defvar lispm-backend)
(defvar lispm-model)

(defconst lispm-rotom-test-directory
  (file-name-directory (or load-file-name buffer-file-name))
  "The directory of the tests and their fake programs.")

(defconst lispm-rotom-test-status
  "rotom: 1.5.16

provider: codex
token: token expires in 7d 00h 18m 57s (Codex credentials)
status: authenticated
highlight_models:
  gpt-6-astra
  gpt-6.1-sol

provider: grok
token: token expires in 3h 12m 57s (Grok credentials)
status: authenticated
highlight_models:
  grok-4.7

provider: vercel
token: token does not expire automatically (Vercel credentials)
status: authenticated
highlight_models:
  openai/gpt-6-astra

daemon: running at http://127.0.0.1:14550
endpoints:
  GET        http://127.0.0.1:14550/health
  POST       http://127.0.0.1:14550/v1/chat/completions
"
  "What rotom 1.5.16 prints for `rotom status'.")

(defmacro lispm-rotom-test-with-fake (&rest body)
  "Run BODY with the fake rotom and curl, in a directory of their own,
bound to `dir', and Rotom's state forgotten before and after."
  (declare (indent 0) (debug t))
  `(let* ((dir (make-temp-file "lispm-rotom-test-" t))
          (home (expand-file-name "home" dir))
          (process-environment (cons (concat "FAKE_ROTOM_DIR=" dir) process-environment))
          (lispm-rotom-program (expand-file-name "fake-rotom" lispm-rotom-test-directory))
          (lispm-rotom-curl-program (expand-file-name "fake-curl" lispm-rotom-test-directory))
          (lispm-rotom-directory home)
          (lispm-rotom-timeout 10)
          (lispm-rotom-changed-hook nil)
          (lispm-rotom-state nil) (lispm-rotom-version nil) (lispm-rotom-accounts nil)
          (lispm-rotom-models nil) (lispm-rotom-daemon nil) (lispm-rotom-served nil)
          (lispm-rotom-config nil) (lispm-rotom-message nil) (lispm-rotom-busy nil)
          (lispm-rotom-signing-in nil) (lispm-rotom--reading nil))
     (make-directory home)
     (unwind-protect
         (progn ,@body)
       (delete-directory dir t))))

(defun lispm-rotom-test-file (dir name &optional text)
  "Write TEXT, or nothing, to NAME in DIR."
  (with-temp-file (expand-file-name name dir)
    (when text (insert text))))

(defun lispm-rotom-test-read-file (dir name)
  "Return what NAME in DIR holds, or nil."
  (let ((file (expand-file-name name dir)))
    (and (file-exists-p file)
         (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defun lispm-rotom-test-wait (predicate &optional seconds)
  "Wait for PREDICATE to return non-nil, at most SECONDS, 10 by default."
  (let ((until (+ (float-time) (or seconds 10))))
    (while (and (not (funcall predicate)) (< (float-time) until))
      (accept-process-output nil 0.05))
    (funcall predicate)))

(defun lispm-rotom-test-read ()
  "Read Rotom from the fakes, and wait for it."
  (let ((done nil))
    (lispm-rotom-read (lambda () (setq done t)))
    (should (lispm-rotom-test-wait (lambda () done)))))

;;;; What rotom prints

(ert-deftest lispm-rotom-test-parse-status ()
  (let* ((status (lispm-rotom-parse-status lispm-rotom-test-status))
         (accounts (plist-get status :accounts)))
    (should (equal (plist-get status :version) "1.5.16"))
    (should (equal (plist-get status :daemon) "http://127.0.0.1:14550"))
    (should (equal (mapcar (lambda (account) (plist-get account :provider)) accounts)
                   '(codex grok vercel)))
    (let ((codex (nth 0 accounts)))
      (should (eq (plist-get codex :status) 'authenticated))
      (should (= (plist-get codex :expires) (+ (* 7 86400) (* 18 60) 57)))
      (should (equal (plist-get codex :highlights) '("gpt-6-astra" "gpt-6.1-sol"))))
    (should (= (plist-get (nth 1 accounts) :expires) (+ (* 3 3600) (* 12 60) 57)))
    ;; A key does not expire; the endpoints are no highlights.
    (should (null (plist-get (nth 2 accounts) :expires)))
    (should (equal (plist-get (nth 2 accounts) :highlights) '("openai/gpt-6-astra")))))

(ert-deftest lispm-rotom-test-parse-status-failed ()
  (let ((account (car (plist-get (lispm-rotom-parse-status
                                  "rotom: 1\n\nprovider: kiro\ntoken: unavailable\nstatus: refresh failed\nhighlight_models:\n  auto\n")
                                 :accounts))))
    (should (eq (plist-get account :status) 'refresh-failed))
    (should (equal (lispm-rotom-account-text account) "its token could not be renewed"))))

(ert-deftest lispm-rotom-test-parse-models ()
  (should (equal (lispm-rotom-parse-models
                  "OpenAI (codex)\n  gpt-6-astra\n  gpt-6.1-sol\n\nVercel AI Gateway (vercel)\n  openai/gpt-6.1-sol\n")
                 '((codex "gpt-6-astra" "gpt-6.1-sol") (vercel "openai/gpt-6.1-sol")))))

(ert-deftest lispm-rotom-test-parse-daemon ()
  (should (equal (lispm-rotom-parse-daemon " Installed  \e[1myes\e[0m\n    Loaded  yes\n")
                 '(:installed t :loaded t)))
  (should (equal (lispm-rotom-parse-daemon " Installed  no\n    Loaded  no\n")
                 '(:installed nil :loaded nil))))

(ert-deftest lispm-rotom-test-durations ()
  (should (equal (lispm-rotom-duration-text 86400) "1 day"))
  (should (equal (lispm-rotom-duration-text (* 3 3600)) "3 hours"))
  (should (equal (lispm-rotom-duration-text 30) "1 minute"))
  (should (equal (lispm-rotom-duration-text 0) "expired"))
  (should (equal (lispm-rotom-duration-text nil) "no expiry")))

;;;; Reading Rotom

(ert-deftest lispm-rotom-test-read ()
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\ngrok\n")
    (lispm-rotom-test-file dir "installed")
    (lispm-rotom-test-file dir "running")
    (lispm-rotom-test-read)
    (should (eq lispm-rotom-state 'ready))
    (should (equal lispm-rotom-version "9.9.9"))
    (should (lispm-rotom-signed-in-p 'codex))
    (should-not (lispm-rotom-signed-in-p 'kiro))
    (should (lispm-rotom-running-p))
    (should (plist-get lispm-rotom-daemon :installed))
    (should (equal (plist-get lispm-rotom-served :provider) "codex"))
    ;; Only the models of accounts signed in are offered.
    (should (equal (lispm-rotom-model-ids) '("gpt-6-astra" "gpt-6.1-sol" "grok-4.7")))
    (should (member "auto" (lispm-rotom-model-ids t)))
    (should (eq (lispm-rotom-model-provider "grok-4.7") 'grok))
    (should (string-match-p "credits 35511; 5h limit 12% left, until 2026-10-27 15:45"
                            (lispm-rotom-limits-text)))
    ;; Without a key of its own, curl was handed no header.
    (should (equal (lispm-rotom-test-read-file dir "curl-config") ""))))

(ert-deftest lispm-rotom-test-read-key ()
  "Rotom's own key goes to curl on its standard input, not its command line."
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file home "config.json"
                           "{\"bind_host\": \"0.0.0.0\", \"bind_port\": 15550, \"api_key\": \"lo\\\"cal\"}")
    (lispm-rotom-test-file dir "accounts" "codex\n")
    (lispm-rotom-test-file dir "running")
    (lispm-rotom-test-read)
    (should (equal (lispm-rotom-url) "http://127.0.0.1:15550"))
    (should (equal (lispm-rotom-api-key) "lo\"cal"))
    (should (equal (lispm-rotom-test-read-file dir "curl-config")
                   "header = \"authorization: Bearer lo\\\"cal\"\n"))))

(ert-deftest lispm-rotom-test-read-stopped ()
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\n")
    (lispm-rotom-test-read)
    (should (eq lispm-rotom-state 'ready))
    (should-not (lispm-rotom-running-p))
    (should-not (plist-get lispm-rotom-daemon :installed))
    (should (null lispm-rotom-served))
    (should (string-match-p "Its daemon is not running" (lispm-rotom-report)))))

(ert-deftest lispm-rotom-test-read-no-account ()
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-read)
    (should (eq lispm-rotom-state 'no-account))
    (should (null lispm-rotom-accounts))
    (should (null lispm-rotom-message))
    (should (null (lispm-rotom-model-ids)))))

(ert-deftest lispm-rotom-test-read-missing ()
  (lispm-rotom-test-with-fake
    (let ((lispm-rotom-program (expand-file-name "no-such-rotom" dir)))
      (lispm-rotom-test-read)
      (should (eq lispm-rotom-state 'missing))
      (should (string-match-p "not installed; it is installed with cargo install --locked rotom"
                              (lispm-rotom-report))))))

(ert-deftest lispm-rotom-test-read-once ()
  "A read asked for while one runs waits for it, rather than starting another."
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\n")
    (let ((done 0))
      (lispm-rotom-read (lambda () (cl-incf done)))
      (lispm-rotom-read (lambda () (cl-incf done)))
      (should (lispm-rotom-test-wait (lambda () (= done 2))))
      (should (= 1 (cl-count "status" (split-string (lispm-rotom-test-read-file dir "calls") "\n")
                             :test #'equal))))))

;;;; The daemon

(ert-deftest lispm-rotom-test-start-installs ()
  "Starting a daemon that is no service yet makes it one first."
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\n")
    (lispm-rotom-test-read)
    (let ((result 'none))
      (lispm-rotom-start (lambda (why) (setq result why)))
      (should (lispm-rotom-test-wait (lambda () (not (eq result 'none)))))
      (should (null result))
      (should (lispm-rotom-running-p))
      (should (null lispm-rotom-busy))
      (should (string-match-p "^daemon install\ndaemon start$" (lispm-rotom-test-read-file dir "calls"))))))

(ert-deftest lispm-rotom-test-stop ()
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\n")
    (lispm-rotom-test-file dir "installed")
    (lispm-rotom-test-file dir "running")
    (lispm-rotom-test-read)
    (should (lispm-rotom-running-p))
    (let ((result 'none))
      (lispm-rotom-stop (lambda (why) (setq result why)))
      (should (lispm-rotom-test-wait (lambda () (not (eq result 'none)))))
      (should-not (lispm-rotom-running-p)))))

(ert-deftest lispm-rotom-test-refresh-tokens ()
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\n")
    (let ((said nil))
      (lispm-rotom-refresh-tokens 'codex (lambda (text &optional _failed) (setq said text)))
      (should (lispm-rotom-test-wait (lambda () said)))
      (should (equal said "refreshed Codex credentials"))
      (should (string-match-p "^refresh --provider codex$" (lispm-rotom-test-read-file dir "calls"))))))

;;;; Signing in

(defun lispm-rotom-test-port ()
  "Return a port nothing listens on."
  (let* ((server (make-network-process :name "lispm-rotom-test-port" :server t
                                       :host "127.0.0.1" :service t :family 'ipv4))
         (port (process-contact server :service)))
    (delete-process server)
    port))

(defun lispm-rotom-test-get (port target)
  "Ask 127.0.0.1's PORT for TARGET as a browser would; return the answer."
  (let* ((answer "")
         (connection (make-network-process
                      :name "lispm-rotom-test-browser" :host "127.0.0.1" :service port
                      :family 'ipv4 :coding 'binary
                      :filter (lambda (_process text) (setq answer (concat answer text))))))
    (process-send-string connection (format "GET %s HTTP/1.1\r\nHost: 127.0.0.1\r\nAccept: */*\r\n\r\n" target))
    (lispm-rotom-test-wait (lambda () (not (process-live-p connection))) 5)
    answer))

(ert-deftest lispm-rotom-test-redirect ()
  (should (equal (lispm-rotom--redirect
                  "https://auth.openai.com/oauth/authorize?response_type=code&redirect_uri=http%3A%2F%2Flocalhost%3A1455%2Fauth%2Fcallback&state=x")
                 '("localhost" 1455)))
  (should (null (lispm-rotom--redirect "https://example.com/authorize?state=x"))))

(ert-deftest lispm-rotom-test-sign-in-browser ()
  "The browser lands on Emacs, which hands the address to Rotom."
  (lispm-rotom-test-with-fake
    (let* ((port (lispm-rotom-test-port))
           (process-environment (cons (format "FAKE_ROTOM_PORT=%d" port) process-environment))
           (opened nil) (icon nil) (landed nil)
           (result 'none)
           (lispm-rotom-browse-function
            (lambda (url)
              (setq opened url)
              (run-at-time 0.1 nil (lambda ()
                                     (setq icon (lispm-rotom-test-get port "/favicon.ico")
                                           landed (lispm-rotom-test-get port "/callback?code=c0de&state=s7")))))))
      (lispm-rotom-test-file dir "accounts" "codex\n")
      (lispm-rotom-test-file dir "installed")
      (lispm-rotom-test-file dir "running")
      (lispm-rotom-test-read)
      (lispm-rotom-sign-in 'grok (lambda (why) (setq result why)))
      (should (lispm-rotom-test-wait (lambda () (not (eq result 'none)))))
      (should (null result))
      (should (string-prefix-p "https://auth.example.com/authorize?" opened))
      (should (string-prefix-p "HTTP/1.1 404" icon))
      (should (string-prefix-p "HTTP/1.1 200" landed))
      (should (string-match-p "Signed in" landed))
      (should (equal (lispm-rotom-test-read-file dir "answer")
                     (format "http://127.0.0.1:%d/callback?code=c0de&state=s7\n" port)))
      (should (lispm-rotom-signed-in-p 'grok))
      ;; An account new to the daemon running makes it restart.
      (should (string-match-p "^daemon restart$" (lispm-rotom-test-read-file dir "calls")))
      (should (null lispm-rotom-signing-in))
      ;; Emacs serves the port no more.
      (should-not (seq-find (lambda (process) (string-prefix-p "lispm-rotom-landing" (process-name process)))
                            (process-list))))))

(ert-deftest lispm-rotom-test-sign-in-port-taken ()
  "Where another program holds the port, the address is asked for."
  (lispm-rotom-test-with-fake
    (let* ((holder (make-network-process :name "lispm-rotom-test-holder" :server t
                                         :host "127.0.0.1" :service t :family 'ipv4))
           (port (process-contact holder :service))
           (process-environment (cons (format "FAKE_ROTOM_PORT=%d" port) process-environment))
           (lispm-rotom-browse-function #'ignore)
           (asked nil)
           (result 'none))
      (unwind-protect
          (cl-letf (((symbol-function 'read-string)
                     (lambda (prompt &rest _)
                       (setq asked prompt)
                       (format "http://127.0.0.1:%d/callback?code=c0de&state=s7" port))))
            (lispm-rotom-sign-in 'grok (lambda (why) (setq result why)))
            (should (lispm-rotom-test-wait (lambda () (not (eq result 'none)))))
            (should (null result))
            (should (string-match-p "Grok (xAI)" asked))
            (should (lispm-rotom-signed-in-p 'grok)))
        (delete-process holder)))))

(ert-deftest lispm-rotom-test-sign-in-cancel ()
  (lispm-rotom-test-with-fake
    (let* ((port (lispm-rotom-test-port))
           (process-environment (cons (format "FAKE_ROTOM_PORT=%d" port) process-environment))
           (lispm-rotom-browse-function #'ignore)
           (result 'none))
      (lispm-rotom-sign-in 'grok (lambda (why) (setq result why)))
      (should (lispm-rotom-test-wait
               (lambda () (eq (lispm-rotom-sign-in-state lispm-rotom-signing-in) 'browser))))
      (should (lispm-rotom-sign-in-servers lispm-rotom-signing-in))
      (lispm-rotom-cancel-sign-in)
      (should (lispm-rotom-test-wait (lambda () (not (eq result 'none)))))
      (should (equal result "Cancelled"))
      (should (null lispm-rotom-busy)))))

(ert-deftest lispm-rotom-test-sign-in-key ()
  "A key is handed to Rotom on its standard input."
  (lispm-rotom-test-with-fake
    (let ((result 'none))
      (cl-letf (((symbol-function 'read-passwd) (lambda (&rest _) (copy-sequence "vck_secret"))))
        (lispm-rotom-sign-in 'vercel (lambda (why) (setq result why))))
      (should (lispm-rotom-test-wait (lambda () (not (eq result 'none)))))
      (should (null result))
      (should (equal (lispm-rotom-test-read-file dir "vercel-key") "vck_secret\n"))
      (should-not (string-match-p "vck_secret" (lispm-rotom-test-read-file dir "calls")))
      (should (lispm-rotom-signed-in-p 'vercel)))))

;;;; LispM's agents

(ert-deftest lispm-rotom-test-backends ()
  (lispm-rotom-test-with-fake
    (let ((lispm-backends '((openai :api openai :url "https://api.openai.com/v1" :model "m"))))
      (lispm-rotom-add-backends)
      (should (equal (mapcar #'car lispm-backends) '(openai rotom rotom-messages)))
      (should (equal (plist-get (alist-get 'rotom lispm-backends) :url) "http://127.0.0.1:14550/v1"))
      (should (equal (plist-get (alist-get 'rotom-messages lispm-backends) :url) "http://127.0.0.1:14550"))
      (should (equal (plist-get (alist-get 'rotom lispm-backends) :model) "gpt-6-sol"))
      (should (null (plist-get (alist-get 'rotom lispm-backends) :key)))))
  (lispm-rotom-test-with-fake
    ;; An entry of the same name is the user's: it stays.
    (lispm-rotom-test-file home "config.json" "{\"model_fallback\": \"grok-4.7\", \"api_key\": \"k\"}")
    (let ((lispm-backends '((rotom :api openai :url "http://elsewhere/v1" :model "mine"))))
      (lispm-rotom-add-backends)
      (should (equal (plist-get (alist-get 'rotom lispm-backends) :model) "mine"))
      (should (equal (plist-get (alist-get 'rotom-messages lispm-backends) :model) "grok-4.7"))
      (should (eq (plist-get (alist-get 'rotom-messages lispm-backends) :key) #'lispm-rotom-api-key)))))

(ert-deftest lispm-rotom-test-use ()
  (skip-unless (require 'lispm-core nil t))
  (lispm-rotom-test-with-fake
    (let* ((custom-file (expand-file-name "custom.el" dir))
           ;; Customize saves nothing for emacs -Q otherwise.
           (user-init-file (expand-file-name "init.el" dir))
           (lispm-backends (list (car lispm-backends)))
           (lispm-backend nil)
           (lispm-model nil))
      (lispm-rotom-test-file dir "accounts" "codex\n")
      (lispm-rotom-test-read)
      (lispm-rotom-use "gpt-6.1-sol")
      (should (eq lispm-backend 'rotom))
      (should (equal lispm-model "gpt-6.1-sol"))
      (should (assq 'rotom lispm-backends))
      (should (equal (lispm-rotom-lispm) '(:backend rotom :rotom t :model "gpt-6.1-sol" :listed t)))
      (should (string-match-p "lispm-backend" (lispm-rotom-test-read-file dir "custom.el")))
      (should (string-match-p "LispM's agents use gpt-6.1-sol through Rotom" (lispm-rotom-report)))
      (lispm-rotom-use "grok-4.7" t)
      (should (eq lispm-backend 'rotom-messages))
      ;; Not offered while Grok is not signed in to.
      (should-not (plist-get (lispm-rotom-lispm) :listed))
      (lispm-rotom-stop-using)
      (should (null lispm-backend))
      (should (null lispm-model))
      (should-not (plist-get (lispm-rotom-lispm) :rotom)))))

;;;; What agents read

(ert-deftest lispm-rotom-test-reports ()
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\nkiro\n")
    (lispm-rotom-test-file dir "installed")
    (lispm-rotom-test-file dir "running")
    (lispm-rotom-test-read)
    (let ((report (lispm-rotom-report)))
      (should (string-match-p "Rotom 9.9.9.  Its daemon runs at http://127.0.0.1:14550." report))
      (should (string-match-p "- OpenAI (Codex) (codex): signed in, token for 7 days; credits 35511" report))
      (should (string-match-p "- Kiro (kiro): signed in, its token could not be renewed" report))
      (should (string-match-p "- Grok (xAI) (grok): not signed in" report)))
    (let ((models (lispm-rotom-models-report)))
      (should (string-match-p "OpenAI (Codex) (codex), signed in:\n- gpt-6-astra\n- gpt-6.1-sol" models))
      (should (string-match-p "Grok (xAI) (grok), not signed in, so not served" models)))
    (should (string-match-p "\\`Grok (xAI) (grok)" (lispm-rotom-models-report 'grok)))
    (should (equal (lispm-rotom-models-report 'nobody) "Rotom lists no models of nobody."))))

(ert-deftest lispm-rotom-test-log-tail ()
  (lispm-rotom-test-with-fake
    (should (string-match-p "has written no log" (lispm-rotom-log-tail)))
    (lispm-rotom-test-file home "rotom.err.log"
                           (mapconcat (lambda (n) (format "error: line %d" n)) (number-sequence 1 100) "\n"))
    (should (equal (lispm-rotom-log-tail 3) "error: line 98\nerror: line 99\nerror: line 100"))))

;;;; The window

(ert-deftest lispm-rotom-test-window ()
  (lispm-rotom-test-with-fake
    (lispm-rotom-test-file dir "accounts" "codex\n")
    (lispm-rotom-test-file dir "installed")
    (lispm-rotom-test-file dir "running")
    (let ((lispm-rotom-changed-hook (list #'lispm-rotom--refresh)))
      (unwind-protect
          (progn
            (save-window-excursion
              (lispm-rotom)
              (should (lispm-rotom-test-wait (lambda () (eq lispm-rotom-state 'ready)))))
            (with-current-buffer lispm-rotom-buffer-name
              (should (derived-mode-p 'lispm-rotom-mode))
              (let ((text (buffer-string)))
                (should (string-match-p "Running at http://127.0.0.1:14550" text))
                (should (string-match-p "OpenAI (Codex) +Signed in, token for 7 days" text))
                (should (string-match-p "Grok (xAI) +Not signed in" text))
                (should (string-match-p "grok-4.7 +Grok (xAI) +not signed in" text)))
              ;; n and p go from item to item; point starts on the daemon.
              (should (equal (lispm-rotom-item) '(daemon)))
              (lispm-rotom-next-line)
              (lispm-rotom-next-line)
              (should (eq (car (lispm-rotom-item)) 'account))
              (goto-char (point-min))
              (search-forward "gpt-6.1-sol")
              (should (equal (lispm-rotom-item) '(model . "gpt-6.1-sol")))
              (let ((used nil))
                (cl-letf (((symbol-function 'yes-or-no-p) (lambda (&rest _) t))
                          ((symbol-function 'lispm-rotom-use) (lambda (model &rest _) (setq used model))))
                  (lispm-rotom-act))
                (should (equal used "gpt-6.1-sol")))
              ;; Redrawn, point stays on its line.
              (lispm-rotom--refresh)
              (should (equal (lispm-rotom-item) '(model . "gpt-6.1-sol")))
              (lispm-rotom-copy)
              (should (equal (current-kill 0) "gpt-6.1-sol"))))
        (when (get-buffer lispm-rotom-buffer-name)
          (kill-buffer lispm-rotom-buffer-name))))))

(ert-deftest lispm-rotom-test-window-missing ()
  (lispm-rotom-test-with-fake
    (let ((lispm-rotom-program (expand-file-name "no-such-rotom" dir))
          (lispm-rotom-changed-hook (list #'lispm-rotom--refresh)))
      (unwind-protect
          (progn
            (save-window-excursion (lispm-rotom))
            (should (lispm-rotom-test-wait (lambda () (eq lispm-rotom-state 'missing))))
            (with-current-buffer lispm-rotom-buffer-name
              (should (string-match-p "Rotom is not installed" (buffer-string)))
              (should (string-match-p "cargo install --locked rotom" (buffer-string)))))
        (when (get-buffer lispm-rotom-buffer-name)
          (kill-buffer lispm-rotom-buffer-name))))))

(defun lispm-rotom-test--menu ()
  "Return Rotom's menu as it shows now: (LABEL COMMAND ENABLED), or
`separator'."
  (let ((items nil))
    (map-keymap (lambda (_key binding)
                  (pcase binding
                    (`(menu-item ,name ,command . ,properties)
                     (push (list (if (stringp name) name (eval name t)) command
                                 (or (not (plist-member properties :enable))
                                     (and (eval (plist-get properties :enable) t) t)))
                           items))
                    (_ (push 'separator items))))
                (keymap-lookup lispm-rotom-mode-map "<menu-bar> <rotom>"))
    (nreverse items)))

(ert-deftest lispm-rotom-test-commands-agree ()
  "Rotom's keys, menu and main commands are declared once, so they agree:
Restart while the daemon runs, Sign In while no account is being signed
in to, Use for LispM once there are models."
  (let ((commands (get 'lispm-rotom-mode 'lispm-kit-commands)))
    ;; Every key of the window is one declared.
    (map-keymap (lambda (_key binding)
                  (unless (keymapp binding)
                    (should (seq-find (lambda (command) (eq (plist-get command :command) binding)) commands))))
                lispm-rotom-mode-map)
    (should (equal (seq-keep (lambda (command) (and (plist-get command :button) (plist-get command :command)))
                             commands)
                   '(lispm-rotom-start-or-stop lispm-rotom-restart lispm-rotom-sign-in-here
                     lispm-rotom-refresh-tokens lispm-rotom-use-here)))
    (cl-flet ((state (running signing-in)
                (cl-letf (((symbol-function 'lispm-rotom-installed-p) (lambda () "/bin/rotom"))
                          ((symbol-function 'lispm-rotom-running-p) (lambda () running))
                          ((symbol-function 'lispm-rotom--signing-in-p) (lambda () signing-in))
                          ((symbol-function 'lispm-rotom-model-ids) (lambda () nil)))
                  (let ((lispm-rotom-busy nil))
                    (list (lispm-rotom-test--menu)
                          (mapcar (lambda (command)
                                    (when-let* ((button (plist-get command :button)))
                                      (list (if (functionp button) (funcall button) button)
                                            (and (funcall (plist-get command :enabled)) t))))
                                  commands))))))
      (pcase-let ((`(,menu ,buttons) (state nil nil)))
        (should (member '("Start Daemon" lispm-rotom-start-or-stop t) menu))
        (should (member '("Restart Daemon" lispm-rotom-restart nil) menu))
        (should (member '("Sign In…" lispm-rotom-sign-in-here t) menu))
        (should (member '("Start" t) buttons))
        (should (member '("Restart" nil) buttons))
        (should (member '("Use for LispM…" nil) buttons)))
      (pcase-let ((`(,menu ,buttons) (state t t)))
        (should (member '("Stop Daemon" lispm-rotom-start-or-stop t) menu))
        (should (member '("Restart Daemon" lispm-rotom-restart t) menu))
        (should (member '("Sign In…" lispm-rotom-sign-in-here nil) menu))
        (should (member '("Stop" t) buttons))
        (should (member '("Restart" t) buttons))
        (should (member '("Sign In…" nil) buttons))))))

(ert-deftest lispm-rotom-test-knows-no-desktop ()
  "Rotom's code names nothing of LispM Desktop's: the desktop dresses it."
  (dolist (file '("lispm-rotom.el" "lispm-rotom-ui.el"))
    (with-temp-buffer
      (insert-file-contents (expand-file-name file (expand-file-name ".." lispm-rotom-test-directory)))
      (should-not (re-search-forward "lispm-desktop\\|[Pp]latinum" nil t)))))

(provide 'lispm-rotom-test)
;;; lispm-rotom-test.el ends here
