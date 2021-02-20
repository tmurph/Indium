;;; indium-nodejs.el --- NodeJS support for indium  -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2018  Nicolas Petton

;; Author: Nicolas Petton <nicolas@petton.fr>
;; Keywords: tools, javascript

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

;; Handle indium connections to a NodeJS process using the v8 backend.
;;
;; Important note: For this package to work, NodeJS version 7.0 (or any newer
;; version) is required.

;;; Code:

(require 'url)
(require 'url-parse)
(require 'json)
(require 'map)
(require 'seq)
(require 'subr-x)
(require 'comint)

(declare-function indium-client-connect "indium-client.el")
(declare-function indium-client-disconnect "indium-client.el")
(declare-function indium-quit "indium-interaction.el")

(defcustom indium-nodejs-client-closes-server nil
  "When non-nil, closing the indium client also closes indium's inferior NodeJS server."
  :group 'indium-nodejs
  :type 'boolean)

(defcustom indium-nodejs-server-prompt-regexp "^> "
  "Regexp to recognize prompts in the NodeJS server repl."
  :group 'indium-nodejs
  :type 'string)

(defvar indium-nodejs--project-file nil
  "The full path to the current indium project's configuration file.")
(defvar indium-nodejs--project-name nil
  "The \"name\" parameter of the current indium project's configuration.")
(defvar indium-nodejs--debugger-connected nil
  "Non-nil if the indium client is currently connected to a NodeJS server.")

(defun indium-launch-nodejs (conf)
  "Start a NodeJS process.

Execute the command specified in CONF, adding the `--inspect'
flag.  When the process is ready, open an Indium connection on
it.

If the configuration setting `inspect-brk' is non-nil, break the
execution at the first statement."
  (let-alist conf
    (unless .program
      (user-error "No NodeJS program specified in the .indium.json file"))
    (let* ((default-directory .resolvedRoot)
           (command-with-flags (indium-nodejs--command-with-flags
                                .program
                                .args
                                .inspect-brk
                                .port))
           (buffer (make-comint-in-buffer "indium-nodejs-process" "*node process*"
                                          shell-file-name nil
                                          shell-command-switch command-with-flags))
	   (process (get-buffer-process buffer)))
      (add-function :after (process-sentinel process)
                    #'indium-nodejs--maybe-stop-client)
      (message "Running node command \"%s\"" command-with-flags)
      (setq indium-nodejs--project-file .projectFile
            indium-nodejs--project-name .name)
      (with-current-buffer buffer
        (indium-nodejs-mode))
      (switch-to-buffer buffer))))


(defun indium-nodejs--command-with-flags (program args inspect-brk &optional port)
  "Return a command string with flags to start the V8 inspector.

PROGRAM is the executable to run, with ARGS being the passed to the program.

If INSPECT-BRK is nil, use the `--inspect', use the
`--inspect-brk' flag otherwise.

If PORT is non-nil, start the debugging process on that port,
otherwise use Node's default port (9229)."
  (let ((inspect-flag (if (eq inspect-brk t) " --inspect-brk" " --inspect"))
        (inspect-port-flag (if port (format " --inspect-port=%s" port) "")))
    (format "%s%s%s %s" program inspect-flag inspect-port-flag args)))

(defun indium-nodejs--maybe-stop-client (proc _msg)
  "Stop the indium client and clean up interaction buffers when PROC dies.

Intended to be run as part of the inferior NodeJS process sentinel."
  (when (not (process-live-p proc))
    (indium-quit)))

(defun indium-nodejs--inferior-node-filter-escape-codes (output)
  "Remove escape codes that the inferior NodeJS server emits."
  (replace-regexp-in-string "\033\\[[0-9]+[GKJ]" "" output))

(defun indium-nodejs--inferior-node-watch-for-debugger-status (output)
  "Update the indium client connection status based on the inferior NodeJS output."
  (cond
   ((and (not indium-nodejs--debugger-connected)
         (string-match-p "Debugger listening on" output))
    ;; Node will keep outputting the "Debugger listening on" message after
    ;; each deconnection, so only try to connect one.
    (indium-client-connect (file-name-directory indium-nodejs--project-file)
                           indium-nodejs--project-name)
    (setq indium-nodejs--debugger-connected t))
   ((string-match-p "Waiting for the debugger to disconnect" output)
    ;; When Node receives a signal to exit, it first waits for all
    ;; debuggers to disconnect before shutting down.  Watch for the
    ;; "Waiting for the debugger to disconnect" message and then do so.
    (indium-client-disconnect)
    (setq indium-nodejs--debugger-connected nil)))
  output)

(defun indium-nodejs--clean-up-project-vars ()
  (setq indium-nodejs--project-file nil
        indium-nodejs--project-name nil))

(defun indium-nodejs--maybe-close-nodejs ()
  "Stop indium's inferior NodeJS server process.

However, this does nothing if `indium-nodejs-client-closes-server' is nil."
  (when indium-nodejs-client-closes-server
    (let* ((proc (get-process "indium-nodejs-process"))
           (buf (and proc (process-buffer proc))))
      (when (process-live-p proc) (kill-process proc))
      (when (buffer-live-p buf) (kill-buffer buf)))))

(define-derived-mode indium-nodejs-mode comint-mode "Indium NodeJS"
  "Major mode for interacting with Indium's inferior NodeJS process.

TODO: mention major commands

TODO: mention major customization options"
  (set (make-local-variable 'comint-prompt-regexp)
       indium-nodejs-server-prompt-regexp)
  (set (make-local-variable 'paragraph-start) indium-nodejs-server-prompt-regexp)
  (set (make-local-variable 'paragraph-separate)
       (concat indium-nodejs-server-prompt-regexp "$"))
  (set (make-local-variable 'comint-prompt-read-only) t)
  (add-hook 'comint-preoutput-filter-functions
            'indium-nodejs--inferior-node-filter-escape-codes nil 'local)
  (add-hook 'comint-preoutput-filter-functions
            'indium-nodejs--inferior-node-watch-for-debugger-status nil 'local)
  (add-hook 'kill-buffer-hook 'indium-nodejs--clean-up-project-vars nil 'local))

(add-hook 'indium-client-closed-hook #'indium-nodejs--maybe-close-nodejs t)

(provide 'indium-nodejs)
;;; indium-nodejs.el ends here
