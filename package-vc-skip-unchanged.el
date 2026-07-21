;;; package-vc-skip-unchanged.el --- Skip unchanged VC package upgrades -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Misaka

;; Author: Misaka <chuxubank@qq.com>
;; Maintainer: Misaka <chuxubank@qq.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1"))
;; Keywords: convenience, package
;; URL: https://github.com/chuxubank/package-vc-skip-unchanged

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; Avoid unnecessary work in `package-vc-upgrade-all'.  Git repositories
;; are fetched with bounded concurrency, then packages whose local HEAD
;; already matches their upstream commit are skipped.  Packages that changed,
;; use another VC backend, or could not be checked are passed to the native
;; `package-vc-upgrade' implementation.
;;
;; Enable the behavior globally with `package-vc-skip-unchanged-mode'.

;;; Code:

(require 'cl-lib)
(require 'package-vc)
(require 'subr-x)
(require 'vc-git)

(defgroup package-vc-skip-unchanged nil
  "Skip unchanged packages during VC package upgrades."
  :group 'package
  :prefix "package-vc-skip-unchanged-")

(defcustom package-vc-skip-unchanged-max-concurrency 6
  "Maximum number of VC package jobs run concurrently.
Each job includes the remote check and, when needed, the native upgrade."
  :type 'integer
  :group 'package-vc-skip-unchanged)

(cl-defstruct (package-vc-skip-unchanged--upgrade-state
               (:constructor package-vc-skip-unchanged--make-upgrade-state))
  pending
  active
  total
  unchanged
  upgrades
  check-failures
  upgrade-failures
  limit
  started-at
  pumping)

(defvar package-vc-skip-unchanged--upgrade-state nil
  "State of the current `package-vc-upgrade-all' operation.")

(defun package-vc-skip-unchanged--installed-packages ()
  "Return all installed VC package descriptors."
  (let (packages)
    (dolist (entry package-alist (nreverse packages))
      (dolist (pkg-desc (cdr entry))
        (when (package-vc-p pkg-desc)
          (push pkg-desc packages))))))

(defun package-vc-skip-unchanged--git-output (pkg-desc &rest args)
  "Run Git with ARGS for PKG-DESC and return its trimmed output."
  (let ((default-directory (package-desc-dir pkg-desc)))
    (string-trim
     (with-output-to-string
       (apply #'vc-git-command standard-output nil nil args)))))

(defun package-vc-skip-unchanged--same-revision-p (pkg-desc)
  "Return non-nil when PKG-DESC is at its upstream revision."
  (string= (package-vc-skip-unchanged--git-output pkg-desc "rev-parse" "HEAD")
           (package-vc-skip-unchanged--git-output pkg-desc "rev-parse" "@{u}")))

(defun package-vc-skip-unchanged--finish (state)
  "Finish the package upgrade operation represented by STATE."
  (when (eq state package-vc-skip-unchanged--upgrade-state)
    (setq package-vc-skip-unchanged--upgrade-state nil)
    (message
     (concat "VC package update finished in %.1fs for %d packages: "
             "%d unchanged, %d selected for upgrade, "
             "%d check fallbacks, %d failures")
     (- (float-time) (package-vc-skip-unchanged--upgrade-state-started-at state))
     (package-vc-skip-unchanged--upgrade-state-total state)
     (package-vc-skip-unchanged--upgrade-state-unchanged state)
     (package-vc-skip-unchanged--upgrade-state-upgrades state)
     (package-vc-skip-unchanged--upgrade-state-check-failures state)
     (package-vc-skip-unchanged--upgrade-state-upgrade-failures state))))

(defun package-vc-skip-unchanged--job-finished (state)
  "Mark one package job in STATE as finished and start more work."
  (cl-decf (package-vc-skip-unchanged--upgrade-state-active state))
  (package-vc-skip-unchanged--pump state))

(defun package-vc-skip-unchanged--upgrade-sentinel (process event)
  "Run the original sentinel for PROCESS, then account for its EVENT."
  (let ((original (process-get process 'package-vc-skip-unchanged--original-sentinel)))
    (unwind-protect
        (when original
          (funcall original process event))
      (when (and (memq (process-status process) '(exit signal))
                 (not (process-get process 'package-vc-skip-unchanged--handled)))
        (process-put process 'package-vc-skip-unchanged--handled t)
        (let ((state (process-get process 'package-vc-skip-unchanged--state)))
          (unless (and (eq (process-status process) 'exit)
                       (zerop (process-exit-status process)))
            (cl-incf (package-vc-skip-unchanged--upgrade-state-upgrade-failures state)))
          (package-vc-skip-unchanged--job-finished state))))))

(defun package-vc-skip-unchanged--start-upgrade (state pkg-desc)
  "Start the native package upgrade for PKG-DESC in STATE."
  (cl-incf (package-vc-skip-unchanged--upgrade-state-upgrades state))
  (message "Updating VC package %s" (package-desc-name pkg-desc))
  (condition-case err
      (let ((process (package-vc-upgrade pkg-desc)))
        (if (and (processp process) (process-live-p process))
            (progn
              (process-put process 'package-vc-skip-unchanged--state state)
              (process-put process 'package-vc-skip-unchanged--original-sentinel
                           (process-sentinel process))
              (set-process-sentinel process #'package-vc-skip-unchanged--upgrade-sentinel))
          (package-vc-skip-unchanged--job-finished state)))
    (error
     (cl-incf (package-vc-skip-unchanged--upgrade-state-upgrade-failures state))
     (message "Failed to update VC package %s: %s"
              (package-desc-name pkg-desc) (error-message-string err))
     (package-vc-skip-unchanged--job-finished state))))

(defun package-vc-skip-unchanged--fetch-sentinel (process _event)
  "Compare revisions and possibly upgrade after PROCESS finishes."
  (when (and (memq (process-status process) '(exit signal))
             (not (process-get process 'package-vc-skip-unchanged--handled)))
    (process-put process 'package-vc-skip-unchanged--handled t)
    (let ((state (process-get process 'package-vc-skip-unchanged--state))
          (pkg-desc (process-get process 'package-vc-skip-unchanged--package))
          (buffer (process-buffer process)))
      (unwind-protect
          (if (and (eq (process-status process) 'exit)
                   (zerop (process-exit-status process)))
              (condition-case err
                  (if (package-vc-skip-unchanged--same-revision-p pkg-desc)
                      (progn
                        (cl-incf
                         (package-vc-skip-unchanged--upgrade-state-unchanged state))
                        (message "Package %s already up-to-date"
                                 (package-desc-name pkg-desc))
                        (package-vc-skip-unchanged--job-finished state))
                    (package-vc-skip-unchanged--start-upgrade state pkg-desc))
                (error
                 (cl-incf
                  (package-vc-skip-unchanged--upgrade-state-check-failures state))
                 (message "Could not compare VC package %s: %s"
                          (package-desc-name pkg-desc)
                          (error-message-string err))
                 (package-vc-skip-unchanged--start-upgrade state pkg-desc)))
            (cl-incf (package-vc-skip-unchanged--upgrade-state-check-failures state))
            (message "Could not fetch VC package %s; falling back to upgrade"
                     (package-desc-name pkg-desc))
            (package-vc-skip-unchanged--start-upgrade state pkg-desc))
        (when (buffer-live-p buffer)
          (kill-buffer buffer))))))

(defun package-vc-skip-unchanged--start-fetch (state pkg-desc)
  "Start an asynchronous Git fetch for PKG-DESC in STATE."
  (require 'vc-git)
  (let* ((default-directory (package-desc-dir pkg-desc))
         (buffer (generate-new-buffer
                  (format " *package-vc-skip-unchanged-fetch: %s*"
                          (package-desc-name pkg-desc))))
         process)
    (unwind-protect
        (progn
          (setq process
                (make-process
                 :name (format "package-vc-skip-unchanged-fetch-%s"
                               (package-desc-name pkg-desc))
                 :buffer buffer
                 :stderr buffer
                 :command (list vc-git-program "fetch" "--quiet")
                 :connection-type 'pipe
                 :noquery t
                 :sentinel #'package-vc-skip-unchanged--fetch-sentinel))
          (process-put process 'package-vc-skip-unchanged--state state)
          (process-put process 'package-vc-skip-unchanged--package pkg-desc))
      (unless process
        (kill-buffer buffer)))))

(defun package-vc-skip-unchanged--start-job (state pkg-desc)
  "Start one package job for PKG-DESC in STATE."
  (condition-case err
      (if (eq (vc-responsible-backend (package-desc-dir pkg-desc)) 'Git)
          (package-vc-skip-unchanged--start-fetch state pkg-desc)
        (package-vc-skip-unchanged--start-upgrade state pkg-desc))
    (error
     (cl-incf (package-vc-skip-unchanged--upgrade-state-check-failures state))
     (message "Could not check VC package %s: %s"
              (package-desc-name pkg-desc) (error-message-string err))
     (package-vc-skip-unchanged--start-upgrade state pkg-desc))))

(defun package-vc-skip-unchanged--pump (state)
  "Fill available concurrent work slots in STATE."
  (unless (package-vc-skip-unchanged--upgrade-state-pumping state)
    (setf (package-vc-skip-unchanged--upgrade-state-pumping state) t)
    (unwind-protect
        (progn
          (while (and (package-vc-skip-unchanged--upgrade-state-pending state)
                      (< (package-vc-skip-unchanged--upgrade-state-active state)
                         (package-vc-skip-unchanged--upgrade-state-limit state)))
            (let ((pkg-desc (pop
                             (package-vc-skip-unchanged--upgrade-state-pending state))))
              (cl-incf (package-vc-skip-unchanged--upgrade-state-active state))
              (package-vc-skip-unchanged--start-job state pkg-desc)))
          (when (and (null (package-vc-skip-unchanged--upgrade-state-pending state))
                     (zerop (package-vc-skip-unchanged--upgrade-state-active state)))
            (package-vc-skip-unchanged--finish state)))
      (setf (package-vc-skip-unchanged--upgrade-state-pumping state) nil))))

(defun package-vc-skip-unchanged-upgrade-all ()
  "Upgrade VC packages, skipping those whose upstream hash is unchanged.
Git repositories are checked concurrently, with at most
`package-vc-skip-unchanged-max-concurrency' active jobs."
  (interactive)
  (when package-vc-skip-unchanged--upgrade-state
    (user-error "A VC package update is already running"))
  (let* ((packages (package-vc-skip-unchanged--installed-packages))
         (state (package-vc-skip-unchanged--make-upgrade-state
                 :pending packages
                 :active 0
                 :total (length packages)
                 :unchanged 0
                 :upgrades 0
                 :check-failures 0
                 :upgrade-failures 0
                 :limit (max 1 package-vc-skip-unchanged-max-concurrency)
                 :started-at (float-time))))
    (setq package-vc-skip-unchanged--upgrade-state state)
    (message "Checking %d VC packages with up to %d concurrent jobs"
             (length packages)
             (package-vc-skip-unchanged--upgrade-state-limit state))
    (package-vc-skip-unchanged--pump state)
    ;; Batch Emacs exits without waiting for asynchronous child processes.
    (when noninteractive
      (while (eq state package-vc-skip-unchanged--upgrade-state)
        (accept-process-output nil 0.1)))
    state))

;;;###autoload
(define-minor-mode package-vc-skip-unchanged-mode
  "Globally skip unchanged packages in `package-vc-upgrade-all'."
  :global t
  :group 'package-vc-skip-unchanged
  (advice-remove 'package-vc-upgrade-all
                 #'package-vc-skip-unchanged-upgrade-all)
  (when package-vc-skip-unchanged-mode
    (advice-add 'package-vc-upgrade-all :override
                #'package-vc-skip-unchanged-upgrade-all)))

(provide 'package-vc-skip-unchanged)
;;; package-vc-skip-unchanged.el ends here
