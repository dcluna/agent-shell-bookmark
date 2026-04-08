;;; agent-shell-ol-test.el --- Tests for agent-shell-ol  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for agent-shell-ol.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Minimal stubs so we can load agent-shell-ol without the real
;; agent-shell package and its transitive dependencies.

(unless (featurep 'agent-shell)
  (defvar agent-shell--state nil)
  (defvar agent-shell-session-strategy 'prompt)
  (defvar agent-shell-mode-hook nil)
  (defvar agent-shell-agent-configs nil)
  (define-derived-mode agent-shell-mode fundamental-mode "Agent-Shell")
  (defun agent-shell-start (&rest _args) nil)
  (defun agent-shell-select-config (&rest _args) nil)
  (defun agent-shell--resolve-preferred-config () nil)
  (defun agent-shell-buffers () nil)
  (provide 'agent-shell))

(require 'org)
(require 'ol)
(require 'agent-shell-ol)

;;; --- path parsing ---

(ert-deftest agent-shell-ol-test-parse-path ()
  "Parse path splits on first two \"::\" into plist."
  (let ((result (agent-shell-ol--parse-path "/tmp/proj::sess-abc-123::claude-code")))
    (should (equal result '(:project-dir "/tmp/proj"
                            :session-id "sess-abc-123"
                            :agent-id "claude-code")))))

(ert-deftest agent-shell-ol-test-parse-path-no-separator ()
  "Parse path returns nil when no \"::\" separator is present."
  (should (null (agent-shell-ol--parse-path "no-separator"))))

(ert-deftest agent-shell-ol-test-parse-path-empty-session-id ()
  "Parse path handles empty session-id between separators."
  (let ((result (agent-shell-ol--parse-path "/tmp/proj::::claude-code")))
    (should (equal (plist-get result :project-dir) "/tmp/proj"))
    (should (equal (plist-get result :session-id) ""))
    (should (equal (plist-get result :agent-id) "claude-code"))))

;;; --- store link ---

(ert-deftest agent-shell-ol-test-store-link-agent-shell-buffer ()
  "Store link includes session-id and agent-id from agent-shell--state."
  (with-temp-buffer
    (rename-buffer "Test Agent @ proj" t)
    (agent-shell-mode)
    (let ((default-directory "/tmp/proj/")
          (agent-shell--state '(:session (:id "sess-42")
                                :agent-config (:identifier claude-code))))
      (let ((link (agent-shell-ol-store-link)))
        (should (stringp link))
        (should (string-match-p "^agent-shell:/tmp/proj/" link))
        (should (string-match-p "::sess-42::" link))
        (should (string-match-p "::claude-code$" link))))))

(ert-deftest agent-shell-ol-test-store-link-deferred-session ()
  "Store link with nil agent-shell--state stores empty session-id."
  (with-temp-buffer
    (rename-buffer "Deferred Agent @ proj" t)
    (agent-shell-mode)
    (let ((default-directory "/tmp/proj/")
          (agent-shell--state nil))
      (let ((link (agent-shell-ol-store-link)))
        (should (stringp link))
        (should (string-match-p "::::" link))))))

(ert-deftest agent-shell-ol-test-store-link-non-agent-shell-buffer ()
  "Store link returns nil in a non-agent-shell buffer."
  (with-temp-buffer
    (fundamental-mode)
    (should (null (agent-shell-ol-store-link)))))

;;; --- follow link ---

(ert-deftest agent-shell-ol-test-follow-find-buffer-by-session-id ()
  "Follow switches to an existing buffer found by session ID."
  (let ((test-buf (generate-new-buffer "Agent @ test")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (setq-local agent-shell--state
                        '(:session (:id "sess-match"))))
          (cl-letf (((symbol-function 'agent-shell-buffers)
                     (lambda () (list test-buf))))
            (agent-shell-ol-follow "/tmp/test::sess-match::claude-code" nil)
            (should (eq (current-buffer) test-buf))))
      (kill-buffer test-buf))))

(ert-deftest agent-shell-ol-test-follow-resume-session ()
  "Follow resumes session when no buffer matches but session-id is non-empty."
  (let ((start-called nil)
        (captured-session-id nil)
        (captured-dir nil))
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil))
              ((symbol-function 'agent-shell--resolve-preferred-config)
               (lambda () '(:identifier fallback-agent)))
              ((symbol-function 'agent-shell-start)
               (lambda (&rest args)
                 (setq start-called t
                       captured-session-id (plist-get args :session-id)
                       captured-dir default-directory))))
      (agent-shell-ol-follow "/tmp/myproj::sess-resume::claude-code" nil)
      (should start-called)
      (should (equal captured-session-id "sess-resume"))
      (should (equal captured-dir "/tmp/myproj")))))

(ert-deftest agent-shell-ol-test-follow-resume-with-stored-config ()
  "Follow uses config matching agent-id from agent-shell-agent-configs."
  (let ((start-called nil)
        (captured-config nil)
        (agent-shell-agent-configs '((:identifier claude-code :name "Claude")
                                     (:identifier other-agent :name "Other"))))
    (cl-letf (((symbol-function 'agent-shell-buffers) (lambda () nil))
              ((symbol-function 'agent-shell-start)
               (lambda (&rest args)
                 (setq start-called t
                       captured-config (plist-get args :config)))))
      (agent-shell-ol-follow "/tmp/proj::sess-cfg::claude-code" nil)
      (should start-called)
      (should (equal (plist-get captured-config :identifier) 'claude-code)))))

(ert-deftest agent-shell-ol-test-follow-spawn-fresh-accept ()
  "Follow spawns a new agent when session-id is empty and user accepts."
  (let ((start-called nil)
        (captured-strategy nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'agent-shell--resolve-preferred-config)
               (lambda () '(:identifier test-agent)))
              ((symbol-function 'agent-shell-start)
               (lambda (&rest args)
                 (setq start-called t
                       captured-strategy agent-shell-session-strategy)
                 ;; session-id should NOT be in args
                 (should (null (plist-get args :session-id))))))
      (agent-shell-ol-follow "/tmp/proj::::claude-code" nil)
      (should start-called)
      (should (eq captured-strategy 'new)))))

(ert-deftest agent-shell-ol-test-follow-spawn-fresh-decline ()
  "Follow does not spawn when session-id is empty and user declines."
  (let ((start-called nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
              ((symbol-function 'agent-shell-start)
               (lambda (&rest _args) (setq start-called t))))
      (agent-shell-ol-follow "/tmp/proj::::claude-code" nil)
      (should-not start-called))))

;;; --- link registration ---

(ert-deftest agent-shell-ol-test-link-registration ()
  "The \"agent-shell\" link type is registered in `org-link-parameters'."
  (should (assoc "agent-shell" org-link-parameters)))

(provide 'agent-shell-ol-test)
;;; agent-shell-ol-test.el ends here
