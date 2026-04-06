;;; agent-shell-ol-test.el --- Tests for agent-shell-ol  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for agent-shell-ol.

;;; Code:

(require 'ert)
(require 'cl-lib)

;; Minimal stubs so we can load agent-shell-ol without the real
;; agent-shell package and its transitive dependencies.

(unless (featurep 'agent-shell)
  (defvar agent-shell-session-strategy 'prompt)
  (defvar agent-shell-mode-hook nil)
  (define-derived-mode agent-shell-mode fundamental-mode "Agent-Shell")
  (defun agent-shell-start (&rest _args) nil)
  (defun agent-shell-select-config (&rest _args) nil)
  (defun agent-shell--resolve-preferred-config () nil)
  (provide 'agent-shell))

(require 'org)
(require 'ol)
(require 'agent-shell-ol)

;;; --- path parsing ---

(ert-deftest agent-shell-ol-test-parse-path ()
  "Parse path splits on first \"::\" into (project-dir . buffer-name)."
  (let ((result (agent-shell-ol--parse-path "/tmp/proj::My Agent @ proj")))
    (should (equal result '("/tmp/proj" . "My Agent @ proj")))))

(ert-deftest agent-shell-ol-test-parse-path-no-separator ()
  "Parse path returns nil when no \"::\" separator is present."
  (should (null (agent-shell-ol--parse-path "no-separator"))))

;;; --- store link ---

(ert-deftest agent-shell-ol-test-store-link-agent-shell-buffer ()
  "Store link returns a link string in an `agent-shell-mode' buffer."
  (with-temp-buffer
    (rename-buffer "Test Agent @ proj" t)
    (agent-shell-mode)
    (let ((default-directory "/tmp/proj/"))
      (let ((link (agent-shell-ol-store-link)))
        (should (stringp link))
        (should (string-match-p "^agent-shell:" link))
        (should (string-match-p "::" link))
        (should (string-match-p (regexp-quote (buffer-name)) link))))))

(ert-deftest agent-shell-ol-test-store-link-non-agent-shell-buffer ()
  "Store link returns nil in a non-agent-shell buffer."
  (with-temp-buffer
    (fundamental-mode)
    (should (null (agent-shell-ol-store-link)))))

;;; --- follow link ---

(ert-deftest agent-shell-ol-test-follow-live-buffer ()
  "Follow switches to an existing live buffer."
  (let ((test-buf (generate-new-buffer "Live Agent @ test")))
    (unwind-protect
        (progn
          (agent-shell-ol-follow
           (concat "/tmp/test::" (buffer-name test-buf)) nil)
          (should (eq (current-buffer) test-buf)))
      (kill-buffer test-buf))))

(ert-deftest agent-shell-ol-test-follow-dead-buffer-accept-respawn ()
  "Follow spawns a new agent when user accepts the prompt."
  (let ((start-called nil)
        (captured-dir nil)
        (captured-strategy nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) t))
              ((symbol-function 'agent-shell--resolve-preferred-config)
               (lambda () '((:identifier . test-agent))))
              ((symbol-function 'agent-shell-start)
               (lambda (&rest _args)
                 (setq start-called t
                       captured-dir default-directory
                       captured-strategy agent-shell-session-strategy))))
      (agent-shell-ol-follow "/tmp/myproj::Dead Agent @ myproj" nil)
      (should start-called)
      (should (equal captured-dir "/tmp/myproj"))
      (should (eq captured-strategy 'new)))))

(ert-deftest agent-shell-ol-test-follow-dead-buffer-decline-respawn ()
  "Follow does not spawn when user declines the prompt."
  (let ((start-called nil))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (_prompt) nil))
              ((symbol-function 'agent-shell-start)
               (lambda (&rest _args) (setq start-called t))))
      (agent-shell-ol-follow "/tmp/myproj::Dead Agent @ myproj" nil)
      (should-not start-called))))

;;; --- link registration ---

(ert-deftest agent-shell-ol-test-link-registration ()
  "The \"agent-shell\" link type is registered in `org-link-parameters'."
  (should (assoc "agent-shell" org-link-parameters)))

(provide 'agent-shell-ol-test)
;;; agent-shell-ol-test.el ends here
