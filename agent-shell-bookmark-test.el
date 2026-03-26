;;; agent-shell-bookmark-test.el --- Tests for agent-shell-bookmark  -*- lexical-binding: t; -*-

;;; Commentary:

;; ERT tests for agent-shell-bookmark.

;;; Code:

(require 'ert)
(require 'bookmark)
(require 'map)

;; Minimal stubs so we can load agent-shell-bookmark without the real
;; agent-shell package and its transitive dependencies.

(unless (featurep 'agent-shell)
  (defvar agent-shell--state nil)
  (defvar agent-shell-mode-hook nil)
  (defun agent-shell-resume-session (_session-id) nil)
  (defun agent-shell () nil)
  (provide 'agent-shell))

(require 'agent-shell-bookmark)

;;; --- bookmark record creation ---

(ert-deftest agent-shell-bookmark-test-make-record-basic ()
  "Record includes handler, location, buffer-name, and project-path."
  (with-temp-buffer
    (rename-buffer "Test Agent @ myproject" t)
    (let ((default-directory "/tmp/myproject/")
          (agent-shell--state nil))
      (let ((record (agent-shell-bookmark-make-record)))
        (should (equal (car record) (buffer-name)))
        (should (eq (alist-get 'handler (cdr record))
                    'agent-shell-bookmark-handler))
        (should (equal (alist-get 'location (cdr record))
                       (expand-file-name "/tmp/myproject/")))
        (should (equal (alist-get 'buffer-name (cdr record))
                       (buffer-name)))
        (should (equal (alist-get 'project-path (cdr record))
                       (expand-file-name "/tmp/myproject/")))))))

(ert-deftest agent-shell-bookmark-test-make-record-with-session-id ()
  "Record captures session ID from agent-shell--state."
  (with-temp-buffer
    (rename-buffer "Test Agent @ proj" t)
    (let ((default-directory "/tmp/proj/")
          (agent-shell--state `((:session . ((:id . "sess-abc-123"))))))
      (let ((record (agent-shell-bookmark-make-record)))
        (should (equal (alist-get 'session-id (cdr record))
                       "sess-abc-123"))))))

(ert-deftest agent-shell-bookmark-test-make-record-nil-session ()
  "Record has nil session-id when state has no session."
  (with-temp-buffer
    (rename-buffer "Test Agent @ proj2" t)
    (let ((default-directory "/tmp/proj2/")
          (agent-shell--state nil))
      (let ((record (agent-shell-bookmark-make-record)))
        (should (null (alist-get 'session-id (cdr record))))))))

;;; --- handler type registration ---

(ert-deftest agent-shell-bookmark-test-handler-type ()
  "Handler has bookmark-handler-type property set to \"agent-shell\"."
  (should (equal (get 'agent-shell-bookmark-handler 'bookmark-handler-type)
                 "agent-shell")))

;;; --- bookmark handler dispatch ---

(ert-deftest agent-shell-bookmark-test-handler-switches-to-live-buffer ()
  "Handler switches to existing buffer when it is still live."
  (let ((test-buf (generate-new-buffer "Live Agent @ test")))
    (unwind-protect
        (let ((bmk `("Live Agent @ test"
                     (handler . agent-shell-bookmark-handler)
                     (buffer-name . ,(buffer-name test-buf))
                     (session-id . "sess-123")
                     (project-path . "/tmp/test/"))))
          (agent-shell-bookmark-handler bmk)
          (should (eq (current-buffer) test-buf)))
      (kill-buffer test-buf))))

(ert-deftest agent-shell-bookmark-test-handler-resumes-session-when-buffer-gone ()
  "Handler calls `agent-shell-resume-session' when buffer does not exist."
  (let ((resumed-id nil))
    (cl-letf (((symbol-function 'agent-shell-resume-session)
               (lambda (sid) (setq resumed-id sid))))
      (let ((bmk `("Gone Agent @ test"
                   (handler . agent-shell-bookmark-handler)
                   (buffer-name . "Nonexistent Buffer 999")
                   (session-id . "sess-xyz-789")
                   (project-path . "/tmp/gone/"))))
        (agent-shell-bookmark-handler bmk)
        (should (equal resumed-id "sess-xyz-789"))))))

(ert-deftest agent-shell-bookmark-test-handler-starts-fresh-without-session-id ()
  "Handler calls `agent-shell' when no session ID and no buffer."
  (let ((called nil))
    (cl-letf (((symbol-function 'agent-shell)
               (lambda () (setq called t))))
      (let ((bmk `("Fresh Agent @ test"
                   (handler . agent-shell-bookmark-handler)
                   (buffer-name . "Nonexistent Buffer 998")
                   (session-id . nil)
                   (project-path . "/tmp/fresh/"))))
        (agent-shell-bookmark-handler bmk)
        (should called)))))

(ert-deftest agent-shell-bookmark-test-handler-sets-default-directory ()
  "Handler sets `default-directory' to project-path for resume and fresh start."
  (let ((captured-dir nil))
    (cl-letf (((symbol-function 'agent-shell-resume-session)
               (lambda (_sid) (setq captured-dir default-directory))))
      (let ((bmk `("Dir Agent @ test"
                   (handler . agent-shell-bookmark-handler)
                   (buffer-name . "Nonexistent Buffer 997")
                   (session-id . "sess-dir-test")
                   (project-path . "/tmp/dirtest/"))))
        (agent-shell-bookmark-handler bmk)
        (should (equal captured-dir "/tmp/dirtest/"))))))

;;; --- mode hook setup ---

(ert-deftest agent-shell-bookmark-test-hook-registered ()
  "Setup function is on `agent-shell-mode-hook'."
  (should (memq #'agent-shell-bookmark--setup agent-shell-mode-hook)))

(ert-deftest agent-shell-bookmark-test-setup-sets-record-function ()
  "Setup function sets `bookmark-make-record-function' buffer-locally."
  (with-temp-buffer
    (agent-shell-bookmark--setup)
    (should (eq bookmark-make-record-function
                #'agent-shell-bookmark-make-record))))

;;; --- counsel transformer ---

(ert-deftest agent-shell-bookmark-test-counsel-transformer-agent-shell ()
  "Transformer shows \"agent-shell\" type for agent-shell bookmarks."
  (let ((bookmark-alist
         `(("my-agent"
            (handler . agent-shell-bookmark-handler)
            (location . "/tmp/project/")
            (buffer-name . "Agent @ proj")
            (session-id . "sess-1")
            (project-path . "/tmp/project/")))))
    (let ((result (agent-shell-bookmark--counsel-transformer "my-agent")))
      (should (string-match-p "agent-shell" result))
      (should (string-match-p "/tmp/project/" result)))))

(ert-deftest agent-shell-bookmark-test-counsel-transformer-file-bookmark ()
  "Transformer shows \"file\" type for bookmarks without a handler."
  (let ((bookmark-alist
         `(("some-file"
            (filename . "/tmp/foo.el")
            (position . 1)))))
    (let ((result (agent-shell-bookmark--counsel-transformer "some-file")))
      (should (string-match-p "file" result))
      (should (string-match-p "/tmp/foo.el" result)))))

;;; --- round-trip integration ---

(ert-deftest agent-shell-bookmark-test-set-and-jump-round-trip ()
  "Setting a bookmark and jumping back reaches the original buffer."
  (let ((bookmark-alist nil)
        (test-buf (generate-new-buffer "Roundtrip Agent @ rt")))
    (unwind-protect
        (progn
          (with-current-buffer test-buf
            (let ((default-directory "/tmp/roundtrip/")
                  (agent-shell--state
                   `((:session . ((:id . "sess-rt-001"))))))
              (agent-shell-bookmark--setup)
              (bookmark-set "rt-test" t)))
          ;; Jump from a different buffer.
          (with-temp-buffer
            (bookmark-jump "rt-test" #'switch-to-buffer)
            (should (eq (current-buffer) test-buf))))
      (kill-buffer test-buf))))

(provide 'agent-shell-bookmark-test)
;;; agent-shell-bookmark-test.el ends here
