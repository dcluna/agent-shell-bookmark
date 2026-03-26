;;; agent-shell-bookmark.el --- Bookmark support for agent-shell sessions  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Luna

;; Author: Daniel Luna
;; URL: https://github.com/dcluna/agent-shell-bookmark
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (agent-shell "0.1.0"))
;; Keywords: convenience, tools

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Bookmark support for `agent-shell' sessions.
;;
;; When this package is loaded, `agent-shell-mode' buffers become
;; bookmarkable.  Jumping to an agent-shell bookmark will:
;;
;; - Switch to the buffer if it is already open.
;; - Resume the session by its stored session ID if the buffer is gone.
;; - Fall back to the session list prompt (same as `agent-shell-session-strategy'
;;   set to `prompt') when the session ID is stale.
;;
;; Usage:
;;
;;   (require 'agent-shell-bookmark)
;;
;; Then use `bookmark-set' (C-x r m) in any agent-shell buffer.
;;
;; If `counsel' is loaded, `counsel-bookmark' candidates are annotated
;; with bookmark type and location automatically.

;;; Code:

(require 'bookmark)
(require 'agent-shell)

;;; Bookmark record creation

(defun agent-shell-bookmark-make-record ()
  "Create a bookmark record for the current `agent-shell' session."
  (let ((session-id (and (boundp 'agent-shell--state)
                         agent-shell--state
                         (map-nested-elt agent-shell--state '(:session :id))))
        (buf-name (buffer-name))
        (project-path (expand-file-name default-directory)))
    `(,buf-name
      (handler . agent-shell-bookmark-handler)
      (location . ,project-path)
      (session-id . ,session-id)
      (buffer-name . ,buf-name)
      (project-path . ,project-path))))

;;; Bookmark jump handler

(defun agent-shell-bookmark-handler (bmk)
  "Handle jumping to an agent-shell bookmark BMK.
If the original buffer is still live, switch to it.
Otherwise, resume the session by its stored ID.  When the session
ID is stale, `agent-shell-resume-session' falls back to the
session list prompt automatically."
  (let* ((buf-name (bookmark-prop-get bmk 'buffer-name))
         (session-id (bookmark-prop-get bmk 'session-id))
         (project-path (bookmark-prop-get bmk 'project-path))
         (buf (and buf-name (get-buffer buf-name))))
    (cond
     ;; Buffer already open — just switch to it.
     ((and buf (buffer-live-p buf))
      (set-buffer buf))
     ;; Session ID available — try to resume (falls back to session list).
     (session-id
      (let ((default-directory (or project-path default-directory)))
        (agent-shell-resume-session session-id)))
     ;; No session ID — start fresh with session prompt.
     (t
      (let ((default-directory (or project-path default-directory)))
        (agent-shell))))))

;;; Type registration

(put 'agent-shell-bookmark-handler 'bookmark-handler-type "agent-shell")

;;; Hook into agent-shell-mode

(defun agent-shell-bookmark--setup ()
  "Set up bookmark support in the current `agent-shell' buffer."
  (setq-local bookmark-make-record-function
              #'agent-shell-bookmark-make-record))

(add-hook 'agent-shell-mode-hook #'agent-shell-bookmark--setup)

;;; counsel-bookmark integration

(defun agent-shell-bookmark--counsel-transformer (candidate)
  "Annotate CANDIDATE with bookmark type and location for `counsel-bookmark'."
  (let* ((record (bookmark-get-bookmark-record candidate))
         (type (or (and record
                        (bookmark-type-from-full-record (cons candidate record)))
                   ""))
         (loc (bookmark-location candidate)))
    (format "%-40s %-14s %s"
            candidate
            (propertize (if (string-empty-p type) "file" type)
                        'face 'font-lock-type-face)
            (propertize (or loc "")
                        'face 'font-lock-comment-face))))

(with-eval-after-load 'counsel
  (with-eval-after-load 'ivy
    (ivy-configure 'counsel-bookmark
      :display-transformer-fn
      #'agent-shell-bookmark--counsel-transformer)))

(provide 'agent-shell-bookmark)
;;; agent-shell-bookmark.el ends here
