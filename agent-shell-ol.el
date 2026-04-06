;;; agent-shell-ol.el --- Org-link support for agent-shell buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Daniel Luna

;; Author: Daniel Luna
;; URL: https://github.com/dcluna/agent-shell-bookmark
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (agent-shell "0.50.1") (org "9.7"))
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

;; Org-link type `"agent-shell"' for `agent-shell' buffers.
;;
;; When this package is loaded, `org-store-link' in an `agent-shell-mode'
;; buffer captures a link of the form:
;;
;;   [[agent-shell:/path/to/project::session-id::agent-id][Buffer Name]]
;;
;; Following such a link finds the buffer by session ID if it is live,
;; resumes the session if the buffer is gone, or offers to spawn a new
;; agent session when no session ID was stored.
;;
;; Usage:
;;
;;   (require 'agent-shell-ol)

;;; Code:

(require 'ol)
(require 'map)
(require 'agent-shell)

;;; Path parsing

(defun agent-shell-ol--parse-path (path)
  "Parse PATH of the form \"project-dir::session-id::agent-id\".
Split on the first two occurrences of \"::\".  Return a plist
\(:project-dir PROJECT-DIR :session-id SESSION-ID :agent-id AGENT-ID),
or nil if fewer than two \"::\" separators are found."
  (when-let ((pos1 (string-search "::" path)))
    (let ((rest (substring path (+ pos1 2))))
      (when-let ((pos2 (string-search "::" rest)))
        (list :project-dir (substring path 0 pos1)
              :session-id (substring rest 0 pos2)
              :agent-id (substring rest (+ pos2 2)))))))

;;; Helpers

(defun agent-shell-ol--find-buffer-by-session-id (session-id)
  "Find a live agent-shell buffer whose session matches SESSION-ID.
Return the buffer or nil."
  (and (not (string-empty-p session-id))
       (seq-find
        (lambda (buf)
          (and (buffer-live-p buf)
               (equal session-id
                      (map-nested-elt
                       (buffer-local-value 'agent-shell--state buf)
                       '(:session :id)))))
        (agent-shell-buffers))))

(defun agent-shell-ol--find-config (agent-id)
  "Find the agent config matching AGENT-ID in `agent-shell-agent-configs'.
AGENT-ID is a string; configs store identifiers as symbols."
  (and agent-id
       (not (string-empty-p agent-id))
       (let ((sym (intern agent-id)))
         (seq-find (lambda (config)
                     (eq (map-elt config :identifier) sym))
                   agent-shell-agent-configs))))

;;; Store link

(defun agent-shell-ol-store-link ()
  "Store an org link to the current `agent-shell' buffer.
Return the link string on success, nil otherwise."
  (when (derived-mode-p 'agent-shell-mode)
    (let* ((project-dir (expand-file-name default-directory))
           (session-id (or (and (boundp 'agent-shell--state)
                                agent-shell--state
                                (map-nested-elt agent-shell--state
                                                '(:session :id)))
                           ""))
           (agent-id (or (and (boundp 'agent-shell--state)
                              agent-shell--state
                              (let ((id (map-nested-elt
                                         agent-shell--state
                                         '(:agent-config :identifier))))
                                (and id (symbol-name id))))
                         ""))
           (buf-name (buffer-name))
           (link (concat "agent-shell:" project-dir
                         "::" session-id
                         "::" agent-id)))
      (org-link-store-props
       :type "agent-shell"
       :link link
       :description buf-name)
      link)))

;;; Follow link

(defun agent-shell-ol-follow (path _arg)
  "Follow an agent-shell link with PATH.
If a buffer with the stored session ID is live, switch to it.
If the session ID is non-empty but no buffer exists, resume the
session.  If the session ID is empty, offer to spawn a new one."
  (if-let ((parsed (agent-shell-ol--parse-path path)))
      (let ((project-dir (plist-get parsed :project-dir))
            (session-id (plist-get parsed :session-id))
            (agent-id (plist-get parsed :agent-id)))
        (cond
         ;; Find existing buffer by session ID.
         ((and (not (string-empty-p session-id))
               (when-let ((buf (agent-shell-ol--find-buffer-by-session-id
                                session-id)))
                 (pop-to-buffer buf)
                 t)))
         ;; Resume session.
         ((not (string-empty-p session-id))
          (let* ((default-directory project-dir)
                 (config (or (agent-shell-ol--find-config agent-id)
                             (agent-shell--resolve-preferred-config)
                             (agent-shell-select-config
                              :prompt "Resume with agent: "))))
            (agent-shell-start :config config :session-id session-id)))
         ;; Spawn fresh (empty session-id).
         (t
          (if (y-or-n-p "No session ID stored. Spawn a new agent?")
              (let ((default-directory project-dir)
                    (agent-shell-session-strategy 'new))
                (let ((config (or (agent-shell-ol--find-config agent-id)
                                  (agent-shell--resolve-preferred-config)
                                  (agent-shell-select-config
                                   :prompt "Spawn with agent: "))))
                  (agent-shell-start :config config)))
            (message "Link not followed.")))))
    (user-error "Invalid agent-shell link: %s" path)))

;;; Registration

(org-link-set-parameters "agent-shell"
                         :follow #'agent-shell-ol-follow
                         :store #'agent-shell-ol-store-link)

(provide 'agent-shell-ol)
;;; agent-shell-ol.el ends here
