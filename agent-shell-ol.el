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
;;   [[agent-shell:/path/to/project::Buffer Name][Buffer Name]]
;;
;; Following such a link switches to the buffer if it is live, or
;; offers to spawn a new agent session in the stored project directory.
;;
;; Usage:
;;
;;   (require 'agent-shell-ol)

;;; Code:

(require 'ol)
(require 'agent-shell)

;;; Path parsing

(defun agent-shell-ol--parse-path (path)
  "Parse PATH of the form \"project-dir::buffer-name\".
Split on the first occurrence of \"::\".  Return a cons cell
\(PROJECT-DIR . BUFFER-NAME), or nil if no \"::\" separator is found."
  (when-let ((pos (string-search "::" path)))
    (cons (substring path 0 pos)
          (substring path (+ pos 2)))))

;;; Store link

(defun agent-shell-ol-store-link ()
  "Store an org link to the current `agent-shell' buffer.
Return the link string on success, nil otherwise."
  (when (derived-mode-p 'agent-shell-mode)
    (let* ((project-dir (expand-file-name default-directory))
           (buf-name (buffer-name))
           (link (concat "agent-shell:" project-dir "::" buf-name)))
      (org-link-store-props
       :type "agent-shell"
       :link link
       :description buf-name)
      link)))

;;; Follow link

(defun agent-shell-ol-follow (path _arg)
  "Follow an agent-shell link with PATH.
If the target buffer is live, switch to it.  Otherwise, offer to
spawn a new agent session in the stored project directory."
  (if-let ((parsed (agent-shell-ol--parse-path path)))
      (let ((project-dir (car parsed))
            (buf-name (cdr parsed)))
        (if-let ((buf (get-buffer buf-name))
                 (_live (buffer-live-p buf)))
            (pop-to-buffer buf)
          (if (y-or-n-p (format "Agent \"%s\" not found. Spawn a new one?" buf-name))
              (let ((default-directory project-dir)
                    (agent-shell-session-strategy 'new))
                (let ((config (or (agent-shell--resolve-preferred-config)
                                  (agent-shell-select-config
                                   :prompt "Spawn with agent: "))))
                  (agent-shell-start :config config)))
            (message "Link target not available: %s" buf-name))))
    (user-error "Invalid agent-shell link: %s" path)))

;;; Registration

(org-link-set-parameters "agent-shell"
                         :follow #'agent-shell-ol-follow
                         :store #'agent-shell-ol-store-link)

(provide 'agent-shell-ol)
;;; agent-shell-ol.el ends here
