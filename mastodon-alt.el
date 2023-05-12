;;; mastodon-alt.el --- Mastodon alternative timeline -*- lexical-binding: t -*-

;; Copyright (C) 2023 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Created: January 9, 2023
;; Version: 0.0.1
;; Homepage: https://github.com/rougier/mastodon-alt
;; Package-Requires: ((emacs "28.1") (mastodon "1.0.0"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Alternative mastodon timeline layout
;;
;; Known bugs:
;;  - When name + handle combination is too long, date will continue
;;    on next line
;;  - When the toot is boosted or favorited, this mess up with prev/next.
;;    Problem disappear if the timeline is reloaded.
;;    -> mastodon-toot--action needs to be modified such as to add the
;;       'toot-base-id' (that is used to identify toots region)

;;; Code:
(require 'mastodon)
(require 'mastodon-tl)
(require 'mastodon-media)

(defgroup mastodon-alt-tl nil
  "Setting group for mastodon alternative timeline."
  :group 'mastodon)

(defface mastodon-alt-tl-separator-face
  `((t :inherit default
       :strike-through t
       :overline nil
       :underline nil
       :height 1.0
       :extend t))
  "Face for line separating toots."
  :group 'mastodon-alt-tl)

(defface mastodon-alt-tl-update-face
  `((t :inherit (error bold)
       :height 1.0
       :extend t))
  "Face for line separating new from old toots."
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-status-faces
  '((default . font-lock-comment-face)
    (active  . font-lock-comment-face)
    (user    . bold))
  "Faces for status depending on their value.

 Default is for when count is 0, active is for when count is
 greater than zero and user is for when user has favourited,
 boosted or bookmarked the toot."
  :group 'mastodon-alt-tl
  :type '(alist :key-type
          symbol :value-type face))

(defface mastodon-alt-tl-actions-face
  `((t :inherit font-lock-comment-face))
  "Face for toot actions (bottom left)."
  :group 'mastodon-alt-tl)

(defface mastodon-alt-tl-timestamp-face
  `((t :inherit default))
  "Face for toot timestamp."
  :group 'mastodon-alt-tl)

(defface mastodon-alt-tl-box-face
  `((t :inherit default ;;font-lock-comment-face
       :extend t))
  "Face for boxes outline."
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-show-separator t
  "Whether to show separation between toots."
  :type 'bool
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-show-status t
  "Whether to show toot status."
  :type 'bool
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-show-actions t
  "Whether to show toot related actions."
  :type 'bool
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-show-timestamp t
  "Whether to show toot timestamp."
  :type 'bool
  :group 'mastodon-alt-tl)


(defcustom mastodon-alt-tl-shorten-url t
  "Whether to shorten url in toots."
  :type 'bool
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-box-boosted t
  "Whether to enclose boosted toots in a text box."
  :type 'bool
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-box-width -2
  "Width of the box for boosted toots.

A negative value width means `window-width - width'"
  :type 'number
  :group 'mastodon-alt-tl)

(defcustom mastodon-alt-tl-box-prefix " "
  "Prefix to add in front of a box for boosted toots."
  :type 'string
  :group 'mastodon-alt-tl)

(defun mastodon-alt-tl--shorten-url-format (host _name ext)
  "Format a shorten url using HOST and EXT.

NAME is unused."

  (if (stringp ext)
      (format "[%s (%s)]" host ext)
    (format "[%s]" host)))

(defun mastodon-alt-tl--shorten-url (string &optional display)
  "Shorten all urls in STRING.

If DISPLAY is t, the returned string is propertized and uses the
display property to show the short url."

  (with-temp-buffer
    (insert string)

    ;; WARNING: We get rid of display properties because it messes
    ;;          everything We could probably be less radical but
    ;;          it'll do for the time being.
    ;; (add-text-properties (point-min) (point-max) '(display nil))
    (remove-text-properties (point-min) (point-max) '(display))

    (goto-char (point-min))
    (while (re-search-forward "\\(Media:: \\)*\\(http[s]*:[^() \\\n\"]*\\)" nil t)
      (let* ((match (match-string 2))
             (props (text-properties-at (match-beginning 0)))
             (url (save-match-data
                    (url-generic-parse-url match)))
             (host (url-host url))
             (file (url-filename url))
             (ext (save-match-data
                    (when (file-name-extension file)
                      (car (split-string (file-name-extension file) "?")))))
             (name (file-name-base file))
             (short (mastodon-alt-tl--shorten-url-format host name ext))
             (props (if display (append `(display ,short) props)
                      props))
             (short (if display match short)))
        (set-text-properties 0 (length short) props short)
        (when (or (not mastodon-tl--display-media-p)
                  (eq 0 (length (match-string 1))))
          (replace-match short))))
    (buffer-substring (point-min) (point-max))))

(defun mastodon-alt-tl--folding-box (content &optional size title folded prefix style)
  "Enclose CONTENT text with a box of given STYLE and SIZE.

If TITLE is given, then place the optional TITLE at the top.

If a PREFIX is given, it is prepended to the box such that total
size is enforced, including prefix. If a title is given, the
content of the box can be shown/hidden by clicking on the title
and initial state is specified with FOLDED. The type of the box
can be either 'unicode, 'ascii or 'unicode-x with x in [1,7]."

  (let* ((prefix (or prefix ""))
         (tl-box-fg (face-attribute 'mastodon-alt-tl-box-face :foreground nil t))
         (styles `((unicode   . ,(propertize "│┌─┐└─┘│" 'face 'mastodon-alt-tl-box-face))
                   (unicode-1 . ,(propertize "│┌─┐└─┘│" 'face 'mastodon-alt-tl-box-face))
                   (unicode-2 . ,(propertize "┃┏━┓┗━┛┃" 'face 'mastodon-alt-tl-box-face))
                   (unicode-3 . ,(propertize "║╔═╗╚═╝║" 'face 'mastodon-alt-tl-box-face))
                   (unicode-4 . ,(propertize " ┌ ┐└ ┘ " 'face 'mastodon-alt-tl-box-face))
                   (unicode-5 . ,(propertize "│┌  └   " 'face 'mastodon-alt-tl-box-face))
                   (unicode-6 . ,(propertize "┃┏  ┗   " 'face 'mastodon-alt-tl-box-face))
                   (unicode-7 . ,(propertize "║╓  ╙   " 'face 'mastodon-alt-tl-box-face))
                   (ascii     . ,(propertize "|+-++-+|" 'face 'mastodon-alt-tl-box-face))
                   (tight     . ,(concat
                                  (propertize " " 'face `(:height .1
                                                          :background ,tl-box-fg))
                                  (propertize " " 'face '(:underline t))
                                  (propertize " " 'face '(:underline t))
                                  (propertize " " 'face '(:height .1
                                                          :underline t))
                                  (propertize " " 'face '(:overline t))
                                  (propertize " " 'face '(:overline t))
                                  (propertize " " 'face '(:height .1
                                                          :overline t))
                                  (propertize " " 'face `(:height .1
                                                          :background ,tl-box-fg))))))
         (style (or style 'tight))
         (style (if (alist-get style styles)
                    style
                  'ascii))
         (size (or size -1))
         (size (cond ((and size (> size 0)) size)
                     ((and size (< size 0)) (+ (window-width) size))
                     (t (- (window-size) 1))))
         (content (with-temp-buffer
                    (when title (insert "\n"))
                    (insert content)
                    (goto-char (point-min))
                    ;; Extra 2 to have room for the filler
                    (let ((fill-column (- size (length prefix) 4 2))
                          (sentence-end-double-space nil))
                      (fill-region (point-min) (point-max)))
                    (buffer-substring (point-min) (point-max))))
         (fill-title (propertize " "
                                 'display `(space :align-to ,(- size 1))))
         (fill-body  (propertize " "
                                 'display `(space :align-to ,(- size 2))))
         (keymap (let ((map (make-sparse-keymap)))
                   (define-key map [mouse-2] #'mastodon-alt-tl--folding-box-toggle)
                   (define-key map (kbd "c") #'mastodon-alt-tl--folding-box-toggle)
                   (define-key map (kbd "RET") #'mastodon-alt-tl--folding-box-toggle)
                   map))
         (box-L  (substring (alist-get style styles) 0 1))
         (box-TL (substring (alist-get style styles) 1 2))
         (box-T  (substring (alist-get style styles) 2 3))
         (box-TR (substring (alist-get style styles) 3 4))
         (box-BL (substring (alist-get style styles) 4 5))
         (box-B  (substring (alist-get style styles) 5 6))
         (box-BR (substring (alist-get style styles) 6 7))
         (box-R  (substring (alist-get style styles) 7 8))

         (body-format (concat prefix
                              box-L
                              "  "
                              (propertize "%s" 'face 'default)
                              fill-body
                              "  "
                              box-R
                              "\n"))
         (header (concat prefix
                         box-TL
                         (propertize
                          (make-string (- size (length prefix) 1)
                                       (string-to-char box-T))
                          'face (get-char-property 0 'face box-T))
                         box-TR
                         "\n"
                         (when (eq style 'tight)
                           (format body-format ""))))
         (footer (concat (when (eq style 'tight)
                           (format body-format ""))
                         prefix
                         box-BL
                         (propertize
                          (make-string (- size (length prefix) 1)
                                       (string-to-char box-B))
                          'face (get-char-property 0 'face box-B))
                         box-BR))
         (body (mapconcat (lambda (line)
                            (format body-format line))
                          (split-string content "[\n]") ""))
         (title (when title (truncate-string-to-width title
                                                      (- size (length prefix) 4) nil nil "…")))
         (title (when title (concat prefix
                                    box-L
                                    "  "
                                    (propertize (concat title fill-title)
                                                'face 'bold
                                                'mouse-face 'highlight
                                                'body-size (length body)
                                                'follow-link t
                                                'help-echo "Click to fold/unfold box content"
                                                'keymap keymap)
                                    " "
                                    box-R
                                    "\n")))
         (body (propertize body 'invisible folded)))
    (concat header (or title "")  body footer)))

(defun mastodon-alt-tl--folding-box-toggle ()
  "Fold / unfold a folding box."

  (interactive)
  (let ((inhibit-read-only t)
        (body-size (get-text-property (point) 'body-size)))
    (when body-size
      (save-excursion
        (end-of-line)
        (let ((invisible (not (get-text-property (+ (point) 1) 'invisible))))
          (add-text-properties (+ (point) 1) (+ (point) body-size 1)
                               `(invisible ,invisible)))))))


(defun mastodon-alt-tl--toggle-spoiler-text-in-toot (_orig-fun)
  "Toggle the visibility of the spoiler text in the current toot."
  (interactive)
  (let ((bounds (mastodon-alt-tl--bounds 'body-size)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (mastodon-alt-tl--folding-box-toggle)))))


(defun mastodon-alt-tl--mark-update ()
  "Add a mark update at the top of a timeline.

This should be ran just before an update."

  (remove-overlays (point-min) (point-max) 'mastodon-alt-update t)
  (let* ((overlay (make-overlay (point-min) (+ (point-min) 1) nil t))
         (width (- (window-width) 1))
         (update (format-time-string " Update %H:%M "))
         (line (concat
                (propertize "\n" 'face `(:foreground ,(face-background 'default)
                                         :extend t
                                         :strike-through t))
                (propertize (make-string (/ (- width (length update) 3) 2) ? )
                            'face '(:inherit mastodon-alt-tl-update-face
                                    :strike-through t))
                (propertize update
                            'face '(:inherit mastodon-alt-tl-update-face
                                    :inverse-video t
                                    :strike-through nil))
                (propertize "\n"
                            'face '(:inherit mastodon-alt-tl-update-face
                                    :strike-through t))
                (propertize "\n" 'face `(:foreground ,(face-background 'default)
                                         :extend t
                                         :strike-through t)))))
    (overlay-put overlay 'mastodon-alt-update t)
    (overlay-put overlay 'display line)))

(defun mastodon-alt-tl--status-face (user count)
  "Return a status face depending on USER and COUNT."

  (cond (user        (alist-get 'user    mastodon-alt-tl-status-faces))
        ((> count 0) (alist-get 'active  mastodon-alt-tl-status-faces))
        (t           (alist-get 'default mastodon-alt-tl-status-faces))))

(defun mastodon-alt-tl--status-toot (&optional toot)
  "Return the TOOT on which we want to extract status.

If no TOOT is given, the one at point is considered."

  (let* ((original-toot (or toot (get-text-property (point) 'toot-json)))
         (toot (or (alist-get 'status original-toot)
                   (when (alist-get 'type original-toot)
                     original-toot)
                   (alist-get 'reblog original-toot)
                   original-toot))
         (type (alist-get 'type (or toot))))
    (unless (member type '("follow" "follow_request"))
      toot)))


(defun mastodon-alt-tl--bounds (property)
  "Get the bounds of the specified PROPERTY for the toot at point."

  (save-excursion
    (let ((toot-id (get-char-property (point) 'toot-id)))
      (beginning-of-line)
      (when-let* ((beg (next-single-property-change (point) property))
                  (end (next-single-property-change beg property))
                  (same (eq toot-id (get-char-property beg 'toot-id))))
        (cons beg end)))))

;; We could add +1/-1 to the count and use the display property to
;; show a count including our own
(defun mastodon-alt-toot--toggle-boost (_orig-fun)
  "Toggle boosted status for the toot at point."

    (when-let* ((inhibit-read-only t)
                (toot (mastodon-alt-tl--status-toot)))
      (when-let* ((bounds (mastodon-alt-tl--bounds 'boosts-field)))
        (let ((boosted-p (not (get-char-property (car bounds) 'boosted-p)))
              (count (alist-get 'reblogs_count toot)))

          (mastodon-toot--action (if boosted-p "reblog" "unreblog")
                                 (lambda ()))

          (add-text-properties (car bounds) (cdr bounds)
               `(boosted-p ,boosted-p
                 face ,(mastodon-alt-tl--status-face boosted-p count)))))))

;; We could add +1/-1 to the count and use the display property to
;; show a count including our onw
(defun mastodon-alt-toot--toggle-favourite (_orig-fun)
  "Toggle boosted status for the toot at point."

  (when-let* ((inhibit-read-only t)
              (toot (mastodon-alt-tl--status-toot)))
    (when-let* ((bounds (mastodon-alt-tl--bounds 'favourites-field)))
      (let ((favourited-p (not (get-char-property (car bounds) 'favourited-p)))
            (count (alist-get 'reblogs_count toot)))
        (mastodon-toot--action (if favourited-p "favourite" "unfavourite")
                               (lambda ()))
        (add-text-properties (car bounds) (cdr bounds)
             `(favourited-p ,favourited-p
                            face ,(mastodon-alt-tl--status-face favourited-p count)))))))

(defun mastodon-alt-toot--bookmark-toot-toggle (_orig-fun)
  "Toggle bookmark for the toot at point."

    (when-let* ((inhibit-read-only t)
                (toot (mastodon-alt-tl--status-toot)))
      (when-let* ((bounds (mastodon-alt-tl--bounds 'bookmark-field)))
        (let ((bookmarked-p (not (get-char-property (car bounds) 'bookmarked-p))))
          (mastodon-toot--action (if bookmarked-p "bookmark" "unbookmark")
                                 (lambda ()))
          (add-text-properties (car bounds) (cdr bounds)
               `(bookmarked-p ,bookmarked-p
                 face ,(mastodon-alt-tl--status-face bookmarked-p 0)))))))

(defun mastodon-alt-tl--update (orig-fun &rest args)
  "Advice to insert a mark update before getting more toots.

This advisizes ORIG-FUN `mastodon-tl--update' and applies ARGS."
  (save-excursion
    (mastodon-alt-tl--mark-update)
    (apply orig-fun args)))

(defun mastodon-alt-tl--more (orig-fun &rest args)
  "Advice to insert a mark update before getting more toots.

This advisizes ORIG-FUN `mastodon-tl--more' and applies ARGS."
  (save-excursion
    (mastodon-alt-tl--mark-update)
    (apply orig-fun args)))

(defun mastodon-alt-tl--goto-prev-toot (_orig-fun &rest _args)
  "Jump to previous toot header."
  (interactive)

  (let ((property 'toot-id))
    (let ((prev (previous-single-property-change (point) property)))
      (if (not prev)
          (progn
            (mastodon-tl--update)
            (goto-char (+ (point-min) 1)))
        (goto-char (- prev 1))))

    (let ((prev (previous-single-property-change (point) property)))
      (if (not prev)
          (goto-char (+ (point-min) 1))
        (goto-char (+ prev 1))))))

(defun mastodon-alt-tl--goto-next-toot (_orig-fun &rest _args)
  "Jump to next toot header."
  (interactive)

  (let* ((property 'toot-id)
         (next (next-single-property-change (point) property)))
    (if (or (not next) (> (+ next 2) (point-max)))
        (progn
          (goto-char (point-max))
          (mastodon-tl--more))
      (goto-char (+ next 2)))))


(defun mastodon-alt-tl--toot-status (toot)
  "Return a right aligned string (using display align-to).

String is filled with TOOT statistics (boosts, favs, replies and
bookmark). When the TOOT is a reblog (boost), statistics from
reblogged toots are returned.

To disable showing the status string at all, customize
`mastodon-alt-tl-show-status'."

  (when mastodon-alt-tl-show-status
    (when-let* ((toot (mastodon-alt-tl--status-toot toot)))
      (let* ((favourites-count (alist-get 'favourites_count toot))
             (favourited (equal 't (alist-get 'favourited toot)))
             (boosts-count (alist-get 'reblogs_count toot))
             (boosted (equal 't (alist-get 'reblogged toot)))
             (replies-count (alist-get 'replies_count toot))
             (favourites (format "%s %s" favourites-count (mastodon-tl--symbol 'favourite)))
             (boosts (format "%s %s" boosts-count (mastodon-tl--symbol 'boost)))
             (replies (format "%s %s" replies-count (mastodon-tl--symbol 'reply)))
             (bookmark (format "%s" (mastodon-tl--symbol 'bookmark)))
             (bookmarked (equal 't (alist-get 'bookmarked toot)))
             (status (concat
                      (propertize favourites
                                  'favourited-p favourited
                                  'favourites-field t
                                  'favourites-count favourites-count
                                  'face (mastodon-alt-tl--status-face favourited favourites-count))
                      (propertize " | " 'face (alist-get 'default mastodon-alt-tl-status-faces))
                      (propertize boosts
                                  'boosted-p boosted
                                  'boosts-field t
                                  'boosts-count boosts-count
                                  'face (mastodon-alt-tl--status-face boosted boosts-count))
                      (propertize " | " 'face (alist-get 'default mastodon-alt-tl-status-faces))
                      (propertize replies
                                  'replies-field t
                                  'replies-count replies-count
                                  'face (mastodon-alt-tl--status-face nil replies-count))
                      (propertize " | " 'face (alist-get 'default mastodon-alt-tl-status-faces))
                      (propertize bookmark
                                  'bookmark-field t
                                  'face (mastodon-alt-tl--status-face bookmarked 0))))
             (status (concat
                      (propertize " " 'display `(space :align-to (- right ,(+ (length status) 2))))
                      status)))
        status))))


(defun mastodon-alt-tl--relative-time-details (orig-fun timestamp &optional current-time)
  "Advice to make sure relative time is 12 characters long.

This advisizes ORIG-FUN `mastodon-tl--relative-time-details' and
applies TIMESTAMP and CURRENT-TIME."

  (let* ((result (apply orig-fun timestamp current-time))
         (result (cons (replace-regexp-in-string "minutes" "mins" (car result))
                       (cdr result))))
    (cons (format "%12s" (car result)) (cdr result))))

(defun mastodon-alt-tl--toot-timestamp (toot)
  "Return a right aligned (using display align-to) string with TOOT timestamp."

  (let* ((created-time (or (mastodon-tl--field 'created_at
                                               (mastodon-tl--field 'status toot))
                           (mastodon-tl--field 'created_at toot)))
         (parsed-time (date-to-time created-time))
         (absolute (format-time-string
                    mastodon-toot-timestamp-format parsed-time))
         (relative (format "%12s"
                           (mastodon-tl--relative-time-description parsed-time))))
    (if mastodon-tl--enable-relative-timestamps
        (concat (propertize " "
                            'display `(space :align-to (- right ,(length relative) 2)))
                (propertize relative
                            'display relative
                            'face 'mastodon-alt-tl-timestamp-face
                            'timestamp parsed-time))
      (concat (propertize " "
                          'display `(space :align-to (- right ,(length absolute) 2 )))
              (propertize absolute
                          'display nil
                          'face 'mastodon-alt-tl-timestamp-face
                          'timestamp parsed-time)))))

(defun mastodon-alt-tl--toot-actions (_toot)
  "Return a string with TOOT actions."

  (if mastodon-alt-tl-show-actions
      (concat " "
              (propertize "follow"
                          'face 'mastodon-alt-tl-actions-face
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-2]   #'mastodon-tl--follow-user)
                                    (define-key map (kbd "RET") #'mastodon-tl--follow-user)
                                    map)
                          'mouse-face 'highlight
                          'follow-link t
                          'help-echo "Click to follow user")
              (propertize " | "
                          'face 'mastodon-alt-tl-actions-face)
              (propertize "reply"
                          'face 'mastodon-alt-tl-actions-face
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-2]   #'mastodon-toot--reply)
                                    (define-key map (kbd "RET") #'mastodon-toot--reply)
                                    map)
                          'mouse-face 'highlight
                          'follow-link t
                          'help-echo "Click to reply")
              (propertize " | "
                          'face 'mastodon-alt-tl-actions-face)
              (propertize "mute"
                          'face 'mastodon-alt-tl-actions-face
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-2]   #'mastodon-tl--mute-user)
                                    (define-key map (kbd "RET") #'mastodon-tl--mute-user)
                                    map)
                          'mouse-face 'highlight
                          'follow-link t
                          'help-echo "Click to mute user")
              (propertize " | "
                          'face 'mastodon-alt-tl-actions-face)
              (propertize "block"
                          'face 'mastodon-alt-tl-actions-face
                          'keymap (let ((map (make-sparse-keymap)))
                                    (define-key map [mouse-2]   #'mastodon-tl--block-user)
                                    (define-key map (kbd "RET") #'mastodon-tl--block-user)
                                    map)
                          'mouse-face 'highlight
                          'follow-link t
                          'help-echo "Click to mute user"))
    ""))

(defun mastodon-alt-tl--toot-separator (_toot)
  "Return a separator string."

   (propertize "\n" 'face 'mastodon-alt-tl-separator-face
                    'toot-start t))

(defun mastodon-alt-tl--toot-content (toot)
  "Return a string with TOOT content."

  (let* ((reblog  (alist-get 'reblog toot))
         (content (mastodon-tl--content toot))
         (content (mastodon-tl--clean-tabs-and-nl content))
         (content (if mastodon-alt-tl-shorten-url
                      (mastodon-alt-tl--shorten-url content)
                    content)))
    (cond ((eq 0 (length content))
           "")

          ((mastodon-tl--has-spoiler toot)
           (concat
            (mastodon-alt-tl--folding-box content
                                          mastodon-alt-tl-box-width
                                          (mastodon-tl--field 'spoiler_text toot)
                                          mastodon-toot--content-warning
                                          mastodon-alt-tl-box-prefix)
            "\n"))

          ((and reblog mastodon-alt-tl-box-boosted)
           (concat
            (mastodon-alt-tl--folding-box content
                                          mastodon-alt-tl-box-width
                                          nil
                                          nil
                                          mastodon-alt-tl-box-prefix)
           "\n"))

          (t
           (concat
            "\n"
            (string-fill content (min (- (window-width) 2) fill-column))
           "\n\n")))))

(defun mastodon-alt-tl--insert-status (_orig-fun toot _body author-byline action-byline
                                                 &optional id parent-toot detailed-p thread)
  "Advice to replace the insertion of a TOOT with byline first.

The arguments AUTHOR-BYLINE, ACTION-BYLINE, ID, PARENT-TOOT, and
DETAILED-P are the same as the original wrapped function
`mastodon-tl--insert-status'."

  (let ((beg (point)))
    (insert
     (propertize
      (concat
       (mastodon-alt-tl--toot-separator toot)
       (mastodon-tl--byline toot author-byline action-byline detailed-p)
       ;; Add linebreak if reblogged (to work with
       ;; `mastodon-alt-tl--byline-boosted'), if not showing avatars,
       ;; or if the image is short enough.
       (when (or (alist-get 'reblog toot)
                 (not mastodon-tl--show-avatars)
                 (mastodon-tl--has-spoiler toot)
                 (< mastodon-media--avatar-height (frame-char-height)))
         "\n")
       (mastodon-alt-tl--toot-content toot)
       (mastodon-alt-tl--toot-actions toot)
       (mastodon-alt-tl--toot-status toot))
      'toot-id      (or id (alist-get 'id toot))
      'base-toot-id (mastodon-tl--toot-id (or parent-toot toot))
      'toot-json    toot
      'parent-toot  parent-toot)
     "\n")

    (when mastodon-tl--display-media-p
      (mastodon-media--inline-images beg (point)))))

(defun mastodon-alt-tl--byline-boosted (_orig-fun toot)
  "Add byline for boosted data from TOOT."

  (let ((reblog (alist-get 'reblog toot)))
    (when reblog
      (concat
       "\n "
       (propertize "boosted" 'face 'mastodon-boosted-face)
       " "
       (mastodon-tl--byline-author reblog)))))


(defun mastodon-alt-tl--byline (_orig-fun toot author-byline action-byline
                                          &optional _detailed-p)
  "Generate byline for TOOT.

AUTHOR-BYLINE is a function for adding the author portion of
the byline that takes one variable.
ACTION-BYLINE is a function for adding an action, such as boosting,
favoriting and following to the byline. It also takes a single function.
By default it is `mastodon-tl--byline-boosted'.

DETAILED-P means display more detailed info. For now
this just means displaying toot client."
  (let* ((faved (equal 't (mastodon-tl--field 'favourited toot)))
         (boosted (equal 't (mastodon-tl--field 'reblogged toot)))
         (bookmarked (equal 't (mastodon-tl--field 'bookmarked toot)))
         (visibility (mastodon-tl--field 'visibility toot))
         (account (alist-get 'account toot))
         (avatar-url (alist-get 'avatar account))
         (edited-time (alist-get 'edited_at toot)))
    (concat
     (when (and mastodon-tl--show-avatars
                mastodon-tl--display-media-p
                (if (version< emacs-version "27.1")
                    (image-type-available-p 'imagemagick)
                  (image-transforms-p)))
       (mastodon-media--get-avatar-rendering avatar-url))
     (propertize
      (concat
       ;; we propertize help-echo format faves for author name
       ;; in `mastodon-tl--byline-author'
       (funcall author-byline toot)
       ;; visibility:
       (cond ((equal visibility "direct")
              (concat " " (mastodon-tl--symbol 'direct)))
             ((equal visibility "private")
              (concat " " (mastodon-tl--symbol 'private))))
       (funcall action-byline toot)
       " "
       (mastodon-alt-tl--toot-timestamp toot))
      'favourited-p faved
      'boosted-p    boosted
      'bookmarked-p bookmarked
      'edited edited-time
      'edit-history (when edited-time
                      (mastodon-toot--get-toot-edits (alist-get 'id toot)))
      'byline       t))))


;; Advice to use our thumbnail making code
(defun  mastodon-alt-media--process-image-response
    (_orig-fun status-plist marker image-options region-length url)
  "Callback function processing the url retrieve response for URL.

STATUS-PLIST is the usual plist of status events as per `url-retrieve'.
IMAGE-OPTIONS are the precomputed options to apply to the image.
MARKER is the marker to where the response should be visible.
REGION-LENGTH is the length of the region that should be replaced
with the image."
  (when (marker-buffer marker) ; only if the buffer hasn't been kill in the meantime
    (let ((url-buffer (current-buffer))
          (is-error-response-p (eq :error (car status-plist))))
      (unwind-protect
          (let* ((data (unless is-error-response-p
                         (goto-char (point-min))
                         (search-forward "\n\n")
                         (buffer-substring (point) (point-max))))
                 (image (when data
                          (apply #'create-image data
                                 (if (version< emacs-version "27.1")
                                     (when image-options 'imagemagick)
                                   nil) ; inbuilt scaling in 27.1
                                 t image-options))))
            (when mastodon-media--enable-image-caching
              (unless (url-is-cached url) ; cache if not already cached
                (url-store-in-cache url-buffer)))
            (with-current-buffer (marker-buffer marker)
              ;; Save narrowing in our buffer
              (let ((inhibit-read-only t))
                (save-restriction
                  (widen)
                  (put-text-property marker
                                     (+ marker region-length) 'media-state 'loaded)
                  (when image
                    ;; We only set the image to display if we could load
                    ;; it; we already have set a default image when we
                    ;; added the tag.
                    (if (eq (plist-get image-options :height)
                            mastodon-media--avatar-height)
                        (put-text-property marker (+ marker region-length)
                                           'display image)
                      (mastodon-alt-tl--thumbnail marker (+ marker region-length) image)))
                  ;; We are done with the marker; release it:
                  (set-marker marker nil)))
              (kill-buffer url-buffer)))))))


(defun mastodon-alt-tl--thumbnail (start end image)
  "Make a thumbnail of an IMAGE.

Make the region going from START to END to display it and show a
bigger preview when mouse hovers it."

  (let* ((char-height (frame-char-height))
         (char-width (frame-char-width))
         (rows (+ (/ mastodon-media--preview-max-height char-height) 1))
         (crop t)
         (image-width  (car (image-size image t)))
         (image-height (cdr (image-size image t)))
         (image-file (plist-get (cdr image) :file))
         (image-data (plist-get (cdr image) :data))
         (thumbnail-height (* rows char-height))
         (thumbnail-width  (if crop
                               (* (/ thumbnail-height char-width) char-width)
                             1.0))
         (thumbnail (list 'image
                          :type (plist-get (cdr image) :type)))
         (preview (list 'image
                        :type (plist-get (cdr image) :type))))
    (if image-data
        (progn
          (plist-put (cdr thumbnail) :data image-data)
          (plist-put (cdr preview) :data image-data))
      (progn
        (plist-put (cdr thumbnail) :file image-file)
        (plist-put (cdr preview) :file image-file)))

    (if (> image-width image-height)
        (progn
          (plist-put (cdr thumbnail) :height thumbnail-height)
          (plist-put (cdr preview) :width 512)
          (plist-put (cdr preview) :height nil))
      (progn
        (plist-put (cdr thumbnail) :width thumbnail-width)
        (plist-put (cdr preview) :width nil)
        (plist-put (cdr preview) :height 512)))

    (put-text-property start end 'display
                       (list (list 'slice 0 0 thumbnail-width thumbnail-height) thumbnail))
    (put-text-property start end 'face '(:box t))
    (put-text-property start end 'mouse-face '(:box t))
    (put-text-property start end 'help-echo (propertize " " 'display preview))))


(defun mastodon-alt-profile--insert-statuses-pinned (_orig-fun pinned-statuses)
  "Insert each of the PINNED-STATUSES for a given account."
  (mapc (lambda (pinned-status)
          (insert (format "%s " (mastodon-tl--symbol 'pinned)))
          (mastodon-tl--toot pinned-status))
        pinned-statuses))


(defun mastodon-alt-setup ()
  "Some buffer local settings."

  (tooltip-mode 1)
  (setq tooltip-delay 0)
  (setq tooltip-hide-delay 60))


(defun mastodon-alt-tl (state)
  "Activate or deactivate the alternative timeline layout depending on STATE."

  (if state
      (progn
        (add-hook 'mastodon-mode-hook #'mastodon-alt-setup)

        (advice-add 'mastodon-media--process-image-response :around
                    #'mastodon-alt-media--process-image-response)
        (advice-add 'mastodon-tl--toggle-spoiler-text-in-toot :around
                    #'mastodon-alt-tl--toggle-spoiler-text-in-toot)
        (advice-add 'mastodon-profile--insert-statuses-pinned :around
                    #'mastodon-alt-profile--insert-statuses-pinned)
        (advice-add 'mastodon-tl--update :around
                    #'mastodon-alt-tl--update)
        (advice-add 'mastodon-tl--more :around
                    #'mastodon-alt-tl--more)
        (advice-add 'mastodon-tl--goto-prev-toot :around
                    #'mastodon-alt-tl--goto-prev-toot)
        (advice-add 'mastodon-tl--goto-next-toot :around
                    #'mastodon-alt-tl--goto-next-toot)
        (advice-add 'mastodon-tl--insert-status :around
                    #'mastodon-alt-tl--insert-status)
        (advice-add 'mastodon-tl--byline-boosted :around
                    #'mastodon-alt-tl--byline-boosted)
        (advice-add 'mastodon-tl--relative-time-details :around
                    #'mastodon-alt-tl--relative-time-details)
        (advice-add 'mastodon-tl--byline :around
                    #'mastodon-alt-tl--byline)
        (advice-add 'mastodon-toot--bookmark-toot-toggle :around
                    #'mastodon-alt-toot--bookmark-toot-toggle)
        (advice-add 'mastodon-toot--toggle-favourite :around
                    #'mastodon-alt-toot--toggle-favourite)
        (advice-add 'mastodon-toot--toggle-boost :around
                    #'mastodon-alt-toot--toggle-boost))
    (progn
      (remove-hook 'mastodon-mode-hook #'mastodon-alt-setup)

      (advice-remove 'mastodon-media--process-image-response
                     #'mastodon-alt-media--process-image-response)
      (advice-remove 'mastodon-tl--toggle-spoiler-text-in-toot
                     #'mastodon-alt-tl--toggle-spoiler-text-in-toot)
      (advice-remove 'mastodon-profile--insert-statuses-pinned
                     #'mastodon-alt-profile--insert-statuses-pinned)
      (advice-remove 'mastodon-tl--update
                     #'mastodon-alt-tl--update)
      (advice-remove 'mastodon-tl--more
                     #'mastodon-alt-tl--more)
      (advice-remove 'mastodon-tl--goto-prev-toot
                     #'mastodon-alt-tl--goto-prev-toot)
      (advice-remove 'mastodon-tl--goto-next-toot
                     #'mastodon-alt-tl--goto-next-toot)
      (advice-remove 'mastodon-tl--insert-status
                     #'mastodon-alt-tl--insert-status)
      (advice-remove 'mastodon-tl--byline-boosted
                     #'mastodon-alt-tl--byline-boosted)
      (advice-remove 'mastodon-tl--relative-time-details
                     #'mastodon-alt-tl--relative-time-details)
      (advice-remove 'mastodon-tl--byline
                     #'mastodon-alt-tl--byline)
      (advice-remove 'mastodon-toot--bookmark-toot-toggle
                     #'mastodon-alt-toot--bookmark-toot-toggle)
      (advice-remove 'mastodon-toot--toggle-favourite
                     #'mastodon-alt-toot--toggle-favourite)
      (advice-remove 'mastodon-toot--toggle-boost
                     #'mastodon-alt-toot--toggle-boost))))


;;;###autoload
(defun mastodon-alt-tl-activate ()
  "Activate alternative timeline layout (experimental)."

  (interactive)
  (mastodon-alt-tl t))

;;;###autoload
(defun mastodon-alt-tl-deactivate ()
  "Deactivate alternative timeline layout (experimental)."

  (interactive)
  (mastodon-alt-tl nil))


(provide 'mastodon-alt)
;;; mastodon-alt.el ends here
