;;; el-secretario-org.el org-mode module for el-secretario -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo Okawa Ericson <http://github/Zetagon>
;; Maintainer: Leo <github@relevant-information.com>
;; Created: September 20, 2020
;; Modified: October 17, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/Zetagon/el-secretario
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;
;;
;;; Code:

(require 'org-ql)
(defhydra el-secretario-org-hydra ()
  ("n" el-secretario-next-item "next" :exit t)
  ("r" (progn (org-refile) (el-secretario-next-item)) "Refile" :exit t)
  ("t" org-set-tags-command "Tags")
  ("s" org-schedule "Schedule")
  ("d" org-deadline  "Deadline")
  ("D" (delete-region (point-min) (point-max)) "Delete visible")
  ("q" (el-secretario-end-sesion) "Quit" :exit t)
  ("/" nil "disable hydra"  :exit t))

(defmacro el-secretario-org-make-source (query files &optional next-item-hook hydra)
  "QUERY is an arbitrary org-ql query. FILES is the files to search through.
NEXT-ITEM-HOOk is called on each heading.
HYDRA is an hydra to use during review of this source."
  `(make-el-secretario-source
    :init-function  (lambda () (el-secretario-org-init (quote ,query) (quote ,files) ))
    :next-function  #'el-secretario-org-next-item
    :prev-function  #'el-secretario-org-previous-item
    :hydra-body #'el-secretario-org-hydra/body
    :finished-hook #'widen
    :next-item-hook (or ,next-item-hook (lambda ()))
    :local-state-buf ,name))


(defvar el-secretario--org-items-left nil
  "A list of items that should be reviewed")

(defvar el-secretario--org-items-done nil
  "A list of items that has been reviewed")

(defun el-secretario--org-pop-items (type)
  (let* ((buf (el-secretario-source-local-state-buf
               (car el-secretario-current-source-list)))
         (list-type (pcase type
                      ('left
                       'el-secretario--org-items-left)
                      ('done
                       'el-secretario--org-items-done)
                      (default (error "Can't push to this list type: %s" default))))

         (x (el-secretario-get-local buf list-type)))
    (el-secretario-setq-local buf list-type (cdr x))
    (car x)))

(defun el-secretario--org-push-items (type val)
  (let* ((buf (el-secretario-source-local-state-buf
               (car el-secretario-current-source-list)))
         (list-type (pcase type
                      ('left
                       'el-secretario--org-items-left)
                      ('done
                       'el-secretario--org-items-done)))
         (x (el-secretario-get-local buf list-type)))
    (el-secretario-setq-local buf list-type (cons x val))))

(defun el-secretario-org-init (query &optional files)
  "TODO"
  (el-secretario-setq-local (el-secretario-source-local-state-buf
                             (car el-secretario-current-source-list))
                            el-secretario--org-items-left
                            (org-ql-select (or files
                                               (org-agenda-files)) query
                                               :action '(list (current-buffer)
                                                              (point-marker))))
  (el-secretario-setq-local (el-secretario-source-local-state-buf
                             (car el-secretario-current-source-list))
                            el-secretario--org-items-done nil)
  (funcall (el-secretario-source-hydra-body
            (car el-secretario-current-source-list)))
  (el-secretario-org-next-item))

(defun el-secretario-org-next-item ()
  "TODO"

  (if-let ((item (el-secretario--org-pop-items 'left)))
      (cl-destructuring-bind (buf pos) item
        (el-secretario--org-push-items 'done (list buf pos))
        (switch-to-buffer buf)
        (widen)
        (goto-char pos)
        (org-narrow-to-subtree)
        (funcall (el-secretario-source-next-item-hook
                  (car el-secretario-current-source-list)))
        (el-secretario-org-update-status-buffer)
        (funcall (el-secretario-source-hydra-body
                  (car el-secretario-current-source-list))) )
    (message "No next item!")
    (el-secretario--next-source)))

(defvar date nil)
(defun el-secretario-org-update-status-buffer ()
  (interactive)
  (let ((date (calendar-current-date))
        deadlines
        scheduleds)
    (save-excursion
      (setq deadlines (org-agenda-get-deadlines))
      (setq scheduleds (org-agenda-get-scheduled)))
    (with-current-buffer el-secretario-status-buffer-name
      (delete-region (point-min) (point-max))
      (--each deadlines
        (insert "Deadline: " it "\n"))
      (--each scheduleds
        (insert "Scheduled: " it "\n")))))

  (defun el-secretario-org-previous-item ()
    "TODO Implement this")

(defun el-secretario-org-add-tag (&rest tags)
  "Add TAGS to headline."
  (org-set-tags (cl-remove-duplicates
                 (append tags (or (org-get-tags nil 't)
                                  '()))
                 :test #'string-equal)))

(defun el-secretario-org-remove-tag (&rest tags)
  "Add TAGS to headline."
  (org-set-tags (--keep (string-equal tags it) (org-get-tags nil 't))))

(provide 'el-secretario-org)
;;; el-secretario-org.el ends here
