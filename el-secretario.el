;;; el-secretario.el General interface for el-secretario -*- lexical-binding: t; -*-
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




(defhydra el-secretario-default-hydra ()
  ("n" el-secretario-next-item "next" :exit t)
  ("q" (el-secretario-end-sesion) "Quit" :exit t)
  ("/" nil "disable hydra"  :exit t))

(defun el-secretario-activate-hydra ()
  (interactive)
  (when el-secretario-current-source-list
    (funcall (el-secretario-source-hydra-body
              (car el-secretario-current-source-list)))))


(cl-defstruct el-secretario-source
  init-function
  next-function
  prev-function
  hydra-body
  finished-hook
  next-item-hook
  local-state-buf)

(defvar el-secretario-current-source-list nil
  "TODO")

(defvar el-secretario-current-source-list-done nil
  "TODO")

(defvar el-secretario-status-buffer-name "*el-secretario-status*"
  "TODO")
(defvar el-secretario--original-buffer nil
  "The buffer the user was in before activating el-secretario.")

(defhydra el-secretario--hydra-quit (:exit t
                        :foreign-keys run)
  ("q"  (when el-secretario--original-buffer
          (switch-to-buffer el-secretario--original-buffer)) "Quit"))

;;;###autoload
(defun el-secretario-start-session (source-list)
  (setq el-secretario--original-buffer (current-buffer))
  (setq el-secretario-current-source-list source-list)
  (with-current-buffer (get-buffer-create "*el-secretario-en*")
    (delete-region (point-min) (point-max)))
  (funcall (el-secretario-source-init-function (car source-list)))
  (el-secretario-status-buffer-activate))

(defun el-secretario-end-sesion ()
  (switch-to-buffer el-secretario--original-buffer)
  (el-secretario-status-buffer-deactivate))

(defun el-secretario-next-item ()
  (interactive)
  (when el-secretario-current-source-list
    (funcall (el-secretario-source-next-function
              (car el-secretario-current-source-list)))))

(defun el-secretario-prev-item ()
  (interactive)
  (when el-secretario-current-source-list
    (funcall (el-secretario-source-prev-function
              (car el-secretario-current-source-list)))))

(defun el-secretario--next-source ()
  "TODO"
  (el-secretario--next-prev-source))
(defun el-secretario--prev-source ()
  "TODO"
  (el-secretario--next-prev-source t))

(defun el-secretario--next-prev-source (prev)
  "TODO"
  (if el-secretario-current-source-list
      (let ((next-list (if prev
                           el-secretario-current-source-list-done
                         el-secretario-current-source-list))
            (prev-list (if (not prev)
                           el-secretario-current-source-list-done
                         el-secretario-current-source-list)))
        (progn
          (push prev-list
                (car next-list))
          (pop next-list)
          (if next-list
              (funcall (el-secretario-source-init-function
                        (car next-list)))
            (with-current-buffer (get-buffer-create "*el-secretario-en*")
              (insert "Done!"))
            (switch-to-buffer (get-buffer-create "*el-secretario-en*")))))
    (el-secretario-end-sesion)))


(defun el-secretario-status-buffer-activate ()
  (el-secretario-status-buffer-deactivate)
  (display-buffer-in-side-window (get-buffer-create el-secretario-status-buffer-name)
                                 '((side . top))))

(defun el-secretario-status-buffer-deactivate ()
  (-some-> (get-buffer-window el-secretario-status-buffer-name)
    (delete-window)))

(provide 'el-secretario)
;;; el-secretario.el ends here
