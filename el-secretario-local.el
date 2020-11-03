;;; test.el --- description -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2020 Leo
;;
;; Author: Leo <http://github/leo>
;; Maintainer: Leo <leo@leo-B85-HD3>
;; Created: October 31, 2020
;; Modified: October 31, 2020
;; Version: 0.0.1
;; Keywords:
;; Homepage: https://github.com/leo/test
;; Package-Requires: ((emacs 26.1) (cl-lib "0.5"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;;  description
;;
;;; Code:

(defmacro el-secretario-local-get (buf-name var)
  `(with-current-buffer (get-buffer-create (concat "*el-secretario-state " ,buf-name "*"))
     ,var))

(defmacro el-secretario-local-setq (buf-name var val)
  `(with-current-buffer (get-buffer-create (concat "*el-secretario-state " ,buf-name "*"))
     (setq-local ,var ,val)))

(defmacro el-secretario-local-push (buf-name val var)
  `(with-current-buffer (get-buffer-create (concat "*el-secretario-state " ,buf-name "*"))
     (push ,val ,var)))

(defmacro el-secretario-local-pop (buf-name var)
  `(with-current-buffer (get-buffer-create (concat "*el-secretario-state " ,buf-name "*"))
     (pop ,var)))

;; (let ((barvar 'bar))
;;   (el-secretario-setq-local "foo" barvar 3))
;; (let ((barvar 'bar))
;;   (el-secretario-get-local "foo" barvar) )

;; (el-secretario-get-local "foo" 'bar)

;; (defmacro el-secretario-load-local (buf-name var)
;;   `(setq ,var (el-secretario-get-local ,buf-name ,var)))

;; (hydra-disable)
;; (defvar foo nil)
;; (setq-local foo 1)
;; (setq foo 3)
;; (el-secretario-setq-local "foo" bar 2)
;; (el-secretario-get-local "foo" bar)
;; (el-secretario-load-local "foo" bar)
(provide 'el-secretario-local-get)
;;; test.el ends here
