;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.graph; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        util.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Sep 23 10:45:49 2009 (z)
;;; 
;;; Last-Updated: Wed Sep 23 10:46:14 2009 (z)
;;;           By: Christian Hofmann
;;; 
;;; Copyright (C) 2009, Christian Hofmann. All rights reserved.
;;; 
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use,
;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;; sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following
;;; conditions:
;;; 
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;; 
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;; OTHER DEALINGS IN THE SOFTWARE.
;;; 
;;;****************************************************************************

(in-package :util.graph)

(defun normalize-label (string)
  "Adds a #\SPACE at the beginning of the label STRING,
because dot often places labels too near to the edges.
Furthermore, it removes formatting characters like line break,
and replaces all occurences of double-quotes #\" in the label STRING
against single quotes #\'."
  (declare (type simple-string string))
  (let ((last-char #\SPACE)
        (current-char #\SPACE)
        (skip 0))
    (declare (type char last-char current-char)
             (type fixnum skip))
    (with-output-to-string (stream)
      (loop :for char :across (the simple-string string)
            :do
             (setq last-char current-char)
             (setq current-char char)
             ;; remove formatting directives
             (when (and (eq last-char #\\)
                        (or (eq current-char #\n)
                            (eq current-char #\r)
                            (eq current-char #\t)))                      
               (setf skip 2))
             ;; substitute more than 2 spaces against only 1 space
             (when (and (eq last-char #\space)
                        (eq current-char #\space))
               (setf skip 1))
             ;; remove newline and tab
             (when (or (eq last-char #\newline)
                       (eq last-char #\tab))
               (setf skip 1))
             (if (> skip 0)
                 (decf skip)
                 (if (eq last-char #\")
                     ;; substitute double-quotes
                     (write-char #\' stream)
                     (write-char last-char stream)))
             :finally
              (unless (or (> skip 0)
                          (eq current-char #\newline)
                          (eq current-char #\tab))
                 (if (eq current-char #\")
                     ;; substitute double-quotes
                     (write-char #\' stream)
                     (write-char current-char stream)))))))