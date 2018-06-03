;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.graph.gxl; Base: 10 -*- 
;;; -*- coding: utf-8 -*-
;;;****************************************************************************
;;; FILE:        gxl.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Wed Sep 23 09:50:52 2009 (z)
;;; 
;;; Last-Updated: Wed Sep 23 12:13:35 2009 (z)
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

(in-package :util.graph.gxl)

(defgeneric node->gxl-node (node graph &key label children))
(defgeneric edge->gxl-node (edge graph &key label children))

(defmacro node-to-string (node)
  `(princ-to-string (id ,node)))

(defmethod node->gxl-node ((self node) (graph graph)
                               &key (label "") (children nil))
  (declare (ignore label))
  (let ((child-nodes))
    (if children
        (setq child-nodes (copy-list children))
        (setq child-nodes '()))
    (xmls:make-node :name "node"
                    :attrs `(("id" ,(node-to-string self)))
                    :children child-nodes)))

(defmethod edge->gxl-node ((self edge) (graph graph)
                               &key (label "") (children nil))
  (let ((child-nodes)
        (label-node))
    (if children
        (setq child-nodes (copy-list children))
        (setq child-nodes '()))
    (when (> (length label) 0)
      (setq label-node
            (xmls:make-node :name "string" :children (list (normalize-label label))))
      (push (xmls:make-node :name "attr" :attrs '(("name" "label"))
                            :children (list label-node))
            child-nodes))
    (xmls:make-node :name "edge"
                    :attrs `(("from" ,(node-to-string (source self)))
                             ("to" ,(node-to-string (target self))))
                    :children child-nodes)))

(defmethod graph->gxl (stream (self graph))
  ;; xml header
  (write-string "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" stream)
  (write-char #\Newline stream)
  ;; gxl graph
  (let ((gxl-node)
        (graph-node)
        (xmls-tree '()))
    ;; add nodes
    (loop :for node :in (nodes self)
          :do
           (push (node->gxl-node node self)                       
                 xmls-tree))
    ;; add edges
    (loop :for node :in (nodes self)
          :do
           (loop :for edge :in (edges node)
                 :do
                  (push (edge->gxl-node edge self)
                        xmls-tree)))
    ;; add parent graph-node
    (setq graph-node
          (xmls:make-node :name "graph"
                          :attrs '(("id" "graph")
                                   ("role" "graph")
                                   ("edgeids" "false")
                                   ("edgemode" "directed"))
                          :children (nreverse xmls-tree)))
    ;; add root gxl-node
    (setq gxl-node   (xmls:make-node :name "gxl"
                                     :ns "http://www.gupro.de/GXL/gxl-1.0.dtd"
                                     :children (list graph-node)))
    ;; write graph to stream
    (xmls:write-xml gxl-node stream)))

(defun write-gxl-file (gxl-file-name graph)
  "WRITES a graph GRAPH into a gxl file.
GXL-FILE-NAME should be a pathname."
  (with-open-file (stream gxl-file-name :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (graph->gxl stream graph)))
