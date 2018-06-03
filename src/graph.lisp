;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.graph; Base: 10 -*- 
;;; -*- coding:utf-8 -*-
;;;****************************************************************************
;;; FILE:        graph.lisp
;;; LANGUAGE:    Common-Lisp
;;; 
;;; DESCRIPTION
;;; 
;;; 
;;; 
;;; Author: Christian Hofmann
;;; 
;;; Created: Fri Jun 12 14:12:10 2009 (z)
;;; 
;;; Last-Updated: Sun Oct  4 12:40:05 2009 (z)
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
;;;****************************************************************************

(in-package :util.graph)

;; to add permanent attributes to nodes and edges, subclass
;; for temporarily used attributes used in gaph-algorithms use labeled-nodes or labeled-edges,
;; and subclass labeled-graph accordingly

(defgeneric same-p (one another)
  (:DOCUMENTATION "Equality test."))

(defgeneric  add-node (self &key node-type)
  (:DOCUMENTATION "Adds a node to the graph SELF.
The node-type can be provided using the keyword :NODE-TYPE.
When no node-type is provided, the graph's default node-type is used.
RETURNS the newly created node instance."))

(defgeneric add-edge (self src &key tgt node-type edge-type)
  (:DOCUMENTATION "Adds an edge with the source node SRC and target node :TGT to the graph SELF.
If the keyword :TGT was not used, a new target node is created and
added to the graph. The node-type can be provided using the keyword :NODE-TYPE.
When no node-type is provided, the graph's default node-type is used.
Similarly, the edge-type can be provided using the keyword :EDGE-TYPE.
RETURNS the newly created edge instance."))

;; Adjazenzlisten Repr"asentation

(defclass graph ()
  ((nodes
    :ACCESSOR nodes
    :INITFORM '()
    :TYPE list)
   (edge-type
    :ACCESSOR edge-type
    :INITARG :edge-type)
   (node-type
    :ACCESSOR node-type
    :INITFORM 'node))
  (:DOCUMENTATION "A network of nodes and edges"))

(defclass node ()
  ((id
    :READER id
    :INITFORM (gensym "N"))
   (edges
    :ACCESSOR edges
    :INITFORM '()
    :TYPE list)
   (graph
    :READER graph
    :INITARG :graph
    :TYPE graph
    :DOCUMENTATION "A back-link to the graph that is used to access information about nodes that is managed by the graph.")
   (in-degree
    :ACCESSOR in-degree
    :INITFORM 0
    :TYPE fixnum
    :DOCUMENTATION "The number of incoming edges."))
  (:DOCUMENTATION "Also known as vertex. One of the points on which the graph is defined, and which may be connected by edges."))

(defclass edge ()
  ((graph
    :READER graph
    :INITARG :graph
    :TYPE graph
    :DOCUMENTATION "A back-link to the graph that is used to access information about edges that is managed by the graph."))
  (:DOCUMENTATION "An element that joins a pair of nodes."))

(defun make-graph ()
  (make-instance 'graph))


(defclass directed-graph (graph)
  ((edge-type
    :INITFORM 'directed-edge))
  (:DOCUMENTATION "A graph with directed graph-edges"))

(defclass directed-edge (edge)
  ((src-node
    :ACCESSOR source
    :INITARG :source
    :TYPE node)
   (tgt-node
    :ACCESSOR target
    :INITARG :target
    :TYPE node))
  (:DOCUMENTATION "An ordered pair of nodes."))


(defclass labeled-graph (graph)
  ()
  (:DOCUMENTATION "A graph where nodes or edges are considered distinct."))

(defclass labeled-node (node)
  ()
  (:DOCUMENTATION "A node for vertex-labeled-graphs"))

(defclass labeled-edge (edge)
  ()
  (:DOCUMENTATION "An edge for edge-labeled-graphs"))


(defclass vertex-labeled-graph (labeled-graph)
  ((node-type
    :ACCESSOR node-type
    :INITFORM 'labeled-node))
  (:DOCUMENTATION "A graph where nodes are considered distinct."))

(defclass edge-labeled-graph (labeled-graph)
  ((edge-type
    :ACCESSOR edge-type
    :INITFORM 'labeled-edge))
  (:DOCUMENTATION "A graph where edges are considered distinct."))

(defclass rooted-graph (vertex-labeled-graph)
  ((root-node
    :ACCESSOR root-node
    :INITARG root-node
    :DOCUMENTATION "The root-node in the graph"))
  (:DOCUMENTATION "A graph in which one node is distuingished from the other nodes."))

(defclass rooted-directed-graph (rooted-graph directed-graph)
  ((edge-type
    :INITFORM 'directed-edge))
  (:DOCUMENTATION "A graph with directed edges, where one node with no entering edges is distinguished."))


(defmethod description ((self edge))
  (format nil "<~A>" (class-name (class-of self))))

(defmethod same-p ((node-1 node) (other t))
  "Equality test."
  nil)

(defmethod same-p ((other t) (node-1 node))
  "Equality test."
  nil)

(defmethod same-p ((node-1 node) (node-2 node))
  "Equality test."
  (declare (type node node-1 node-2))
  (or (eq node-1 node-2)
      (eql (slot-value node-1 'id)
           (slot-value node-2 'id))))

#.(declaim (inline out-degree))
(defun out-degree (node)
  "RETURNS the out-degree of a node NODE."
  (declare (type node node))
  (length (edges node)))

(defmethod add-node ((self graph) &key (node-type (node-type self)))
  "Adds a node to the graph SELF.
The node-type can be provided using the keyword :NODE-TYPE.
When no node-type is provided, the graph's default node-type is used.
RETURNS the newly created node instance.
Uses push to add the node to the graph."
  (let ((new-node (make-instance node-type :graph self)))
    (push new-node
          (nodes self))
    new-node))

(defmethod add-node-in-order ((self graph) &key (node-type (node-type self)))
  "Adds a node to the graph SELF.
The node-type can be provided using the keyword :NODE-TYPE.
When no node-type is provided, the graph's default node-type is used.
RETURNS the newly created node instance.
Appends the new node at the end of the edge list of the graph."
  (let ((new-node (make-instance node-type :graph self)))
    (setf (nodes self)
          (nconc (nodes self)
                (list new-node)))
    new-node))

(defmethod add-edge ((self graph) src
                       &key (tgt nil) (node-type (node-type self)) (edge-type (edge-type self)))
  "Adds an edge with the source node SRC and target node :TGT to the graph SELF.
If the keyword :TGT was not used, a new target node is created and
added to the graph. The node-type can be provided using the keyword :NODE-TYPE.
When no node-type is provided, the graph's default node-type is used.
Similarly, the edge-type can be provided using the keyword :EDGE-TYPE.
RETURNS the newly created edge instance.
Uses push to add the edge to the source-node."
  (unless tgt
    (setq tgt
          (add-node self :node-type node-type)))
  (let ((new-edge
         (make-instance edge-type
                        :graph self
                        :source src
                        :target tgt)))
    (incf (in-degree tgt))
    (push new-edge
          (edges src))
    new-edge))

(defmethod remove-outgoing-edges ((self node))
  "Removes all edges outgoing from the node SELF.
When the first edge connecting the node is an undirected-edge,
no changes are performed."
  (let ((edges (edges self)))
    (when (and (> (length edges) 0)
               (typep (first edges) 'directed-edge))
      ;; only if node has directed edges
      (setf (edges self)
            '()))))

(defmacro deftableslotaccessor (attribute-name node-or-edge-type &key documentation)
  "Generates a method that allows to access the attribute of a node or edge
using the hash-table associated with the graph, where the attribute is stored."
  `(progn
    (defmethod ,attribute-name ((self ,node-or-edge-type))
      ,documentation
      (gethash self (slot-value (graph self) ',attribute-name)))
    (defmethod (setf ,attribute-name) (value (self ,node-or-edge-type))
      ,documentation
      (setf (gethash self (slot-value (graph self) ',attribute-name))
       value))))