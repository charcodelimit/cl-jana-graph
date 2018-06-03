;;;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Package: util.graph.s-dot; Base: 10 -*- 
;;;;; -*- coding: utf-8 -*-
;;;;;****************************************************************************
;;;;;
;;;;; File s-dot.lisp
;;;;;
;;;;; A Common Lisp language binding for the graphviz 'dot' tool.
;;;;;
;;;;;
;;;;; Created: Fri Sep 18 09:27:38 2009 (z)
;;;;; 
;;;;; Last-Updated: Thu Oct  1 13:23:51 2009 (z)
;;;;;           By: Christian Hofmann
;;;;;
;;;;; Modifications:
;;;;;  Copyright (C) 2009, Christian Hofmann. All rights reserved.
;;;;;
;;;;; Permission is hereby granted, free of charge, to any person
;;;;; obtaining a copy of this software and associated documentation
;;;;; files (the "Software"), to deal in the Software without
;;;;; restriction, including without limitation the rights to use,
;;;;; copy, modify, merge, publish, distribute, sublicense, and/or
;;;;; sell copies of the Software, and to permit persons to whom the
;;;;; Software is furnished to do so, subject to the following
;;;;; conditions:
;;;;; 
;;;;; The above copyright notice and this permission notice shall be
;;;;; included in all copies or substantial portions of the Software.
;;;;; 
;;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
;;;;; OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;;; OTHER DEALINGS IN THE SOFTWARE.
;;;;;
;;;;;
;;;;; Original version:
;;;;;  Copyright (c) 2006 Martin Loetzsch.  All rights reserved.
;;;;;	      
;;;;; Redistribution and use in source and binary forms, with or without 
;;;;; modification, are permitted provided that the following conditions 
;;;;; are met:
;;;;;	      
;;;;; 1. Redistributions of source code must retain the above copyright
;;;;;    notice, this list of conditions and the following disclaimer.
;;;;;	      
;;;;; 2. Redistributions in binary form must reproduce the above 
;;;;;    copyright  notice, this list of conditions and the following 
;;;;;    disclaimer in the documentation and/or other materials provided 
;;;;;    with the distribution.
;;;;;	      
;;;;; 3. The end-user documentation included with the redistribution, if 
;;;;;    any, must include the following acknowledgment:
;;;;;    "This product includes S-DOT developed by Martin Loetzsch 
;;;;;    (http://www.martin-loetzsch.de/S-DOT)."
;;;;;    Alternately, this acknowledgment may appear in the software 
;;;;;    itself, if and wherever such third-party acknowledgments 
;;;;;    normally appear.
;;;;;	      
;;;;; THIS SOFTWARE IS PROVIDED BY MARTIN LOETZSCH ``AS IS'' AND ANY 
;;;;; EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
;;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR 
;;;;; PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL MARTIN LOETZSCH BE LIABLE 
;;;;; FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
;;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT 
;;;;; OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR 
;;;;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF 
;;;;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING 
;;;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF 
;;;;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;;; 
;;;;;****************************************************************************

(in-package :util.graph.s-dot)

;; the pure syntax of s-dot. 
(defparameter s-dot-syntax
  `((graph (cluster sub-graph node record edge) ;; children
	   nil ;; required attributes
	   (bgcolor fontcolor fontname ;; optional attributes, including the required ones
                    fontsize label margin nodesep ordering rankdir ranksep ratio size))
    (cluster (cluster sub-graph node record edge node-id)
	     (id)
	     (bgcolor color fillcolor fontcolor fontname fontsize id label labeljust
		      labelloc style clusterrank))
    (sub-graph (node record node-id) ;; children
	       nil ;; required attributes
               (rank id)) ;; optional attributes
    (node-id nil ;; children
             (id) ;; required attributes
             (id)) ;; optional attributes
    (node nil 
	  (id) 
	  (color fillcolor fixedsize fontcolor fontname fontsize height id 
		 label shape style URL width))
    (record (record node) 
	    nil
	    (color fillcolor fixedsize fontcolor fontname fontsize height
		   label shape style URL width))
    (edge nil
	  (from to)
	  (arrowhead arrowsize arrowtail color constraint decorate dir fontcolor fontname 
		     fontsize from headlabel headport label labeldistance labelfloat labelfontcolor 
		     labelfontname labelfontsize lhead ltail minlen style samehead sametail
		     taillabel tailport to URL weight))))
  

(defun check-syntax (graph &key (level 0))
  "Checks whether the s-expression 'graph' is syntactically correct. 
Note that it does not check whether the attributes contain useful values."
  (unless (and graph (listp graph)) (error "graph should be a list"))
  (let* ((element (first graph))
	(element-spec (find element s-dot-syntax :key #'first)))
    (when (and (= level 0) (not (equal element 'graph)))
    (error "an s-dot expression should start with 'graph'. Instead, ~a was passed" element))
    (unless (listp (second graph)) (error "~a should be a list of parameters, context: ~a"
					  (second graph) graph))
    (loop for attribute in (second graph) 
       do (when (or (not (listp attribute)) (not (= (length attribute) 2)) 
		    (not (or (stringp (second attribute))
                             (util.colors:color-p (second attribute)))))
	    (error "~a attribute ~a should be of the form (attribute \"value\"), context: " 
		   element attribute))
	 (unless (find (first attribute) (fourth element-spec))
	   (error "attribute ~a is not allowed in ~a elements, context: ~a"
		  (first attribute) element graph)))
    (loop for required-attribute in (third element-spec)
	 do (unless (find required-attribute (second graph) :key #'first)
	      (error "required attribute ~a not found in element ~a, context: ~a"
		     required-attribute element graph)))
    (loop for child in (cddr graph)
	 do (unless (listp child) 
	      (error "child ~a of element ~a should be a alist, context: ~a" child element graph))
	 (unless (symbolp (first child))
	   (error "~a should be the name of an element, context: ~a" (first child) child))
	 (unless (find (first child)  s-dot-syntax :key #'first)
	   (error "element ~a not known, context: ~a" (first child) child))
	 (unless (find (first child) (second element-spec))
	   (error "element ~a not allowed inside ~a, context: ~a"
		  (first child) element child))
	 (check-syntax child :level (+ 1 level)))))

(defun s-dot->dot (stream graph &key (check-syntax t))
  "Generates dot syntax from a s-dot expression and writes the result to 'stream'.
This code looks indeed ugly. If you really want to understand what's going on, then it might
be easier to look in the the XSLT stylesheet at
http://www.martin-loetzsch.de/DOTML/dotml-1.3/dotml2dot.xsl . This code does exactly the same."
  (let ((sub-graph-counter 0)
	(records nil))
    (labels ((attribute-value (attribute element) 
	       (second (find attribute (second element) :key #'first)))
	     (generate-record-label (element)
	       (if (equal (first element) 'record)
		   (format nil "{~{~a~^ | ~}}" (mapcar #'generate-record-label (cddr element)))
		   (format nil "<~a> ~a" (attribute-value 'id element)
			   (if (attribute-value 'label element) 
			       (attribute-value 'label element)
			       (attribute-value 'id element)))))
	     (write-attributes 
		 (element separator exclude-attributes)
	       (let ((element-spec (find (first element) s-dot-syntax :key #'first)))
		 (loop :for attribute :in (fourth element-spec)
                       :for attribute-value = (attribute-value attribute element)
                       :do
                       (unless (find attribute exclude-attributes)
                         (if (util.colors:color-p attribute-value)
                             (format stream "~a=\"~a\"~a" 
                                     (string-downcase (symbol-name attribute))
                                     (with-output-to-string (s) (util.colors:print-hex attribute-value s)) separator)
                             (format stream "~a=\"~a\"~a" 
                                     (string-downcase (symbol-name attribute))
                                     (if attribute-value attribute-value "") separator))))))
	     (write-element 
		 (element)
	       (macrolet ((write-children () 
			    `(loop for e in (cddr element) do (write-element e))))
		 (cond ((equal (first element) 'sub-graph)
			(format stream "~%subgraph sub_graph_~a{rank=\"~a\";"
				(incf sub-graph-counter) (attribute-value 'rank element))
			(write-children)
			(format stream "}"))
		       ((equal (first element) 'cluster)
			(format stream "~%subgraph cluster_~a{" (attribute-value 'id element))
			(write-attributes element ";" '(id))
			(write-children)
			(format stream "}"))
                       ((equal (first element) 'node-id)
                        (format stream " ~a;" (attribute-value 'id element)))
		       ((equal (first element) 'node)
			(format stream "~%node[label=\"~a\", " 
				(if (attribute-value 'label element)
				    (attribute-value 'label element)
				    (attribute-value 'id element)))
			(write-attributes element "," '(label id))
			(format stream "] {~a};" (attribute-value 'id element)))
		       ((equal (first element) 'edge)
			(format stream "~%edge[")
			(write-attributes element "," '(from to lhead ltail))
			(format stream "lhead=\"~a\", ltail=\"~a\"]"
				(if (attribute-value 'lhead element)
				    (concatenate 'string "cluster_" 
						 (attribute-value 'lhead element)) "")
				(if (attribute-value 'ltail element)
				    (concatenate 'string "cluster_" 
						 (attribute-value 'ltail element)) ""))
			(let ((struct-from (loop for r in records 
						when (find (attribute-value 'from element)
							   (third r) :test #'equal)
						return (second r)))
			      (struct-to (loop for r in records 
					    when (find (attribute-value 'to element)
						       (third r) :test #'equal)
					    return (second r))))
			  (when struct-from (format stream "struct~a:" struct-from))
			  (format stream "~a ->" (attribute-value 'from element))
			  (when struct-to (format stream "struct~a:" struct-to))
			  (format stream "~a;" (attribute-value 'to element))))
		       ((equal (first element) 'record)
			(format stream "~%node[shape=\"record\",label=\"~a\""
				(format nil "~{~a~^ | ~}" 
					(mapcar #'generate-record-label (cddr element))))
			(write-attributes element "." '(id label))
			(format stream "]{struct~a};"
				(second (find element records :key #'first :test #'equal)))))))
	     (store-record-nodes (graph)
	       (if (equal (first graph) 'node)
		   (list (second (find 'id (second graph) :key #'first)))
		   (loop for element in (cddr graph) 
		      append (store-record-nodes element)))) 
	     (store-records (graph)
	       (if (equal (first graph) 'record)
		   (push (list  graph (length records)
				(store-record-nodes graph)) records)
		   (loop for element in (cddr graph)
		      do (store-records element)))))
      (store-records graph)
      (when check-syntax (check-syntax graph))
      (format stream "digraph g {compound=\"true\";")
      (write-attributes graph ";" nil)
      (loop for element in (cddr graph) do (write-element element))
      (format stream "}~c" #\linefeed))))

(defun write-dot-file (dot-file-name graph &key (check-syntax t))
  "WRITES an s-dot graph GRAPH into a dot file.
DOT-FILE-NAME should be a pathname."
  (with-open-file (stream dot-file-name :direction :output :if-exists :supersede
                          :if-does-not-exist :create)
    (s-dot->dot stream graph :check-syntax check-syntax)))

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

;;; chr: removed it due to the many system dependencies
;;;      that should we dealt with further upstream
;(defun render-s-dot (file-name format graph &key (check-syntax t))
;  "Renders a s-dot graph into a graphic file. 'file-name' should be a pathname.
;If the file-name is /foo/bar.png, the dot file /foo/bar.dot is created and then rendered.
;Format should be one out of http://www.graphviz.org/doc/info/output.html, 
;for example svg, ps, gif, png, or jpg.
;The asdf:run-shell is used to launch dot. If that does not work for you, write a 
;similar function that uses (s-dot->dot) for dot generation and then runs dot on it.
;
;Please note that it is more efficient to open a pipe to 'dot' instead of writing a 
;file and reading that in again. A cross-platform pipe implementation can be found 
;at  http://cvs.sourceforge.net/viewcvs.py/clocc/clocc/src/port/ext.lisp?view=markup"
;  (let ((dot-file-name (make-pathname :directory (pathname-directory file-name)
;                                      :name (pathname-name file-name) :type "dot")))
;    (with-open-file (stream dot-file-name :direction :output :if-exists :supersede
;                            :if-does-not-exist :create)
;      (s-dot->dot stream graph :check-syntax check-syntax))
;    (asdf:run-shell-command (format nil "dot -o ~a -T~a ~a" file-name format dot-file-name))
;    ;; On MacOSX, there is the nice command 'open' that opens a file with the application 
;    ;; that is associated to its type
;    (when (equal (software-type) "Darwin")
;      (asdf:run-shell-command (format nil "open ~a" file-name)))))

;(defun test-s-dot ()
;  "Generates a few charts in /tmp/. If you don't have /tmp/, 
;change the directory below to something else"
;  (let ((directory '(:absolute "tmp"))
;        (graph1 '(graph ()
;                  (node ((id "a") (label "a")))
;                  (cluster ((id "c1") (label "c1") (style "filled") (fillcolor "#EEEEFF")
;                            (fontcolor "#900000") (fontname "Arial bold") (fontsize "15"))
;                   (node ((id "b"))) (node ((id "c")))
;                   (edge ((from "b") (to "c"))))
;                  (cluster ((id "c2") (label "c2") (fontname "Courier") (style "dashed"))
;                   (node ((id "d"))) (node ((id "e"))) (node ((id "f"))) 
;                   (edge ((from "d") (to "e")))
;                   (edge ((from "e") (to "f"))) 
;                   (edge ((from "d") (to "f")))) 
;                  (node ((id "g")))
;                  (edge ((from "a") (to "b")))
;                  (edge ((from "a") (to "d") (lhead "c2") (label "lhead='c2'")))
;                  (edge ((from "b") (to "e") (ltail "c1")))
;                  (edge ((from "d") (to "c") (ltail "c2")))
;                  (edge ((from "a") (to "g")))
;                  (edge ((from "c") (to "g") (ltail "c1") (label "ltail='c1'")))
;                  (edge ((from "f") (to "g") ))))
;        (graph2 '(graph ()
;                  (record ()
;                   (node ((id "10") (label "left")))
;                   (node ((id "11") (label "middle")))
;                   (node ((id "12") (label "right"))))
;                  (record ()
;                   (node ((id "20") (label "one")))
;                   (node ((id "21") (label "two"))))
;                  (record ()
;                   (node ((id "30") (label "hello\\nworld")))
;                   (record ()
;                    (node ((id "311") (label "b")))
;                    (record () 
;                            (node ((id "3120") (label "c")))
;                            (node ((id "3121") (label "d")))
;                            (node ((id "3122") (label "e"))))
;                    (node ((id "313") (label "f"))))
;                   (node ((id "32") (label "g")))
;                   (node ((id "33") (label "h"))))
;                  (edge ((from "11") (to "20")))
;                  (edge ((from "12") (to "3121"))))))
;    (render-s-dot (make-pathname :directory directory :name "test1" :type "gif") "gif" graph1)
;    (render-s-dot (make-pathname :directory directory :name "test2" :type "ps") "ps" graph1)
;    (render-s-dot (make-pathname :directory directory :name "test3" :type "jpg") "jpg" graph2)
;    (render-s-dot (make-pathname :directory directory :name "test4" :type "png") "png" graph2)
;    (render-s-dot (make-pathname :directory directory :name "test5" :type "svg") "svg" graph2)))

;(test-s-dot)




