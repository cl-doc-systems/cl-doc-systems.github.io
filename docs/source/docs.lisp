(defpackage #:example-docs/docs
  (:nicknames #:example-docs)
  (:use #:cl)
  (:import-from #:mgl-pax
                #:section
                #:defsection)
  (:export
   #:build-docs))
(in-package example-docs/docs)


(defsection @index (:title "Common Lisp Documentation Builders")
  "
This site's goal is to help you to choose a Common Lisp
documentation builder suited your needs.

There are separate repositories with example projects.
Each project uses a different documentation builder.
But all projects are similar in their structure. All of them
define a few packages, functions and classes.

Here is a list of documentation builders along with their main strengths:

* mgl-pax
* coo
* cldomain
* geneva

test definition
: The definition test

second item
: Nother definition test

"
  )


(defun build-docs ()
  (mgl-pax:update-asdf-system-readmes @index :example)
  
  (mgl-pax:update-asdf-system-html-docs
   @index :example
   :target-dir "docs/build/"
   :pages `((:objects (,example-docs:@index)
             :source-uri-fn ,(pax:make-github-source-uri-fn
                              :example
                              "https://github.com/cl-doc-systems/mgl-pax")))))
