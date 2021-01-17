(defpackage #:site
  (:use #:cl)
  (:import-from #:3bmd-definition-lists)
  (:import-from #:mgl-pax
                #:section
                #:defsection)
  (:export #:build-docs))
(in-package site)

;; Markdown definition lists not work
;; because of error:
;; https://github.com/3b/3bmd/issues/43

(defsection @index (:title "Common Lisp Documentation Builders")
  "
This site's goal is to help you to choose a Common Lisp
documentation builder suited your needs.

There are separate repositories with example projects.
Each project uses a different documentation builder.
But all projects are similar in their structure. All of them
define a few packages, functions and classes.

Here is a list of documentation builders along with their main strengths:

### atdoc

- Uses a custom markup language which allows to do a few interesting things.
- There are special tags to help reference functions and classes.
- It is able to extract arguments description and return value
  into a separate section.
- Builds not only HTML, but also a beautiful PDF and Info.

[Template](https://github.com/cl-doc-systems/atdoc), [demo](https://cl-doc-systems.github.io/atdoc/), [demo PDF](https://cl-doc-systems.github.io/atdoc/latex/documentation.pdf).

### cl-api

- Really simple.
- Readable color scheme.
- No way to write free documentation chapters.

[Template](https://github.com/cl-doc-systems/cl-api), [demo](https://cl-doc-systems.github.io/cl-api/).

### cl-gendoc

- You can write an extensions to support markups other than Markdown.
- Links to HyperSpec from code snippets.

[Template](https://github.com/cl-doc-systems/cl-gendoc), [demo](https://cl-doc-systems.github.io/cl-gendoc/).

### codex

- Supports different themes.
- Custom markup format, but you can extend it with other markups.
- Links to HyperSpec using special tag.


[Template](https://github.com/cl-doc-systems/codex), [demo](https://cl-doc-systems.github.io/codex/).

### coo

- Uses reStructured text format.
- New types of reST blocks can be written in Common Lisp.
- Provides cross-reference helpers.

[Template](https://github.com/cl-doc-systems/coo), [demo](https://cl-doc-systems.github.io/coo/).

### declt

- Uses [Texinfo file format](https://www.gnu.org/software/texinfo/manual/texinfo/texinfo.html)
  for intermediate document store.
  This makes possible to generate not only `HTML`, but also `PDF` and other
  output formats.
- It can automatically include license text into the documentation. But
  this works only for a number of popular licenses like `MIT`, `BSD`, `GPL`,
  `LGPL` and `BOOST`.
- Automatically builds comprehensive API documentation.

[Template](https://github.com/cl-doc-systems/declt), [demo](https://cl-doc-systems.github.io/declt/).

### eazy-documentation

- Unique docstring parser which is works with custom forms.
- Works with all markups supported by Pandoc.
- Automatic discovery of documentation files.

[Template](https://github.com/cl-doc-systems/eazy-documentation), [demo](https://cl-doc-systems.github.io/eazy-documentation/).

### geneva

- Custom markup format [mk2](https://inters.co/geneva/mk2.html).

[Template](https://github.com/cl-doc-systems/geneva), [demo](https://cl-doc-systems.github.io/geneva/).

### mgl-pax

- Uses Markdown.
- Provides convenient cross-reference helpers.
- Has nice default scheme.
- Is able to generate not only HTML, but also a markdown README file.
- Nudge you too keep documentation as close to to the code as possible. You can define doc sections in the lisp files.

[Template](https://github.com/cl-doc-systems/mgl-pax), [demo](https://cl-doc-systems.github.io/mgl-pax/).

### sphinxcontrib-cldomain

- Uses reStructured text format and external Sphinx builder.
- Can use all extensions provided by Sphinx, like builtin search facitility.
- Suitable for large multipart documents.
- Provides cross-reference helpers.

[Template](https://github.com/cl-doc-systems/sphinxcontrib-cldomain), [demo](https://cl-doc-systems.github.io/sphinxcontrib-cldomain/).

### staple

- You can reference symbol just by uppercasing.
- Complex to setup.
- Rendered documentation is nice.
- Can be problems with package inferred systems.
- No way to write free documentation chapters.
- Links to the doc sources on the github.


[Template](https://github.com/cl-doc-systems/staple), [demo](https://cl-doc-systems.github.io/staple/).


")


(defun build-docs ()
  (let ((3bmd-definition-lists:*definition-lists* t))
    (mgl-pax:update-asdf-system-readmes @index :site)
  
    (let ((source-uri
           (pax:make-github-source-uri-fn
            :site
            "https://github.com/cl-doc-systems/cl-doc-systems.github.io")))
      (mgl-pax:update-asdf-system-html-docs
       @index :site
       :target-dir "docs/build/"
       :pages `((:objects (,site::@index)
                          :source-uri-fn ,source-uri))))))
