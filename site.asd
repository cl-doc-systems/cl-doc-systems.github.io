(defsystem "site"
  :build-operation build-docs-op
  :build-pathname "docs/build/"
  :class :package-inferred-system
  :pathname "docs/source/"
  :depends-on ("site/docs"))
