(defsystem "site"
  :build-operation build-docs-op
  :build-pathname "docs/build/"
  :class :package-inferred-system
  :pathname "docs/source/"
  :depends-on ("site/docs"))


(asdf:register-system-packages "3bmd-ext-definition-lists"
                               '(#:3bmd-definition-lists))
