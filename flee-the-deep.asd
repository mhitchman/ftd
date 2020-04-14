(defsystem "flee-the-deep"
  :depends-on ("cl-charms")
  :serial t
  :components ((:file "packages")
               (:file "maze")
               (:file "flee-the-deep")))

(defsystem "flee-the-deep/executable"
  :build-operation program-op
  :build-pathname "builds/ftd"
  :entry-point "flee-the-deep:main"
  :depends-on ("flee-the-deep"))
