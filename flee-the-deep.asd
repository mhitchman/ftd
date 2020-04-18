(defsystem "flee-the-deep"
  :depends-on ("alexandria" "cl-charms")
  :serial t
  :components ((:file "packages")
               (:file "colour")
               (:file "maze")
               (:file "flee-the-deep")))

(defsystem "flee-the-deep/executable"
  :defsystem-depends-on (:deploy)
  :build-operation deploy-op
  :build-pathname "ftd"
  :entry-point "flee-the-deep:main"
  :depends-on ("flee-the-deep"))

#+sb-core-compression
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c) :executable t :compression t))
