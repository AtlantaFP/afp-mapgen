(asdf:defsystem #:afp-mapgen-renderer
  :description "A renderer for afp-mapgen."
  :author ("Michael Fiano <mail@michaelfiano.com>")
  :maintainer "Michael Fiano <mail@michaelfiano.com>"
  :license "MIT"
  :homepage "https://www.youtube.com/channel/UCYg6qFXDE5SGT_YXhuJPU0A"
  :source-control (:git "https://github.com/AtlantaFP/afp-mapgen.git")
  :bug-tracker "https://github.com/AtlantaFP/afp-mapgen/issues"
  :version "0.1.0"
  :encoding :utf-8
  :long-description #.(uiop:read-file-string (uiop/pathname:subpathname *load-pathname* "README.md"))
  :depends-on (#:afp-mapgen
               #:sketch)
  :pathname "src"
  :serial t
  :components
  ((:file "package")
   (:file "render")))
