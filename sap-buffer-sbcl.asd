(defsystem "sap-buffer-sbcl"
  :version "0.0.1"
  :author "Andrew J. Berkley"
  :license "BSD 3-Clause"
  :depends-on (#:lparallel #:alexandria)
  :components ((:file "sap-buffer-sbcl-util")
               (:file "sap-buffer-sbcl"))
  :description "Allocating space in foreign memory")
