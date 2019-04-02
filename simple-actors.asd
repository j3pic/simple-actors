(asdf:defsystem :simple-actors
    :author ("Jeremy Phelps")
    :version "1"
    :license "BSD"
    :description "Port of banker.scm from Racket"
    :depends-on (:bordeaux-threads)
    :components
    ((:file "actors" :depends-on ("ipc" "better-handler-case"))
     (:file "ipc")
     (:file "better-handler-case")))
