(push #p"~/.emacs.d/elpa/sly-20200314.55/slynk/" ASDF:*CENTRAL-REGISTRY*)
(asdf:make "slynk")
(slynk:create-server :dont-close t)
(loop (sleep 1000))
