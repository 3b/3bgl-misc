(defsystem :3bgl-misc
  :depends-on (alexandria cl-glut cl-glu sb-cga zpb-ttf)
  :components
  ((:module "ttf"
            :serial t
            :components
            ((:file "package")
             (:file "extrude")
             (:file "ttf-extrude-example")))))

