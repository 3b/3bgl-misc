(defsystem :3bgl-misc
  :depends-on (alexandria cl-glut cl-glu sb-cga zpb-ttf
                          glop classimp static-vectors
                          opticl com.gigamonkeys.binary-data
                          split-sequence parse-number)
  :serial t
  :components
  ((:module "math"
            :serial t
            :components ((:file "package")
                         (:file "math")
                         (:file "quat")))
   (:module "basecode"
            :serial t
            :components ((:file "package")
                         (:file "basecode")
                         (:file "basecode-glut")
                         (:file "basecode-glop")
                         (:file "basecode-projection")
                         (:file "basecode-camera")
                         (:file "basecode-misc")
                         (:file "basecode-exit-on-esc")
                         (:file "fps")
                         (:file "basecode-fbo")
                         #++(:file "basecode-demo")))
   (:module "geometry"
            :serial t
            :components ((:file "package")
                         (:file "bounding-sphere")))
   (:module "spline"
            :serial t
            :components ((:file "package")
                         (:file "quadratic")
                         (:file "spline-demo")))
   (:module "shader"
            :serial t
            :components ((:file "package")
                         (:file "ir")
                         (:file "walker")
                         (:file "types")
                         (:file "infer")
                         (:file "glsl-base")
                         (:file "cl-functions")
                         (:file "glsl420")
                         (:file "finalize-inference")
                         (:file "printer")
                         (:file "compiler")
                         (:file "utils")
))
   (:module "model-viewer"
    :serial t
    :components ((:file "package")
                 (:file "viewer")))
   (:module "bench"
    :serial t
    :components ((:file "package")
                 (:file "bench")))
   (:module "opticl"
    :serial t
    :components ((:file "package")
                 (:file "opticl")))
   (:module "dds"
    :serial t
    :components ((:file "package")
                 (:file "dds")))
   (:module "mesh"
    :serial t
    :components ((:file "package")
                 (:file "mesh")
                 (:file "shaders")))
   (:module "skybox"
    :serial t
    :components ((:file "package")
                 (:file "shaders")
                 #++(:file "skybox-demo")))
   #++(:module "ttf"
            :serial t
            :components
            ((:file "package")
             (:file "extrude")
             (:file "ttf-extrude-example")))))

