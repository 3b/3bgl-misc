(defsystem :3bgl-misc
  :depends-on (alexandria cl-glut cl-glu sb-cga zpb-ttf
                          glop classimp static-vectors
                          opticl com.gigamonkeys.binary-data
                          split-sequence parse-number
                          mathkit 3bgl-shader glsl-packing
                          cl-tga-opengl)
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
                 (:file "shaders")
                 (:file "util")
))
   (:module "skybox"
    :serial t
    :components ((:file "package")
                 (:file "shaders")
                 #++(:file "skybox-demo")))
   (:module "shader-helper"
    :serial t
    :components ((:file "shader-helper")
                 (:file "program-interface-query")))
   (:module "timing"
    :serial t
    :components ((:file "timing-helper")))
   #+unix
   (:module "xembed"
    :serial t
    :components ((:file "embed")
                 (:file "pixel-cube-shader")))
   #+unix
   (:module "livecode"
    :serial t
    :components ((:file "package")))
   (:module "gpuanim"
            :serial t
            :components
            ((:file "package")
             (:file "gpuanim-shaders")
             (:file "gpuanim")))
   (:module "ttf"
            :serial t
            :components
            ((:file "package")
             (:file "extrude")
             (:file "ttf-extrude-example")))
   (:module "buffer-builder"
            :serial t
            :components
            ((:file "package")
             (:file "builder")))
   (:module "scenegraph"
            :serial t
            :components
            ((:file "package")
             (:file "state")
             (:file "state-helper")
             (:file "scene")))
   (:module "scenegraph2"
            :serial t
            :components
            ((:file "package")
             (:file "resource-manager")
             (:file "file-loader")
             (:file "scenegraph2")))

))

