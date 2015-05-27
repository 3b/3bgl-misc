(defpackage #:3bgl-gpuanim
  (:use #:cl)
  (:import-from :static-vectors
                #:with-static-vector)
  (:export #:build-anim-data
           #:update-instance-data
           #:with-gpu-anim
           #:update-skeleton))


(defpackage #:3bgl-gpuanim-shaders
  (:use #:3bgl-glsl/cl)
  (:export #:max-bone
           #:max-bone-models
           #:time-ms))
