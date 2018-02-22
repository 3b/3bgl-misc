(in-package 3bgl-sg2)

(defvar *3bgl-misc-data-dir*
  (asdf:system-relative-pathname '3bgl-misc "data/"))

(defmethod load-texture ((type (eql :default)) name &key target)
  (let ((*default-pathname-defaults* *3bgl-misc-data-dir*))
    (case name
      (:debug (get-texture (merge-pathnames "debug-texture.png")
                           :type :file :target target))
      (:debug-height (get-texture (merge-pathnames "debug-height-texture.png")
                                  :type :file :target target))
      (:debug-normal (get-texture (merge-pathnames "debug-normal-texture.png")
                                  :type :file :target target))
      (:debug-metal (get-texture (merge-pathnames "debug-metal-texture.png")
                                 :type :file :target target))
      (:debug-emissive (get-texture
                        (merge-pathnames "debug-emissive-texture.png")
                        :type :file :target target))
      (:debug-skybox-cube (get-texture
                           (merge-pathnames "debug-skybox-cube.png")
                           :type :file :target target))
      (t (get-texture (merge-pathnames "debug-texture.png")
                      :type :file :target target)))))

(defmethod load-object ((loader (eql :default)) name
                        &key sg parent-node)
  (let ((*default-pathname-defaults* *3bgl-misc-data-dir*))
    (case name
      (:floor (load-object :file (merge-pathnames "floor.obj")
                           :sg sg :parent-node parent-node))
      (:light (load-object :file (merge-pathnames "light.obj")
                           :sg sg :parent-node parent-node))
      (:box (load-object :file (merge-pathnames "box.obj")
                         :sg sg :parent-node parent-node))
      (:skybox (load-object :file (merge-pathnames "skybox.obj")
                            :sg sg :parent-node parent-node)))))
