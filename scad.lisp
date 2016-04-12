(in-package #:openclscad)

(defvar *render-stream* t "Stream to render OpenSCAD to")

(defmacro box ((x &optional y z) &key center)
  "Box takes a size designator list (either a single value, used as the
side length for a cube, or three values, used as the X, Y and Z length).
The box defaults to having one corner at <0, 0, 0>, but it you want it 
centered, you can use :center t.

A unit cube:
  (box (1))

Unit cube, centered on the origin:
  (box (1) :center t)"
  (when (null z)
    (unless (null y)
      (error "No Z provided, with Y provided")))
  (if (null y)
      (let ((x-var (gensym "box-x")))
	`(let ((,x-var ,x))
	   (box-fn ,x-var ,x-var ,x-var ,center)))
    `(box-fn ,x ,y ,z ,center)))

(defun box-fn (x y z center)
  "Internal 'Generate a box' function."
  (format *render-stream*
	  "cube([~f, ~f, ~f], ~a);~&"
	  x y z (if center "true" "false")))

(defun resolution-helper (fragment-angle fragment-size resolution)
  (let ((fs-part (when fragment-size (list
				      (format nil "$fs = ~f" fragment-size))))
	(fa-part (when fragment-angle (list
				       (format nil "$fa = ~f" fragment-angle))))
	(res-part (when resolution (list (format nil "$fn = ~f" resolution)))))
    (nconc fs-part fa-part res-part)))

(defmacro res-list ()
  `(resolution-helper fragment-angle fragment-size resolution))

(defun sphere (&key radius diameter fragment-angle fragment-size resolution)
  "Generate a sphere of  given radius/diameter, centred on the origin.

You can specify resolution, fragment-angle ($fa), fragment-size ($fs) or 
resolution ($fn)."
  (when radius
    (if diameter
	(error "Sphere: Cannot specify both diameter and radius")))
  (unless (or radius diameter)
    	(error "Sphere: Must specify exactly one of radius and diameter"))
  (let ((size (or radius (/ diameter 2)))
	(size-spec (if radius "r" "d")))
    (format *render-stream* "sphere(~a = ~f~{, ~a~})~&" size-spec size (res-list))))

(defun cylinder (&key height
		      radius bottom-radius top-radius
		      diameter bottom-diameter top-diameter
		      center fragment-angle fragment-size resolution)
  (when (or diameter bottom-diameter top-diameter)
    (when (or radius top-radius bottom-radius)
      (error "Cylinder: Cannot specify both diameter and radius"))
    (when (or bottom-diameter top-diameter)
      (when diameter
	(error "Cylinder: Cannot specify diameter if bottom or top diameter is specified"))
      (unless (and top-diameter bottom-diameter)
	(error "Cylinder: Must specify both bottom and top diameter if one is given."))))
  (when (or radius top-radius bottom-radius)
    (when (or bottom-radius top-radius)
      (when radius
	(error "Cylinder: Cannot specify both radius and top/bottom radius"))
      (unless (and top-radius bottom-radius)
	(error "Cylinder: Must specify both top and bottom radius."))))

  ;; Not complete
  )

(defmacro translate ((x y z) &body body)
  `(progn
     (format *render-stream* "translate(v = [~f, ~f, ~f]) {~&" ,x ,y ,z)
     ,@body
     (format *render-stream* "}~&")))

(defmacro union (&body body)
  `(progn
     (format *render-stream* "union() {~&")
     ,@body
     (format *render-stream* "}~&")))
    
(defmacro render ((&optional sink) &body body)
  (if sink
      `(with-open-file (*render-stream* sink :direction :output :if-exists :supersede :if-does-not-exist :create)
		       ,@body)
    `(progn `@body)))
