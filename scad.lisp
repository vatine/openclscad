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
  (format *render-stream*
	  "cube([~f, ~f, ~f], ~a)~&"
	  x y z (if center "true" "false")))

(defun sphere (&key radius diameter fragment-angle fragment-size resolution)
  (when radius
    (if diameter
	(error "Must specify only one of radius and diameter")))
  (unless (or radius diameter)
    	(error "Must specify only one of radius and diameter"))
  (let ((size (or radius diameter))
	(size-spec (if radius "r" "d"))
	(fs-part (when fragment-size (list
				      (format nil "$fs = ~f" fragment-size))))
	(fa-part (when fragment-angle (list
				       (format nil "$fa = ~f" fragment-angle))))
	(res-part (when resolution (list (format nil "$fn = ~f" resolution)))))
    (format *render-stream* "sphere(~a = ~f~{, ~a~})~&" size-spec size (nconc fs-part fa-part res-part))))
