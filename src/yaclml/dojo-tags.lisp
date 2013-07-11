;;; -*- lisp -*-

(in-package :it.bese.ucw)

(deftag-macro <dojo:widget (&attribute &allow-other-attributes others &body body)
  `(<:div (@ ,@(iter (for (attribute value) :on others :by #'cddr)
                     (collect (strcat "dojo:" (js::symbol-to-js attribute)))
                     (collect value)))
    ,@body))

