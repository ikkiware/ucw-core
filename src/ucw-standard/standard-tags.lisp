(in-package :ucw-standard)

;;; * Standard YACLML tags. 

;;; * Utilities

(defun gen-id (string)
  (format nil "~A-~A" string  (arnesi:random-string)))

;;; ** ACTION tags

;;; These tags take UCW "actions" and create the appropriate HTML
;;; tag to signal their execution.

(defmacro %with-action-unique-names (&body body)
  "These magic macros."
  `(with-unique-names (url action-object action-id current-frame)
    (assert (xor action action* function) nil
	    "Must supply only one of ACTION,  ACTION* or FUNCTION")
    `(let* ((,current-frame (context.current-frame *context*)) 
	    (,action-object  ,(or action* 
				  `(make-action 
				    ,(or function
					 `(lambda ()
					    (with-call/cc ,action))))))
	    (,action-id  (register-action-in-frame 
			  ,current-frame 
			  ,action-object))
						      
				
	    (,url (compute-url ,action-object *current-component*)))
       (declare (ignorable ,action-id ,url))
       ,,@body)))


(deftag-macro <ucw:a (&attribute action action* function
				 &allow-other-attributes others
				 &body body)
  "A Simple <:A which does not require javascript."
  (%with-action-unique-names 
   `(<:a :href (print-uri-to-string ,url)
	 ,@others
	 ,@body)))

(defvar *in-form* nil)

(deftag-macro <ucw:form (&attribute action action* function
				    &allow-other-attributes others
				    &body body)
  "A Simple form which does not require javascript. "
  (%with-action-unique-names 
    `(let ((*in-form* t)) 
       (<:form :action (print-uri-to-string-sans-query ,url)
	       ,@others
	       (dolist (query (uri.query ,url))
		 (if (string= ,+action-parameter-name+ (car query))
		     (<:input :type "hidden" :name ,+action-parameter-name+
			      :value (cdr query)
			      :id ,action-id)
		     (<:input :type "hidden" :name (car query) :value (cdr query))))
	       ,@body))))

(deftag-macro <ucw:submit (&attribute action action* function value
				      &allow-other-attributes others
				      &body body)
  (%with-action-unique-names 
    `(<:input :type "submit" 
	      :value (or ,value ,@body)
	      :name (format nil "~A~A~A" 
			    ,+action-parameter-name+
			    ,+action-compound-name-delimiter+
			    ,action-id)
	      ,@others)))

;;; * CALLBACK tags

;;; All these tags take some kind of input, and execute a UCW callback.

(defmacro %with-callback-writer (&body body)
  "Bind WRITER to either WRITER or ACCESSOR."
  `(progn
     (assert (xor writer accessor) nil "Must supply one of WRITER or ACCESSOR")
     (let ((writer (or writer `(lambda (v) (setf ,accessor v)))))
       ,@body)))

(deftag-macro <ucw:input (&attribute accessor reader writer name
				     &allow-other-attributes others)
  (%with-callback-writer
    (let ((reader (or reader accessor)))
      `(<:input :value ,reader
                :name (register-callback ,writer :id ,name)
                ,@others))))

(deftag-macro <ucw:textarea (&attribute accessor reader writer name
					&allow-other-attributes others &body body)
  (%with-callback-writer
    (let ((reader (or reader accessor)))
       `(<:textarea 
         :name (register-callback ,writer :id ,name)
         ,@others
         ,@(cond (body `((<:as-html ,@body)))
		 (reader (with-unique-names (read-value)
			   `((when-bind ,read-value ,reader
			       (<:as-html ,read-value))))))))))



(deftag-macro <ucw::%select (&attribute writer accessor reader
					(test '#'eql) 
					(key '#'identity)
					name
                             &allow-other-attributes others
                             &body body)
  "The implementation of <ucw:select and tal tags with a :accessor (or :writer and :reader) attribute.

You need to supply either an accessor, or a writer and (optionally) a
reader to <ucw:select."
  (with-unique-names (v)
    (%with-callback-writer
      (let ((reader (or reader accessor)))
	`(let ((%select-table (cons t nil))) ; push on cdr to allow
					     ; <ucw:option to push
					     ; onto the dynamically
					     ; bound version
	   (let ((*%current-select-value* ,reader)
		 (*%current-select-test* ,test)
		 (*%current-select-key* ,key)
		 (*%select-table* %select-table))
	     (declare (special *%current-select-value*
			       *%current-select-test*
			       *%current-select-key*
			       *%select-table*))
	      (<:select :name (register-callback
			       (flet ((get-associated-value (v)
					(let ((v (assoc v (cdr %select-table)
							:test #'string=)))
					  (if v
					      (cdr v)
					      (error "Unknown option value: ~S." v)))))
				 (lambda (,v) (funcall ,writer (get-associated-value ,v))))
			       :id ,name)
			,@others
			,@body)))))))

(deftag-macro <ucw:select (&allow-other-attributes others
                           &body body)
  `(<ucw::%select ,@others ,@body))

(deftag-macro <ucw::%option (&attribute value &allow-other-attributes others &body body)
  (with-unique-names (value-id)
    (rebinding (value)
      `(let ((,value-id (random-string 10)))
	 (declare (special *%current-select-value*
			   *%current-select-test*
			   *%current-select-key*
			   *%select-table*))
	 (push (cons ,value-id ,value) (cdr *%select-table*))
	 (<:option :value ,value-id
		   ;;NB: we are applying key to both the option value
		   ;; being rendered, as well as the selected
		   ;; value(s).
		   
		   ;;That was how the code worked previously, I don't
		   ;;know if it is desirable.

		   ;;I think the alternative would be to apply the key
		   ;; to ",value" that is the option being rendered,
		   ;; and remove the :key argument from find.

		   ;;The logical operation we are trying to accomplish is
		   ;;(mapcar #'add-selected-attribute
		   ;;          (find-all %current-select-value(s)
		   ;;                    (list-of-collected-<ucw::%option-calls)
		   ;;                    :key %current-select-key))
		   :selected (when (find
				    (funcall *%current-select-key* ,value) ;key applied to an option
				    ;; fixme: multi selects are unsupported
				    (if nil ;%multiple
					*%current-select-value*
					(list *%current-select-value*))
				    :test *%current-select-test*
				    :key *%current-select-key*)
			       T)
		   ,@others ,@body)))))

(deftag-macro <ucw:option (&allow-other-attributes others &body body)
  "Replacement for the standard OPTION tag, must be used with
  <UCW:SELECT tag. Unlike \"regular\" OPTION tags the :value
  attribute can be any lisp object (printable or not)."
  `(<ucw::%option ,@others ,@body))
  
  
  

  


			    

