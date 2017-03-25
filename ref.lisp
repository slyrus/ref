
(cl:defpackage #:ref
  (:use :common-lisp)
  (:export #:ref
           #:hash-ref))

(cl:in-package #:ref)

(defun ref (plist &rest keys)
  (reduce #'getf keys :initial-value plist))

(defun %put-ref (new-value plist key &rest more-keys)
  ;; not quite Perl-style autovivification, but we do create
  ;; appropriate list structure for intermediate keys that can't be found
  (unless (listp plist) (setf plist nil))
  (let* ((sub (getf plist key))
         (val (if more-keys
                  (apply #'%put-ref new-value sub more-keys)
                  new-value)))
    (if sub
        (progn (setf (getf plist key) val) plist)
        (list* key val plist))))

(define-setf-expander ref (place &rest props
                                 &environment env)
  ;; %put-ref may cons new structure or mutate its argument.
  ;; all this magic is just so that we can
  ;; (let ((l nil)) (setf (ref l :foo :bar) t))
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion place env)
    (let ((newval (gensym))
          (ptemps (loop for i in props collect (gensym))))
      (values `(,@temps ,@ptemps)
              `(,@values ,@props)
              `(,newval)
              `(let ((,(car stores) (%put-ref ,newval ,get ,@ptemps)))
                ,set
                ,newval)
              `(ref ,get ,@ptemps)))))



(defun hash-ref (h &rest keys)                                                                                  
  (when h
    (reduce #'(lambda (h k) (when h (gethash k h))) keys :initial-value h)))                                                

(defun %put-hash-ref (new-value h key &rest more-keys)
  (unless (hash-table-p h)
    (setf h (make-hash-table :test 'equal)))
  (let* ((sub (gethash key h))
         (val (if more-keys
                  (apply #'%put-hash-ref new-value sub more-keys)
                  new-value)))
    (progn (setf (gethash key h) val) h)))

(define-setf-expander hash-ref (place &rest props
                                      &environment env)                                                             
  (multiple-value-bind (temps values stores set get)
      (get-setf-expansion place env)
    (let ((newval (gensym))
          (ptemps (loop for i in props collect (gensym))))
      (values `(,@temps ,@ptemps)
              `(,@values ,@props)
              `(,newval)
              `(let ((,(car stores) (%put-hash-ref ,newval ,get ,@ptemps)))
                ,set
                ,newval)
              `(hash-ref ,get ,@ptemps)))))
