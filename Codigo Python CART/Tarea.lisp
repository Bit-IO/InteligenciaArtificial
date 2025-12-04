(defun subset (a b)
  (cond
    ((null a) t)
    ((member (car a) b :test #'equal)
     (subset (cdr a) b))
    (t nil)))

(defun inter (a b)
  (cond
    ((null a) nil)
    ((member (car a) b :test #'equal)
     (cons (car a) (inter (cdr a) b)))
    (t (inter (cdr a) b))))

(defun union (a b)
  (cond
    ((null a) b)
    ((member (car a) b :test #'equal)
     (union (cdr a) b))
    (t (cons (car a) (union (cdr a) b)))))

(defun dif (a b)
  (cond
    ((null a) nil)
    ((member (car a) b :test #'equal)
     (dif (cdr a) b))
    (t (cons (car a) (dif (cdr a) b)))))

(defun eliminar (x lista)
  (cond
    ((null lista) nil)
    ((equal x (car lista))
     (eliminar x (cdr lista)))
    (t (cons (car lista)
             (eliminar x (cdr lista))))))

(defun insertar-en (elem lista)
  (if (null lista)
      (list (list elem))
      (cons (cons elem lista)
            (mapcar (lambda (y)
                      (cons (car lista) y))
                    (insertar-en elem (cdr lista))))))

(defun perms (lista)
  (cond
    ((null lista) (list nil))
    (t (mapcan (lambda (p)
                 (insertar-en (car lista) p))
               (perms (cdr lista))))))

(defun inter-loop (a b)
  (loop for x in a
        when (member x b :test #'equal)
        collect x))

(defmacro repeat (n expr)
  `(loop for i from 1 to ,n do ,expr))

(defun crea-libro (titulo autor ed precio)
  (list :titulo titulo :autor autor :ed ed :precio precio))

(defvar *db* nil)

(defun agregar-reg (libro)
  (push libro *db*))

(defun listado-db ()
  (dolist (libro *db*)
    (format t "TITULO: ~a~%" (getf libro :titulo))
    (format t "AUTOR: ~a~%"  (getf libro :autor))
    (format t "ED: ~a~%"     (getf libro :ed))
    (format t "PRECIO: ~,2f~%" (getf libro :precio))))

(defun buscar-autor (autor)
  (remove-if-not (lambda (libro)
                   (equal (getf libro :autor) autor))
                 *db*))
