; append element to list
(defun list-append (list element)
  (if (null list)
      (list element)
    (cons element list)))