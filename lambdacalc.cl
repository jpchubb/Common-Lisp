;; ==================== \ Calculator
;;; Author:
;; Black Unicorn <jdxyw2004@gmail.com>
;; Joshua Chubb <chubb.jp@gmail.com>
;;
;;; DATE: 29/10/2011
;; ========================================
;; 
;; Zero, Succ, Plus, Mult, Pred, Sub, Exp, czerop, int-to-cn, cn-to-int
;; from www.jdxyw.com/?p=1098
;; 
;; ========================================

;; \fx. x
(defvar zero
	   (lambda (f)
	     (lambda (x) x)))

;; \nfx. f (n f x)
(defun succ(n)
	   (lambda (f)
	     (lambda (x) (funcall f (funcall (funcall n f) x)))))

;; \mnfx. m f (n f x)
(defun plus (m n)
	   (lambda (f)
	     (lambda (x) (funcall (funcall m f) (funcall (funcall n f) x))))) 

;; \mnf. n (m f)
(defun mult(m n)
	   (lambda (f)
	     (funcall n (funcall m f))))

;; \nfx. n (\gh. h (g f)) (\u. x) (\u. u)
(defun pred (n)
	   (lambda (f)
	     (lambda (x)
	       (funcall (funcall (funcall n (lambda (g)
					      (lambda (h)
						(funcall h (funcall g f)))))
				 (lambda (u) x))
			(lambda (u)u)))))

;; \mn. (n pred) m 
(defun sub(m n)
	   (funcall (funcall n #'pred) m))

;; \mn. n m
(defun cexp(m n)
	   (funcall n m))

;; isZero
;; \n. n (\x. F) T
;; I should probably redo this in logic when I get the chance
(defun czerop(n)
	   (funcall (funcall n (lambda (x) nil)) t))


;; ================================ ;;
;;; Playing with the outside world  ;;
;; ================================ ;;

;; turn an int into a cn
(defun int-to-cn (n)
	   (if (= n 0)
	       (lambda (f)
		 (lambda(x) x))
	       (lambda (f)
		 (lambda (x)
		   (funcall f (funcall (funcall (int-to-cn (- n 1)) f) x))))))

;; and back the other way...
(defun cn-to-int (cn)
		(funcall (funcall cn (lambda (x) (+ x 1))) 0))
