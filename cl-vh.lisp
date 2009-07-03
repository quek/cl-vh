(in-package :cl-vh)

(define-application-frame vh-frame (esa-frame-mixin standard-application-frame)
  ()
  (:menu-bar t)
  (:panes
   (interactor :interactor))
  (:layouts
   (defalut interactor)))

(define-vh-frame-command (com-quit :name t :menu t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

#+nil
(run-frame-top-level (make-instance 'vh-frame))