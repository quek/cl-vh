(in-package :cl-vh)

(defvar *default-external-format* :utf-8)

(defvar *vh-text-style* (make-text-style :fix nil nil))

(defclass vh-pane (drei-pane esa-pane-mixin)
  ()
  (:metaclass esa-utils:modual-class))

(defclass vh-buffer (drei-buffer)
  ((external-format :initform *default-external-format*
                    :accessor external-format)))

(defclass vh-minibuffer-pane (minibuffer-pane)
  ())

(define-application-frame vh-frame (esa-frame-mixin standard-application-frame)
  ()
  (:menu-bar t)
  (:panes
   (vh-window
    (let* ((*esa-instance* *application-frame*)
           (vh-pane (make-pane 'vh-pane :active t)))
      (vertically () (scrolling () vh-pane ))))
   (minibuffer (make-pane 'vh-minibuffer-pane))
   (interactor :interactor))
  (:layouts
   (defalut
       (vertically (:scroll-bars nil)
         vh-window
         minibuffer
         interactor))))

(define-vh-frame-command (com-quit :name t :menu t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

#+nil
(run-frame-top-level (make-instance 'vh-frame))