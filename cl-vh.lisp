(in-package :cl-vh)

(defvar *default-external-format* :utf-8)

(defvar *vh-text-style* (make-text-style :fix nil nil))

(defclass vh-buffer (drei-buffer)
  ((external-format :initform *default-external-format*
                    :accessor external-format)))

(defclass vh-pane (drei-pane esa-pane-mixin)
  ()
  (:metaclass esa-utils:modual-class)
  (:default-initargs
      :view (make-instance 'textual-drei-syntax-view
                           :buffer (make-instance 'vh-buffer))))

(defclass vh-minibuffer-pane (minibuffer-pane)
  ())

(make-command-table 'input-table :errorp nil)

(make-command-table 'edit-table :errorp  nil)

(define-application-frame vh (esa-frame-mixin standard-application-frame)
  ()
  (:menu-bar t)
  (:panes
   (window
    (let* ((*esa-instance* *application-frame*)
           (vh-pane (make-pane 'vh-pane :active t)))
      (setf (windows *application-frame*) (list vh-pane))
      (vertically () (scrolling () vh-pane ))))
   (minibuffer (make-pane 'vh-minibuffer-pane))
   ;;(interactor :interactor)
   )
  (:layouts
   (defalut
       (vertically (:scroll-bars nil)
         window
         minibuffer
         ;;interactor
         )))
  (:top-level (esa-top-level)))

(define-vh-command (com-quit :name t :menu t :keystroke (#\q :meta)) ()
  (frame-exit *application-frame*))

(defmethod drei-instance-of ((frame vh))
  (esa-current-window frame))

#+nil
(run-frame-top-level (make-instance 'vh))