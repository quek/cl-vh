(in-package :cl-vh)

(defvar *default-external-format* :utf-8)

(defvar *vh-text-style* (make-text-style :fix nil nil))

(defclass vh-info-pane (info-pane)
  ()
  (:default-initargs
      :height 20 :max-height 20 :min-height 20
    :display-function 'display-info
    :incremental-redisplay t))

(defun display-info (frame pane)
  (format pane "~a ~a" frame pane))

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


(define-command-table vh-command-table
    :inherit-from (global-esa-table keyboard-macro-table))

(make-command-table 'input-table :errorp nil)

(make-command-table 'edit-table :errorp  nil)

(define-application-frame vh (esa-frame-mixin standard-application-frame)
  ()
  (:menu-bar t)
  (:panes
   (window
    (let* ((*esa-instance* *application-frame*)
           (vh-pane (make-pane 'vh-pane :active t
                               :command-table 'vh-command-table))
           (info-pane (make-pane 'vh-info-pane :master-pane vh-pane)))
      (setf (windows *application-frame*) (list vh-pane))
      (vertically ()
        (scrolling () vh-pane )
        info-pane)))
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

(defmethod command-for-unbound-gestures ((frame vh) gestures)
  (command-for-unbound-gestures (esa-current-window frame) gestures))

#+nil
(run-frame-top-level (make-instance 'vh))