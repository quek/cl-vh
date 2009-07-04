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
  (format pane "~a" (mode frame)))

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


(define-command-table input-command-table
    :inherit-from (self-insert-table))

(define-command-table edit-command-table
    :inherit-from (movement-table))

(defclass vh-mode ()
  ((command-table :initarg :command-table :accessor command-table)))

(defclass vh-input-mode (vh-mode)
  ()
  (:default-initargs :command-table (find-command-table 'input-command-table)))

(defclass vh-edit-mode (vh-mode)
  ()
  (:default-initargs :command-table (find-command-table 'edit-command-table)))

(defvar *input-mode* (make-instance 'vh-input-mode))

(defvar *edit-mode* (make-instance 'vh-edit-mode))


(define-application-frame vh (esa-frame-mixin standard-application-frame)
  ((mode :initform *edit-mode*
         :accessor mode))
  (:menu-bar t)
  (:panes
   (window
    (let* ((*esa-instance* *application-frame*)
           (vh-pane (make-pane 'vh-pane :active t))
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

(defun change-to-input-mode (vh)
  (setf (mode vh) *input-mode*))

(defun change-to-edit-mode (vh)
  (setf (mode vh) *edit-mode*))

(defun input-mode-p (vh)
  (eq (mode vh) *input-mode*))

(defun edit-mode-p (vh)
  (eq (mode vh) *edit-mode*))

(defmethod find-applicable-command-table ((vh vh))
  (command-table (mode vh)))

(defmethod frame-command-table ((vh vh))
  (find-applicable-command-table vh))

(defmethod drei-instance-of ((frame vh))
  (esa-current-window frame))

(defmethod command-table ((pane vh-pane))
  (command-table (pane-frame pane)))

(defmethod command-for-unbound-gestures ((frame vh) gestures)
  "for self insert"
  (if (input-mode-p frame)
      (command-for-unbound-gestures (esa-current-window frame) gestures)))

(set-key `(drei-commands::com-self-insert ,*numeric-argument-marker*)
         'self-insert-table
	 '((#\Newline)))

(define-command (com-edit-mode :command-table input-command-table) ()
  (change-to-edit-mode *application-frame*))

(define-command (com-quit :name t :command-table edit-command-table) ()
  (frame-exit *application-frame*))

(define-command (com-insert :command-table edit-command-table) ()
  (change-to-input-mode *application-frame*))

(set-key `(com-edit-mode ,*numeric-argument-marker*)
         'input-command-table
         '((:escape)))

(set-key `(com-insert ,*numeric-argument-marker*)
	 'edit-command-table
	 '((#\i)))

(set-key `(drei-commands::com-forward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\n)))

(set-key `(drei-commands::com-backward-object ,*numeric-argument-marker*)
	 'movement-table
	 '((#\d)))

(set-key `(drei-commands::com-forward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\h)))

(set-key `(drei-commands::com-backward-line ,*numeric-argument-marker*)
	 'movement-table
	 '((#\t)))


(define-command (com-extended-command :command-table edit-command-table) ()
  "Prompt for a command name and arguments, then run it."
  (let ((item (handler-case
                  (accept
                   '(command :command-table edit-command-table)
                   ;; this gets erased immediately anyway
                   :prompt t :prompt-mode :raw)
                ((or command-not-accessible command-not-present) ()
                  (beep)
                  (display-message "No such command")
                  (return-from com-extended-command nil)))))
    (execute-frame-command *application-frame* item)))

(set-key 'com-extended-command 'edit-command-table '((#\:)))


#+nil
(run-frame-top-level (make-instance 'vh))