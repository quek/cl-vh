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


(define-command-table insert-mode-command-table
    :inherit-from (self-insert-table))

(define-command-table command-mode-command-table
    :inherit-from (global-esa-table movement-table esa-io:esa-io-table))

(define-command-table ex-mode-command-table)

(defclass vh-mode ()
  ((command-table :initarg :command-table :accessor command-table)))

(defclass insert-mode (vh-mode)
  ()
  (:default-initargs
      :command-table (find-command-table 'insert-mode-command-table)))

(defclass command-mode (vh-mode)
  ()
  (:default-initargs
      :command-table (find-command-table 'command-mode-command-table)))

(defclass ex-mode (vh-mode)
  ()
  (:default-initargs
      :command-table (find-command-table 'ex-mode-command-table)))

(defvar *insert-mode* (make-instance 'insert-mode))

(defvar *command-mode* (make-instance 'command-mode))

(defvar *ex-mode* (make-instance 'ex-mode))

(define-application-frame vh (esa-frame-mixin standard-application-frame)
  ((mode :initform *command-mode*
         :accessor mode)
   (views :initform nil :accessor views))
  (:menu-bar t)
  (:panes
   (window
    (let* ((*esa-instance* *application-frame*)
           (vh-pane (make-pane 'vh-pane :active t))
           (info-pane (make-pane 'vh-info-pane :master-pane vh-pane)))
      (setf (windows *application-frame*) (list vh-pane)
            (views *application-frame*) (list (view vh-pane)))
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
  (:top-level (esa-top-level :prompt ":")))

(defun input-from-stream (stream buffer offset)
  (let* ((seq (make-string (file-length stream)))
         (count (read-sequence seq stream)))
    (insert-buffer-sequence buffer offset
                            (if (= count (length seq))
                                seq
                                (subseq seq 0 count)))))

(defmethod frame-make-buffer-from-stream ((vh vh) stream)
  (let ((buffer (make-new-buffer)))
    (input-from-stream stream buffer 0)
    (clear-undo-history buffer)
    buffer))

(defmethod frame-make-new-buffer ((vh vh) &key (name "*scratch*"))
  (make-instance 'vh-buffer :name name))

(defmethod buffers ((vh vh))
  (mapcar #'buffer (views vh)))

(defun change-to-insert-mode (vh)
  (setf (mode vh) *insert-mode*))

(defun change-to-command-mode (vh)
  (setf (mode vh) *command-mode*))

(defun change-to-ex-mode (vh)
  (setf (mode vh) *ex-mode*))

(defun insert-mode-p (vh)
  (eq (mode vh) *insert-mode*))

(defun command-mode-p (vh)
  (eq (mode vh) *command-mode*))

(defun ex-mode-p (vh)
  (eq (mode vh) *ex-mode*))

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
  (if (insert-mode-p frame)
      (command-for-unbound-gestures (esa-current-window frame) gestures)))

(set-key `(drei-commands::com-self-insert ,*numeric-argument-marker*)
         'insert-mode-command-table
	 '((#\Newline)))

(define-command (com-command-mode :command-table insert-mode-command-table) ()
  (change-to-command-mode *application-frame*))

(define-command (com-insert :command-table command-mode-command-table) ()
  (change-to-insert-mode *application-frame*))

(define-command (com-append :command-table command-mode-command-table) ()
  (change-to-insert-mode *application-frame*)
  (ignore-errors
    (execute-frame-command *application-frame*
                           `(drei-commands::com-forward-object 1))))


(define-command (com-edit :command-table ex-mode-command-table :name t)
    ((filepath 'pathname))
  (handler-case (find-file filepath)
    (file-error (e)
      (display-message "~A" e))))

(define-command (com-quit :command-table ex-mode-command-table :name t) ()
  (frame-exit *application-frame*))


(set-key `(com-command-mode ,*numeric-argument-marker*)
         'insert-mode-command-table
         '((:escape)))

(set-key `(com-insert ,*numeric-argument-marker*)
	 'command-mode-command-table
	 '((#\i)))

(set-key `(com-append ,*numeric-argument-marker*)
         'command-mode-command-table
         '((#\a)))

(set-key `(drei-commands::com-forward-object ,*numeric-argument-marker*)
	 'command-mode-command-table
	 '((#\n)))

(set-key `(drei-commands::com-backward-object ,*numeric-argument-marker*)
	 'command-mode-command-table
	 '((#\d)))

(set-key `(drei-commands::com-forward-line ,*numeric-argument-marker*)
	 'command-mode-command-table
	 '((#\h)))

(set-key `(drei-commands::com-backward-line ,*numeric-argument-marker*)
	 'command-mode-command-table
	 '((#\t)))



(define-command (|com-:| :command-table command-mode-command-table :name t) ()
  "ex mode"
  (let ((item (handler-case
                  (accept
                   `(command :command-table
                             ,(find-command-table 'ex-mode-command-table))
                   ;; this gets erased immediately anyway
                   :prompt "" :prompt-mode :raw)
                ((or command-not-accessible command-not-present) ()
                  (beep)
                 (display-message "No such command")
                 (return-from |com-:| nil)))))
    (execute-frame-command *application-frame* item)))

;;  (change-to-ex-mode *application-frame*) ; ← でどうしてうまくいかないんだろう？
;;  (print (command-table *application-frame*))
;;  (unwind-protect
;;       ;; accept が command-mode-commant-table で実行されてしまい h とか入力できない。
;;       (let ((input (accept 'string :prompt ":" :prompt-mode :raw)))
;;         (print input))
;;    (change-to-command-mode *application-frame*)))

(set-key '|com-:| 'command-mode-command-table '((#\:)))

(defvar *vh*)
(defun vh ()
  (setf *vh* (make-instance 'vh))
  (run-frame-top-level *vh*))


#+nil
(vh)