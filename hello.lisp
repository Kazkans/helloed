(ql:quickload :cl-charms)
(ql:quickload "str")

(defparameter *raw-rows*
  (uiop:read-file-lines "ed.adb"))

(defparameter *tab-width* 4)

(defun tab-width-i (i)
          (- *tab-width* (mod i *tab-width*)))

(defun first-tab-to-space (s i)
  (let ((how-many 
          (tab-width-i i)))
    (concatenate 'string 
                 (subseq s 0 i)
                 (str:repeat how-many " ")
                 (subseq s (+ 1 i)))))

(defun render-row (s)
  (progn 
    (loop
      for p = (position #\tab s)
      while p
      do (setq s (first-tab-to-space s p)))
    s))

(defun cursor/normal (s i)
  (loop for c across s
        with cur = 0
        with prev = 0
        for j from 0
        when (> cur i)
                return prev
        do (progn 
             (setq prev cur)
             (if (eq c #\tab)
                 (setq cur (+ cur (tab-width-i cur)))
                 (setq cur (+ cur 1))))
        finally (return i)))

(defun logt (v)
  (progn
    (charms:write-string-at-point T (write-to-string v) 10 12)
    v))

(defun index/normal_ (s i)
  (cond
    ((> i (length s)) i)
    ((< i 0) 0)
    (t (length (render-row (subseq s 0 i))))))

(defun index/normal (s i)
  (let ((o (index/normal_ s i)))
    (progn
     ;   (write (write-to-string (list "in" i o)))
        o)))

(defun cursor/index_ (s i)
  (loop for c across s
        with cur = 0
        for j from 0
        when (> cur i)
                return (- j 1)
        do (if (eq c #\tab)
               (setq cur (+ cur (tab-width-i cur)))
               (setq cur (+ cur 1)))
        finally (return (progn
                          (if (>= i cur)
                              (1+ j)
                              j
                              )
                           )
                ; return j
                 )))

(defun cursor/index (s i)
  (let ((o (cursor/index_ s i)))
    (progn
        ;(write (write-to-string (list i o)))
        o)))

(defparameter *ss*
  (loop for s in *raw-rows*
       collect (render-row s)))

(defvar *right*
  (code-char 261))
(defvar *left*
  (code-char 260))
(defvar *up*
  (code-char 259))
(defvar *down*
  (code-char 258))
(defvar *backspace*
  (code-char 263))
(defvar *enter*
  (code-char 10))
(defvar *del*
  (code-char 330))
(defvar *ctrl-h*
  (code-char 8))
(defvar *ctrl-l*
  (code-char 12))

(defun write-at (window s x y)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (when (and (< y (- height 1)) (>= y 0))
      (charms/ll:mvaddnstr y x s width)
 ;     (charms:write-string-at-point window s x y)
      )))

(defmacro dec (x)
  `(when (> ,x 0) (decf ,x)))

(defmacro inc (x i)
  `(when (< ,x ,i) (incf ,x)))

(defmacro dec-cap (x i)
  `(when (> ,x 0) (setf ,x (- (min ,x ,i) 1))))

(defun inc-i (x i)
  (when (< x i) (+ x 1)))

(defun dec-cap-i (x i)
  (when (> x 0) (- (min x i) 1)))

(defun move-cursor (window x y clip)
  (if (< x clip)
    (charms:move-cursor window x y)    
    (charms:move-cursor window clip y)))

(defun nth-length (i text)
  (length (nth i text)))

(defparameter *height* 20)

(defun draw (h)
   (loop for s in *ss*
         for x from (- h)
         do (write-at T s 0 x)))


(defun set-nth (list n value)
  (substitute value nil list
             :test (constantly t)
             :start n
             :count 1))

(defmacro setf-nth (list n value)
  `(setf ,list (set-nth ,list ,n ,value)))

(defun remove-i (s i)
    (concatenate 'string 
                 (subseq s 0 i)
                 (subseq s (+ 1 i))))

(defun remove-lst-i (s i)
    (concatenate 'list 
                 (subseq s 0 i)
                 (subseq s (+ 1 i))))
+(defmacro removef-lst-i (s i)
  `(setf ,s (remove-lst-i ,s ,i)))

(defun insert-lst (s w i)
    (concatenate 'list 
                 (subseq s 0 i)
                 (list w)
                 (subseq s i)))

; TODO: Fix deleting empty lines
(defmacro parse-input (c x y)
  `(cond 
    ((eq ,c *up*)    (dec ,y))
    ((eq ,c *down*)  (inc ,y (- (length *ss*) 1)))

    ((eq ,c *right*) (setf ,x (min 
                              (nth-length ,y *ss*)
                              (index/normal (nth ,y *raw-rows*) 
                                            (min 
                                              (1+ (cursor/index (nth ,y *raw-rows*) x)) 
                                              (nth-length ,y *raw-rows*))))))
    ((eq ,c *left*)  (setf ,x (index/normal (nth ,y *raw-rows*) 
                                          (1- (cursor/index (nth ,y *raw-rows*) ,x)))))

    ((eq ,c #\q)     (return-from hello))
    ((eq ,c *enter*) 
     (progn
       (let* ((row (nth ,y *raw-rows*))
             (i (cursor/index row x)))
         (progn
           (when (string= row "")
               (setf i 0))
           (setf *raw-rows*
                 (insert-lst *raw-rows*
                             (subseq row i)
                             (1+ ,y)))
           (setf-nth *raw-rows* ,y (subseq row 0 i))
           (setf-nth *ss* ,y (render-row (nth ,y *raw-rows*)))))

       (setf *ss* (insert-lst *ss* (render-row (nth (1+ ,y) *raw-rows*)) (1+ ,y)))
       (setf ,y (1+ ,y))
       (setf ,x 0)))
       ;(change-cursro)
    ; TODO: ADD deleting lines (at 0 index special case) aslo think what about empty file
    ((eq ,c *backspace*) 
     (let* ((raw-row (nth y *raw-rows*))
            (i (cursor/index raw-row ,x)))
       (if (or (= i 0) (= (length (nth ,y *ss*)) 0))
           (when (not (= ,y 0))
             (progn
               (when (> (length raw-row) 0)
                 (progn
                   (setf-nth *raw-rows* (1- ,y) (concatenate 'string
                                                             (nth (1- ,y) *raw-rows*)
                                                             raw-row))
                   (setf-nth *ss* (1- ,y) (render-row (nth (1- ,y) *raw-rows*)))))
               (removef-lst-i *ss* ,y)   
               (removef-lst-i *raw-rows* ,y)

               (let ((new-row (nth (1- ,y) *raw-rows*)))
                 (setf ,x (index/normal new-row (length new-row))))

               (decf ,y)))
           (progn 
             (setf-nth *raw-rows* ,y (remove-i raw-row (1- i)))
             (setf-nth *ss* ,y (render-row (nth ,y *raw-rows*)))
             (setf x (index/normal (nth ,y *raw-rows*) (1- i)))))))

    ((eq ,c nil) nil) 

    (T               (let* ((raw-row (nth y *raw-rows*))
                           (i (cursor/index raw-row ,x)))
                       (progn
                         (when (string= raw-row "")
                           (setf i 0))
                        (setf-nth *raw-rows* ,y (str:insert ,c i raw-row))
                        (setf-nth *ss* ,y (render-row (nth ,y *raw-rows*)))
                        (setf x (index/normal (nth ,y *raw-rows*) (1+ i)))
                        ))))


  )

(defun hello ()
  (charms:with-curses ()
        (charms:disable-echoing)
        (charms:enable-raw-input)
     ;  (charms/ll:nocbreak)
     ;   (charms/ll:timeout 0)
        (charms/ll:cbreak)
        (charms:enable-extra-keys T)
        (charms:clear-window T)

        (setf *height* (nth-value 1 (charms:window-dimensions T)))
        
        (loop named hello
              with window = charms:*standard-window*
              with x = 0
              with y = 0
              with h = 0
              for c = (charms:get-char window :ignore-error t)
              do (progn
                   (charms:clear-window window :force-repaint nil)
                   
                   (parse-input c x y)
                   (draw h)

                   ; SCROLL SCREEN
                   ; TODO: Refactor to outer function to clean up
                   (when (and (< (- y 4) h) (> h 0))
                     (decf h))
                   (when (>= (+ y 4) (+ *height* h))
                     (incf h))

                   ; MOVE CURSOR
                   ; TODO: Refactor to outer function to clean up
                   (let ((raw-row (nth y *raw-rows*)))

                         (move-cursor window 
                                      (cursor/normal raw-row x)
                                      (- y h) (length (nth y *ss*)))
                         )))))

