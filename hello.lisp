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

(defun index/normal (s i)
  (cond
    ((> i (length s)) i)
    ((< i 0) 0)
    (t (length (render-row (subseq s 0 i))))))

(defun cursor/index (s i)
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
                              j)))))

(defparameter *ss*
  (loop for s in *raw-rows*
       collect (render-row s)))

(defparameter *status* "")
(defparameter *input-state* 'normal)

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
(defvar *escape*
  (code-char 27))
(defvar *ctrl-f*
  (code-char 6))
(defvar *ctrl-h*
  (code-char 8))
(defvar *ctrl-l*
  (code-char 12))

(defun write-at (window s x y)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (when (and (< y (- height 3)) (>= y 0))
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
(defparameter *last-pos-x* 0)
(defparameter *last-pos-y* 0)

(defun draw-status (window)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (charms/ll:mvaddnstr (- height 2) 0 *status* width)))
  
;  (multiple-value-bind (width height) (charms:window-dimensions window)
;    (write-at T *status* 0 (-1 height))))

(defun draw (h)
  (progn
    (loop for s in *ss*
          for y from (- h)
          do (write-at T s 0 y))
    (draw-status T)
    ))


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

(defmacro removef-lst-i (s i)
  `(setf ,s (remove-lst-i ,s ,i)))

(defun insert-lst (s w i)
    (concatenate 'list 
                 (subseq s 0 i)
                 (list w)
                 (subseq s i)))

(defun search-idx (dir y)
  (case dir
    (1 (loop ;for s in *ss*
         for i from y to (length *ss*)
         for s = (nth i *ss*)
         for sub-i = (search *status* s)
         if sub-i return (values i sub-i)
         finally (return NIL)))
    (-1 (loop 
          for i from y downto 0
          for s = (nth i *ss*)
          for sub-i = (search *status* s)
          if sub-i return (values i sub-i)
          finally (return NIL)))))

(defmacro parse-input-field (c x y)
  `(cond
     ((eq ,c NIL)
      ())
     ((eq ,c *enter*)
      (setf *input-state* 'normal))
     ((eq ,c *escape*)
      (progn
        (setf ,y *last-pos-y*)
        (setf ,x *last-pos-x*)
        (setf *input-state* 'normal)))
     ((or (eq ,c *left*) (eq ,c *up*))
      (multiple-value-bind (yy xx) (search-idx -1 (1- ,y))
        (when yy
          (progn
            (setf ,y yy)
            (setf ,x xx)))))
     ((or (eq ,c *right*) (eq ,c *down*))
      (multiple-value-bind (yy xx) (search-idx 1 (1+ ,y))
        (when yy
          (progn
            ;(write (write-to-string (list "nie" ,y yy)))
            (setf ,y yy)
            (setf ,x xx)))))
     ((eq ,c *backspace*)
      (setf *status* (str:substring 0 -1 *status*)))
     (T
      (progn
        (setf *status* (str:insert ,c (length *status*) *status*))
        
        (multiple-value-bind (yy xx) (search-idx 1 0)
          (when yy
            (progn
              (setf ,y yy)
              (setf ,x xx)))
          ;(write yy)
          ;(write (write-to-string xx))
         ; (setf ,x xx)
         )
        
        ))
     ))

(defmacro parse-input-normal (c x y)
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

    ((eq ,c *ctrl-f*) (progn
                        (setf *last-pos-y* ,y)
                        (setf *last-pos-x* ,x)
                        (setf *input-state* 'field)))
    ((eq ,c nil) nil) 

    (T               (let* ((raw-row (nth y *raw-rows*))
                           (i (cursor/index raw-row ,x)))
                       (progn
                       ;  (write (char-int ,c))
                         (when (string= raw-row "")
                           (setf i 0))
                         (setf-nth *raw-rows* ,y (str:insert ,c i raw-row))
                         (setf-nth *ss* ,y (render-row (nth ,y *raw-rows*)))
                         (setf x (index/normal (nth ,y *raw-rows*) (1+ i)))
                         ))))
  )

(defmacro parse-input (c x y)
  `(cond
    ((eq *input-state* 'normal) (parse-input-normal ,c ,x ,y))
    ((eq *input-state* 'field)  (parse-input-field ,c ,x ,y))))

(defun hello ()
  (charms:with-curses ()
        (charms:disable-echoing)
        (charms:enable-raw-input)
        (charms/ll:cbreak)
        (charms:enable-extra-keys T)
        (charms:clear-window T)
        (charms/ll:timeout 25)
        (setf charms/ll:*ESCDELAY* 25)
        
        (setf *height* (- (nth-value 1 (charms:window-dimensions T)) 3))
        ;(write (write-to-string *height*))
        
        (loop named hello
              with window = charms:*standard-window*
              with x = 0
              with y = 0
              with h = 0
              for c = (charms:get-char window :ignore-error t)
              do (progn
                   (charms:clear-window window :force-repaint nil)
                   ;(when c
                   ;  (write (write-to-string (char-int c))))
                   
                   (parse-input c x y)


                   ; SCROLL SCREEN
                   ; TODO: Refactor to outer function to clean up
                   ; FIX SCROLLING
                   (when (and (< (- y 4) h) (> h 0))
                     (setf h (max (- y 4) 0)))

                   ;(write (write-to-string y))

                   (when (>= (+ y 4) (+ *height* h))
                     (setf h (+ (- y *height*) 4))
                     )

                   (draw h)
                  ; (when (eq *input-state* 'field)
                  ;   (setf x 2))

                   ; MOVE CURSOR
                   ; TODO: Refactor to outer function to clean up
                   (let ((raw-row (nth y *raw-rows*)))

                         (move-cursor window 
                                      (cursor/normal raw-row x)
                                      (- y h) (length (nth y *ss*)))
                         )))))

