(in-package :main)

(ql:quickload :cl-charms)
(ql:quickload "str")
(ql:quickload "bt-semaphore")

(defparameter *raw-rows*
  (uiop:read-file-lines "test.c"))

(defparameter *tab-width* 4)

(defparameter *height* 20)
(defparameter *last-pos-x* 0)
(defparameter *last-pos-y* 0)

(defparameter *arr* NIL)

(defstruct highlight
  y
  start
  end)

(defstruct scanner
  x
  y
  s
  storage)

(defparameter *highlight*
  NIL)

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
(defvar *ctrl-w*
  (code-char 23))
(defvar *ctrl-f*
  (code-char 6))
(defvar *ctrl-h*
  (code-char 8))
(defvar *ctrl-l*
  (code-char 12))

(defun in-highlight (y x hl)
  (and
    hl
    (= (highlight-y hl) y)
    (<= (highlight-start hl) x)
    (> (highlight-end hl) x)))

(defun write-at (window s x y row)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (when (and (< y (- height 3)) (>= y 0))
      (loop for i from 0 below (min width (length s))
            for c = (char s i)
            do (progn 
                 (if (in-highlight row i *highlight*) 
                     (charms/ll:attron (charms/ll:COLOR-PAIR 2))
                     (charms/ll:attron (charms/ll:COLOR-PAIR 1)))
                 (charms:write-char-at-point window c (+ x i) y)))
      ;(charms/ll:mvaddnstr y x s width)
 ;     (charms:write-string-at-point window s x y)
      )))

(defun get-arr (arr x y)
  (when (and (< y (length arr))
             (< x (length (aref arr y))))
    (aref (aref arr y) x)))

(defun write-at-hl (window s x y hl y-idx)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (when (and (< y (- height 3)) (>= y 0))
      (loop for i from 0 below (min width (length s))
            for c = (char s i)
            do (progn 
                 (case (get-arr hl i y-idx)
                   ('NUM (charms/ll:attron (charms/ll:COLOR-PAIR 2)))
                   ('KEYWORD (charms/ll:attron (charms/ll:COLOR-PAIR 3)))
                   ('COM (charms/ll:attron (charms/ll:COLOR-PAIR 4)))
                   (otherwise (charms/ll:attron (charms/ll:COLOR-PAIR 1))))
                 (when (in-highlight y-idx i *highlight*)
                     (charms/ll:attron (charms/ll:COLOR-PAIR 5)))
                 (charms:write-char-at-point window c (+ x i) y)
                 (charms/ll:attron (charms/ll:COLOR-PAIR 1)))))))

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

(defun draw-status (window)
  (multiple-value-bind (width height) (charms:window-dimensions window)
    (charms/ll:mvaddnstr (- height 2) 0 *status* width)))
  
;  (multiple-value-bind (width height) (charms:window-dimensions window)
;    (write-at T *status* 0 (-1 height))))

(defun scanner-cur (s)
  (nth (scanner-y s) (scanner-s s)))

(defun save (scanner)
  (values (scanner-x scanner) (scanner-y scanner)))

(defmacro revert (scanner pos)
  `(multiple-value-bind (x y) ,pos
    (setf (scanner-x ,scanner) x)
    (setf (scanner-y ,scanner) y)))

(defun peek (s)
  (progn
    ;(write (write-to-string (scanner-y s)))
    ;(write (scanner-x s))
    ;  (write (list (scanner-x s) (scanner-y s)))
    (if (< (scanner-x s) (length (scanner-cur s)))
        (char (scanner-cur s) (scanner-x s))
        (if (< (scanner-y s) (1- (length (scanner-s s))))
            #\linefeed  
            #\Null))))
 ; (if (>= (scanner-x s) (length (scanner-cur s)))
 ;     #\linefeed
 ;     (char (scanner-cur s) (scanner-x s))))

(defmacro eat (s)
  `(progn
    (setf (scanner-storage ,s) 
          (nconc (scanner-storage ,s) (list (list (scanner-x ,s) (scanner-y ,s)))))
    (if (>= (scanner-x ,s) (length (scanner-cur ,s)))
        ; (when (< (scanner-y ,s) (length (scanner-s ,s)))
        (progn
          (setf (scanner-x ,s) 0)
          (incf (scanner-y ,s)));)
        (incf (scanner-x ,s)))))

(defmacro reset-s (s)
  `(setf (scanner-storage ,s) NIL))

(defun is-digit (c)
  (digit-char-p c))

(defun is-whitespace (c)
  (or (char= c #\Space)
      (char= c #\Tab)
      (char= c #\Newline)
      (char= c #\Return)
      (char= c #\Page)
      (char= c #\Linefeed)
      (char= c #\Null)))

(defparameter +seperators+ ",.()+-/*=~%<>^[];!|")

(defun is-seperator (c)
  (or
    (position c +seperators+)
    (is-whitespace c)))

(defun is-alpha (c)
  (or
    (alpha-char-p c)
    (char= c #\_)))

(defmacro save-storage-hl (scanner arr hl)
  `(progn  
    (dolist (e (scanner-storage ,scanner))
       (let ((x (first e))
             (y (second e)))
         (progn
           (with-open-file (stream "put.txt"
                                   :direction :output
                                   :if-exists :append
                                   :if-does-not-exist :create)
             (write-string (write-to-string (list x y)) stream))
           (setf (aref (aref ,arr y) x) ,hl))))))

(defun storage-to-string (scanner)
  (let ((out "")
        (s (scanner-s scanner)))
    (progn
      (dolist (e (scanner-storage scanner))
        (let ((x (first e))
              (y (second e)))
          (setf out (concatenate 'string 
                                 out 
                                 (string (char (nth y s) x))))))
      out)))

; we have first digit
(defmacro parse-num (scanner arr)
  `(progn
    (reset-s ,scanner)
    (loop while (is-digit (peek ,scanner))
          do (eat ,scanner))
    (when (eq (peek ,scanner) #\.) (eat ,scanner))
    (loop while (is-digit (peek ,scanner))
          do (eat ,scanner))
    (is-seperator (peek ,scanner))
    (save-storage-hl ,scanner ,arr 'NUM)))


(defparameter *keywords* '("switch" "if" "while" "for" "break" "continue" "return" "else" "struct" "union" "typedef" "static" "enum" "class" "case" "int" "long" "double" "float" "char" "unsigned" "signed" "void"))

; TODO MAKE KEYWORD
(defmacro parse-keyword (scanner arr)
  `(progn
     (reset-s ,scanner)
     (loop
       for c = (peek ,scanner)
       while (or (is-alpha c) (is-digit c))
       do (eat ,scanner))
     (is-seperator (peek ,scanner))
     (when (member (storage-to-string scanner) *keywords* :test 'string=)
       (save-storage-hl ,scanner ,arr 'KEYWORD))))


(defun check-for-*/ (scanner)
  (if (eq (peek scanner) #\*)
      (progn
        (eat scanner)
        (eq (peek scanner) #\/))))

(defmacro parse-comment (scanner arr)
  `(let ((found-comment T))
     (progn
       (reset-s ,scanner)
       (eat ,scanner)
       (case (peek ,scanner)
         (#\*
          (progn
            (eat ,scanner)
            (loop while (not (check-for-*/ ,scanner))
                  do (eat ,scanner))
            (eat ,scanner)))
         (#\/
          (loop while (not (eq (peek ,scanner) #\Newline))
                do (eat ,scanner)))
         (t (setf found-comment nil)))
       (when found-comment
         (save-storage-hl ,scanner ,arr 'COM)))))

(defmacro parse-string (scanner arr)
  `(progn
     (reset-s ,scanner)
     (eat ,scanner)
     (loop
       for c = (peek ,scanner)
       while (not (or (eq c #\") (eq c #\Null)))
       do (progn
            (when (eq c #\\)
              (eat ,scanner))
            (eat ,scanner)))
     (eat ,scanner)
     (save-storage-hl ,scanner ,arr 'COM)))

(defmacro parse-char (scanner arr)
  `(progn
     (reset-s ,scanner)
     (eat ,scanner)
     (when (eq (peek ,scanner) #\\)
       (eat ,scanner))
     (eat ,scanner)
     (when (eq (peek ,scanner) #\')
       (progn
         (eat ,scanner)
         (save-storage-hl ,scanner ,arr 'COM)))))

; TODO ADD strings
(defun parse (scanner)
  (let ((arr (make-array (length *ss*))))
    (progn
      (loop for s in *ss*
            for y from 0
            do (setf (aref arr y) (make-array (1+ (length s)))))
      (loop 
        for c = (peek scanner)
        while (not (char= c #\Null))
        do (cond
             ((is-digit c) (parse-num scanner arr))
             ((is-alpha c) (parse-keyword scanner arr)) ; //
             ((char= c #\') (parse-char scanner arr))
             ((char= c #\") (parse-string scanner arr))
             ((char= c #\/) (parse-comment scanner arr))
           ;  ((char= c #\") (parse-string scanner arr))
             (T (eat scanner))))
      arr
      )))

(defparameter *syntax-thread* nil)

(defparameter *arr* nil)

(defun parse-f ()
 ; (setf *arr* (parse (make-scanner :x 0 :y 0 :s *ss*))))
  (progn
    (when (and *syntax-thread* (bt:thread-alive-p *syntax-thread*))
      (bt:destroy-thread *syntax-thread*))

    (setf *syntax-thread* 
          (bt:make-thread
            (lambda ()
              (setf *arr* (parse (make-scanner :x 0 :y 0 :s *ss*)))
             )))))

; bt:destroy-thread

; TODO use parse in another thread
(defun draw (h)
    (progn
      (loop for s in *ss*
            for y from (- h)
            for i from 0
            do (progn 
                 (write-at-hl T s 0 y *arr* i)))
      (draw-status T)))



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

(defun escape-input-state ()
  (setf *input-state* 'normal)
  (setf *highlight* NIL))

(defmacro search-set (x y dir)
  `(let ((y-start (if (= ,dir 0) 0 ,y))
         (dir (if (= ,dir 0) 1 ,dir)))
     (multiple-value-bind (yy xx) (search-idx dir (+ dir y-start))
       (when yy
         (progn
           (setf ,y yy)
           (setf ,x xx)
           (setf *highlight* (make-highlight :y yy :start xx :end (+ xx (length *status*)))))))))

(defmacro parse-input-field (c x y)
  `(cond
     ((eq ,c NIL)
      ())
     ((eq ,c *enter*)
      (escape-input-state))
     ((eq ,c *escape*)
      (progn
        (setf ,y *last-pos-y*)
        (setf ,x *last-pos-x*)
        (escape-input-state)))
     ((or (eq ,c *left*) (eq ,c *up*))
      (search-set ,x ,y -1))
     ((or (eq ,c *right*) (eq ,c *down*))
      (search-set ,x ,y 1))
     ((eq ,c *backspace*)
      (setf *status* (str:substring 0 -1 *status*)) 
      (search-set ,x ,y 0))
     (T
      (progn
        (setf *status* (str:insert ,c (length *status*) *status*))
        (search-set ,x ,y 0)))))

(defun key-changing (c)
  (not (or 
             (eq c *up*)
             (eq c *down*)
             (eq c *right*)
             (eq c *left*)
             (eq c *ctrl-w*)
             (eq c NIL)
             (eq c *ctrl-f*))))

(defmacro parse-input-normal (c x y)
  `(progn 
     (when (not (eq ,c nil))
       (write (write-to-string (char-int ,c))))
     (cond 
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

       ((eq ,c *ctrl-w*)     (return-from hello))
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
                  (let ((new-row (nth (1- ,y) *raw-rows*)))
                    (setf ,x (index/normal new-row (length new-row))))

                  (when (> (length raw-row) 0)
                    (progn
                      (setf-nth *raw-rows* (1- ,y) (concatenate 'string
                                                                (nth (1- ,y) *raw-rows*)
                                                                raw-row))
                      (setf-nth *ss* (1- ,y) (render-row (nth (1- ,y) *raw-rows*)))))
                  (removef-lst-i *ss* ,y)   
                  (removef-lst-i *raw-rows* ,y)
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
                            (when (string= raw-row "")
                              (setf i 0))
                            (setf-nth *raw-rows* ,y (str:insert ,c i raw-row))
                            (setf-nth *ss* ,y (render-row (nth ,y *raw-rows*)))
                            (setf x (index/normal (nth ,y *raw-rows*) (1+ i)))))))
     (when (key-changing ,c)
       (progn
         (parse-f)))))

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
        (charms/ll:start-color)
        (charms/ll:init-pair 1 charms/ll:COLOR_WHITE charms/ll:COLOR_BLACK)
        (charms/ll:init-pair 2 charms/ll:COLOR_BLUE charms/ll:COLOR_BLACK)
        (charms/ll:init-pair 3 charms/ll:COLOR_YELLOW charms/ll:COLOR_BLACK)
        (charms/ll:init-pair 4 charms/ll:COLOR_GREEN charms/ll:COLOR_BLACK)
        (charms/ll:init-pair 5 charms/ll:COLOR_RED charms/ll:COLOR_BLACK)
      ;  (charms/ll:attron (charms/ll:COLOR-PAIR 2))
        (setf charms/ll:*ESCDELAY* 25)
        
        (setf *height* (- (nth-value 1 (charms:window-dimensions T)) 3))
        
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

                   (when (>= (+ y 4) (+ *height* h))
                     (setf h (+ (- y *height*) 4)))

                   (draw h)

                   ; MOVE CURSOR
                   ; TODO: Refactor to outer function to clean up
                   (let ((raw-row (nth y *raw-rows*)))

                         (move-cursor window 
                                      (cursor/normal raw-row x)
                                      (- y h) (length (nth y *ss*)))
                         )))))

