
(library (linux inotify)
  (export inotify-init 
	  inotify-init1
	  inotify-add-watch
	  inotify-rm-watch
	  inotify-flags
	  inotify-flags->int
	  inotify-int->flags
	  inotify-read-port)
  (import 
    (chezscheme)
    (rnrs bytevectors)
    )

;;   "simple wrapper for inotify
;; WARNNING: errno is unreliable"
  (define libc
    (load-shared-object ""))
  
  (define c-inotify-init
    (foreign-procedure  "inotify_init" () int))

  (define c-inotify-init1
    (foreign-procedure "inotify_init1" (int) int))
  
  (define c-inotify-add-watch
    (foreign-procedure "inotify_add_watch" (int string unsigned-32) int))

  (define c-inotify-rm-watch
    (foreign-procedure "inotify_rm_watch" (int int) int))

  ;; (define c-errno-pointer (dynamic-pointer "errno" (dynamic-link))) 

  (define-ftype struct-inotify-event
    (struct
  	(wd int)
        (mask unsigned-32)
        (cookie unsigned-32)
  	(len unsigned-32)))

  (define (parse-inotify-event bv)
    "hard coded parser"
    (let lp ([s '(int uint32 uint32 uint32)]
	     [n '(wd mask cookie namelen)]
	     [sz '(4 4 4 4)]
	     [p 0]
	     [r '()])
      (if (eq? s '())
	  r
	  (let* ((cur-s (car s))
		 (cur-n (car n))
		 (cur-sz (car sz))
		 (cv (if (eq? cur-s 'int)
			 bytevector-s32-native-ref
			 bytevector-u32-native-ref))
		 (c-r (cons (cons cur-n (cv bv p))
			    r))
		 (c-p (+ p cur-sz)))
	    (lp (cdr s) (cdr n) (cdr sz) c-p c-r)))))
  
  (define size-struct-inotify-event
    (* 4 4))

  (define init-flags
    '((cloexec #o2000000)
      (nonblock #o4000)))

  (define inotify-flags 
    '((access        #x00000001)  
      (modify        #x00000002)  
      (attrib        #x00000004)  
      (close-write   #x00000008)  
      (close-nowrite #x00000010)  
      (open          #x00000020)  
      (moved-from    #x00000040)  
      (moved-to      #x00000080)  
      (create        #x00000100)  
      (delete        #x00000200)  
      (delete-self   #x00000400)  
      (move-self     #x00000800)  

      (unmount       #x00002000)  
      (q-overflow    #x00004000)  
      (ignored       #x00008000)  

      (onlydir       #x01000000)  
      (dont-follow   #x02000000)  
      (excl-unlink   #x04000000)  
      (mask-add      #x20000000)  
      (isdir         #x40000000)  
      (oneshot       #x80000000)))  

  (define (in-inotify-flags->int l flags-assoc)
    (let lp ((flags l)
	     (calced-flags 0))
      (if (null? flags)
	  calced-flags
	  (lp (cdr flags)
	      (logior calced-flags
		      (cadr 
		       (assoc (car flags)
			      flags-assoc)))))))

  (define strerror
    (foreign-procedure  "strerror" (int) string))
  (define (call-and-check-errno proc procname)
    (call-with-values 
	(lambda ()
	  (with-interrupts-disabled
	   (let ()
	     (values 
	      (proc)
	      (foreign-ref 'int
			   ((foreign-procedure 
			     "__errno_location" () void*)) 0)))))
      (lambda (return-code errno)
	(if (< return-code 0)
	    (error procname (strerror errno) (list 'errno errno))
	    return-code))))

  (define (inotify-flags->int l)
    (in-inotify-flags->int l inotify-flags))



  (define (inotify-init1 flag)
    "- Scheme Procedure: inotify-init1 flag

     init inotify port using flag
     flag: list of 'cloexec, 'nonblock or just '() 

     return a port
     raise 'system-error on failure
"
    (open-fd-input-port
     (call-and-check-errno 
      (lambda ()
	(c-inotify-init1 (in-inotify-flags->int flag init-flags)))
      "inotify-init1")
     (buffer-mode block)))

  (define (inotify-init)
    "- Scheme Procedure: inotify-init1 flag

     init inotify port

     return a port
     raise 'system-error on failure
"
    (inotify-init1 '()))

  (define (inotify-add-watch port file-path flags)
    "- Scheme Procedure: inotify-add-watch port file-path flags

      adds a watch on file-path
      port: port returned by inotify-init or inotify-init1
      file-path: file or directory to watch
      flags: list of watch flag
      valid flags:
                  access       
                  modify       
                  attrib       
                  close-write  
                  close-nowrite
                  open         
                  moved-from   
                  moved-to     
                  create       
                  delete       
                  delete-self  
                  move-self    
                  unmount      
                  q-overflow   
                  ignored      
                  onlydir      
                  dont-follow  
                  excl-unlink  
                  mask-add     
                  isdir        
                  oneshot 

     return: watch descriptor
     raise 'system-error on failure                    
" 
    (let ((fd (port-file-descriptor port))
	  (fnptr file-path)
	  (flag (inotify-flags->int flags)))
      (call-and-check-errno 
       (lambda () (c-inotify-add-watch fd fnptr flag))
       "inotify-add-watch")))

  (define (inotify-rm-watch port wd)
    "- Scheme Procedure: inotify-rm-watch port wd

      remove watch `wd' from inotify port `port'
      port: port returned by inotify-init or inotify-init1
      wd: watch-descriptor returned by inotify-add-watch

      return 0 on success
      raise 'system-error on failure
    "
    (let ((inotify-fd (port-file-descriptor port)))
      (call-and-check-errno (lambda ()
			      (c-inotify-rm-watch inotify-fd wd))
			    "inotify-rm-watch")))


  (define (inotify-int->flags flag)
    (map (lambda (x) (car x))
	 (filter (lambda (x)
		   (logtest (cadr x) flag))
		 inotify-flags)))

  (define (bv->string-cut-zero bv len)
    (bytevector->string (bytevector-truncate! bv (- len 1))
			(make-transcoder (utf-8-codec))))

  (define (read-bv-and-check-length port length)
    (let* ([bv (make-bytevector length)]
	   [nread (get-bytevector-some! port bv 0 length)])
      (if (or (eof-object? nread) (< nread length))
	  (error "inotify-read-port" "not reading enough bytes") 
	  bv)))
  (define (inotify-read-port port)
    "- Scheme Procedure: inotify-read-port port 

      read an event from inotify port `port'
      port: port returned by inotify-init or inotify-init1

     return: ((wd `watch-descriptor') (mask `(list of flags)') (cookie `cookie') (name `file-name'))
   "

    (let* ((bv (read-bv-and-check-length port size-struct-inotify-event))
	   (parsed (parse-inotify-event bv))
	   (wd (cdr  (assoc 'wd parsed)))
	   (mask (cdr  (assoc 'mask parsed )))
	   (cookie (cdr (assoc 'cookie parsed)))
	   (len (cdr (assoc 'namelen parsed)))
	   (name (if (> len 0)
		     (bv->string-cut-zero
		      (read-bv-and-check-length port len) len)
		     "")))
    
      (list 
       (list 'wd wd)
       (list 'mask (inotify-int->flags mask))
       (list 'cookie cookie)
       (list 'name name))))
)
