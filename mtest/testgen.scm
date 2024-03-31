
(define %sourcedir #f)
(define %builddir #f)
(define %libdir #f)
(define %libdir-mtest #f)
(define %output-file #f)
(define %source-files '())
(define %generate-headers '(
  "../libmscore/types-gen.h.scm"
  "../libmscore/tremolo-gen.h.scm"
  "../libmscore/property-gen.h.scm"
  "../libmscore/style-gen.h.scm"
  ))

(define (load-source-file filename)
  (let ((file (format #f "~a/~a" %sourcedir filename)))
    (format #t "-- loading source file ~s~%" file)
    (psyntax-load file)))

(let ((sarg #f))
  (do ((pair %command-line (cdr pair)))
      ((eq? '() pair))
    (let ((arg (car pair)))
      (cond
       ((string=? "-l" arg)
        (set! pair (cdr pair))
        (set! %libdir (car pair)))
       ((string=? "-s" arg)
        (set! pair (cdr pair))
        (set! %sourcedir (car pair)))
       ((string=? "-b" arg)
        (set! pair (cdr pair))
        (set! %builddir (car pair)))
       ((string=? "-o" arg)
        (set! pair (cdr pair))
        (set! %output-file (car pair)))
       ((string=? "--" arg)
        (set! sarg #t))
       (sarg
        (set! %source-files (cons arg %source-files)))))))
(set! %source-files (reverse %source-files))
(set! %libdir-mtest (format #f "~a/../mtest" %libdir))

(format #t "---- generate scheme test code ----~%")
(format #t "-- libdir: ~s~%" %libdir)
(format #t "-- sourcedir: ~s~%" %sourcedir)
(format #t "-- source-files: ~s~%" %source-files)
(format #t "-- output-file: ~s~%" %output-file)

(if (not %libdir) (error "S7 LIBDIR not found"))

(format #t "-- spicing up the S7 scheme interpreter~%")
(load (format #f "~a/psyntax-s7.scm" %libdir))
(load (format #f "~a/psyntax-expanded.ss" %libdir))

(define (psyntax-eval form)
  (let ((ex (sc-expand form)))
    (eval ex (rootlet))))

(define (psyntax-load file)
  (with-input-from-file file
    (lambda () 
      (do ((c (read) (read))) 
          ((eof-object? c)) 
        (psyntax-eval c)))))

; scheme extensions
(psyntax-load (format #f "~a/plib.scm" %libdir))

(define %emit-port #f)

(define (emit-body form)
  (cond
   ((proper-list? form)
    (write-string "(" %emit-port)
    (do ((pair form (cdr pair)))
        ((eq? '() pair))
      (let ((item (car pair)))
        (emit-body item)
        (write-string " " %emit-port)))
    (write-string ")" %emit-port))
   ((string? form)
     (write-string "\"" %emit-port)
     (write-string form %emit-port)
     (write-string "\"" %emit-port))
   (else
    (write form %emit-port))))

(format #t "-- loading source files~%")
(do ((pair %source-files (cdr pair)))
    ((null? pair))
    (let ((file (format #f "~a/~a" %sourcedir (car pair))))
      (format #t "-- loading source file ~s~%" file)
      (psyntax-load file)))

(load-source-file "../s7/lib.scm")
(load-source-file "comp.scm")
(load-source-file "gen.scm")
(load-source-file "types.scm")
(load-source-file "ms-objects.scm")
(load-source-file "ms.scm")

(do ((pair %generate-headers (cdr pair)))
    ((null? pair))
    (let ((file (format #f "~a/~a" %sourcedir (car pair))))
      (format #t "-- loading generate headers source file ~s~%" file)
      (psyntax-load file)))

(load-source-file "decl.scm")

; generate runtime S7/Guile scheme code
(load-source-file "types-code.scm")
(write-types-code-file "types-code-gen.scm")

(format #t "---- scheme generate code done ----~%")
