(use-modules (ice-9 format))
(use-modules (ice-9 match))

(load "format.scm")
(load "expand.scm")
(load "speccomp.scm")

(match (command-line)
  ((_ infile outfile)
   (format #t "compile-specification infile=~s outfile=~s~%" infile outfile)
   (call-with-input-file infile
     (lambda (inproc)
       (call-with-output-file outfile
         (lambda (outproc)
           (compile-specification inproc outproc)))))))

(exit)
