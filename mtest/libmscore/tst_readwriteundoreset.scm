;;;; ms-score-cmdResetAllPositions
;;;;   c-function defined in libmscore/cmd.cpp:
;;;;   void Score::cmdResetAllPositions(bool undoable);
;;;;   scheme-function-wrapper is defined in mtest/ms-objects.scm:
;;;;   (define-object score ...)
;;;;
;;;; ms-score-undoRedo ...
;;;;  in file libmscore/cmd.cpp:
;;;;  void Score::undoRedo(bool undo, EditData* ed)
;;;;
;;;; since Score::undoRedo takes a structure of type EditData*,
;;;; we also need to define it in ms-objects.scm
;;;;
;;;; in file libmscore/style.h the style-id enum is defined,
;;;;   example Sid::createMultiMeasureRests
;;;; corresponding enum are listed in mtest/ms.scm in %style-id 
;;;; (in this example Sid-Sid-createMultiMeasureRests)

(load "./mtest.scm")

; testReadWriteResetPositions
(let ((DIR "libmscore/readwriteundoreset/"))
  (for-each (lambda (file)
    (let* ((read-filename (string-append DIR file ".mscx"))
           (write-filename (string-append file "-undoreset-test.mscx"))
           (score (ms-mtest-readScore read-filename)))
      (ms-score-cmdResetAllPositions score)
      (ms-score-undoRedo score #t (ms-make-nullgoo)) ; args: true, nullptr
      (ms-test-check
       (ms-mtest-saveCompareScore score write-filename read-filename))
      ; delete score
      ))
    '("barlines" "slurs" "mmrestBarlineTextLinks")))

;;---------------------------------------------------------
;;   testMMRestLinksRecreateMMRest
;;   For barlines links with MM rests a separate test is
;;   needed: in this test score, if creating MM rests from
;;   scratch, <linked> tags in BarLines may have appeared
;;   before <linkedMain> tags, so they were not able to
;;   link and prevented text elements from linking as well.
;;---------------------------------------------------------

; testMMRestLinksRecreateMMRest
(let* ((DIR "libmscore/readwriteundoreset/")
       (file "mmrestBarlineTextLinks")
       (readFile (string-append DIR file ".mscx"))
       (writeFile(string-append file "-recreate-mmrest-test.mscx"))
       (disableMMRestRefFile (string-append DIR file "-disable-mmrest-ref.mscx"))
       (recreateMMRestRefFile (string-append DIR file "-recreate-mmrest-ref.mscx"))
       (score (ms-mtest-readScore readFile)))

      ;; Regenerate MM rests from scratch:
      ;; 1) turn MM rests off
      (ms-score-startCmd score)
      (ms-score-undo score (ms-make-ChangeStyleVal score 'Sid-Sid-createMultiMeasureRests #f))
      (ms-score-endCmd score)

      ;; 2) save/close/reopen the score
      (ms-test-check
       (ms-mtest-saveCompareScore score writeFile disableMMRestRefFile))
      ; delete score;
      (set! score (ms-mtest-readCreatedScore writeFile))

      ;; 3) turn MM rests back on
      (ms-score-startCmd score)
      (ms-score-undo score (ms-make-ChangeStyleVal score 'Sid-Sid-createMultiMeasureRests #t))
      (ms-score-endCmd score)

      (ms-test-check
       (ms-mtest-saveCompareScore score writeFile recreateMMRestRefFile))

      ;delete score;
      )
