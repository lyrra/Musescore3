
(for-each (lambda (score)
            (let ((mea (ms-score-firstmeasure score)))
              (assert mea "firstMeasure failed")
              (assert (measure? mea) "firstMeasure returned non-measure object")))
          (ms-scores))
