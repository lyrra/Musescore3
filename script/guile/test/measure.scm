(deftest (measure)
  (loop-list score (ms-scores)
    (let ((mea (ms-score-firstmeasure score)))
      (assert mea "firstMeasure failed")
      (assert (ms-measure? mea) "firstMeasure returned non-measure object"))))
