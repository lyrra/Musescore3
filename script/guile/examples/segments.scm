(map (lambda (score)
       (map (lambda (measure)
              (ms-measure-segments measure))
            (ms-score-measures score)))
     (ms-scores))
