(defun isDivisibleByThree (input) (= 0 (mod input 3)))
(defun isDivisibleByFive (input) (= 0 (mod input 5)))

(defun range (start end) (let ((i start)
                               (rangeList '()))
                           (loop while (<= i end)
                                 do (progn
                                     (setq rangeList (append rangeList (list i)))

                                     (setq i (+ 1 i))))
                           rangeList))


(defun fizzBuzzOnce (input) (let ((isDivByThree (isDivisibleByThree input))
                                  (isDivByFive (isDivisibleByFive input)))
                              ;(format t "~a" isDivByFive)
                              (if (and isDivByThree isDivByFive) (format t "FizzBuzz"))
                              (if (and isDivByThree (not isDivByFive)) (format t "Fizz"))
                              (if (and isDivByFive (not isDivByThree)) (format t "Buzz"))))

(defun fizzBuzz () (loop for x in (range 1 100)
                         do (fizzBuzzOnce x)))

(fizzBuzz)
