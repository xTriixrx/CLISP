;; The prime factors of 13195 are 5, 7, 13 and 29.
;; What is the largest prime factor of the number 600851475143 ?

(defun largest-prime-factor (n)
  (let ((max -1))
    ;; Reduce even N as divisible by 2
    (do () ((not (eql (mod n 2) 0)))
      (setf max 2)
      (setf n (/ n 2)))

    ;; N must be odd at this point, skip even nums and iterate only for odd ints
    (do ((i 3 (+ 2 i))) ((or (eql i (isqrt n)) (> i (isqrt n))))
      (do () ((not (eql (mod n i) 0)))
        (setf max i)
        (setf n (/ n i))))

    ;; Handles case when n is a prime number greater than 2.
    (if (> n 2) (setf max n)) max))
