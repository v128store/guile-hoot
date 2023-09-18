;;; Copyright (C) 2023 Igalia, S.L.
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; Port tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-ports")

(test-call "#vu8(100 120)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-u8 100 p)
               (write-u8 120 p)
               (get-output-bytevector p))))

(test-call "#vu8(100 120 130 140)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-bytevector #vu8(100 120) p)
               (write-bytevector #vu8(130 140) p)
               (get-output-bytevector p))))

(test-call "#vu8(104 101 108 108 111 44 32 119 111 114 108 100)"
           (lambda ()
             (let ((p (open-output-bytevector)))
               (write-string "hello, world" p)
               (get-output-bytevector p))))

(test-call "#(1 1 2 3 #<eof> #<eof> #<eof>)"
           (lambda ()
             (let* ((p (open-input-bytevector #vu8(1 2 3)))
                    (a (peek-u8 p))
                    (b (read-u8 p))
                    (c (read-u8 p))
                    (d (read-u8 p))
                    (e (read-u8 p))
                    (f (peek-u8 p))
                    (g (read-u8 p)))
               (vector a b c d e f g))))

(test-call "#(#vu8() #vu8(1) #vu8(1 2) #vu8(1 2 3) #vu8(1 2 3))"
           (lambda ()
             (define (read-n n)
               (read-bytevector n (open-input-bytevector #vu8(1 2 3))))
             (vector (read-n 0)
                     (read-n 1)
                     (read-n 2)
                     (read-n 3)
                     (read-n 4))))

(test-call "#<eof>"
           (lambda ()
             (read-bytevector 1 (open-input-bytevector #vu8()))))

(test-call "#(#\\h #\\h #\\e #\\l #\\l #\\o #<eof> #<eof> #<eof>)"
           (lambda ()
             (let* ((p (open-input-bytevector #vu8(104 101 108 108 111)))
                    (a (peek-char p))
                    (b (read-char p))
                    (c (read-char p))
                    (d (read-char p))
                    (e (read-char p))
                    (f (read-char p))
                    (g (read-char p))
                    (h (peek-char p))
                    (i (read-char p)))
               (vector a b c d e f g h i))))

(test-call "#(\"\" \"h\" \"he\" \"hel\" \"hell\" \"hello\" \"hello\")"
           (lambda ()
             (define (read-n n)
               (read-string n (open-input-bytevector #vu8(104 101 108 108 111))))
             (vector (read-n 0)
                     (read-n 1)
                     (read-n 2)
                     (read-n 3)
                     (read-n 4)
                     (read-n 5)
                     (read-n 6))))

(test-call "#(43 43 70 #(101 101 421) 70)"
           (lambda ()
             (let* ((p (make-parameter 42 1+))
                    (a (p))
                    (b (p 69))
                    (c (p))
                    (d (parameterize ((p 100))
                         (let* ((a (p))
                                (b (p 420))
                                (c (p)))
                           (vector a b c))))
                    (e (p)))
               (vector a b c d e))))

(test-call "#(\"foo\" \"bar\" \"baz\" \"asdfa\" #<eof> #<eof>)"
           (lambda ()
             (let* ((p (open-input-string "foo\nbar\r\nbaz\rasdfa"))
                             (a (read-line p))
                             (b (read-line p))
                             (c (read-line p))
                             (d (read-line p))
                             (e (read-line p))
                             (f (read-line p)))
                        (vector a b c d e f))))

(test-end* "test-ports")
