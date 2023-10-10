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
;;; Bytevector tests.
;;;
;;; Code:

(use-modules (srfi srfi-64)
             (test utils))

(test-begin "test-bytevectors")

(test-call "10" (lambda (bv) (bytevector-length bv))
           #vu8(0 1 2 3 4 5 6 7 8 9))

(test-call "8" (lambda (bv) (bytevector-u8-ref bv 8))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "8" (lambda (bv) (bytevector-s8-ref bv 8))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "-9" (lambda (bv) (bytevector-s8-ref bv 9))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))
(test-call "247" (lambda (bv) (bytevector-u8-ref bv 9))
           #vu8(0 #xff 2 #xfd 4 #xfb 6 #xf9 8 #xf7))

(test-call "65280" (lambda (bv) (bytevector-u16-native-ref bv 0))
           #vu8(#x00 #xff #xff #x00))
(test-call "65535" (lambda (bv) (bytevector-u16-native-ref bv 1))
           #vu8(#x00 #xff #xff #x00))
(test-call "255" (lambda (bv) (bytevector-u16-native-ref bv 2))
           #vu8(#x00 #xff #xff #x00))
(test-call "-256" (lambda (bv) (bytevector-s16-native-ref bv 0))
           #vu8(#x00 #xff #xff #x00))
(test-call "-1" (lambda (bv) (bytevector-s16-native-ref bv 1))
           #vu8(#x00 #xff #xff #x00))
(test-call "255" (lambda (bv) (bytevector-s16-native-ref bv 2))
           #vu8(#x00 #xff #xff #x00))

(test-call "50463231" (lambda (bv) (bytevector-u32-native-ref bv 0))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "67305985" (lambda (bv) (bytevector-u32-native-ref bv 1))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "4278452994" (lambda (bv) (bytevector-u32-native-ref bv 2))
;;            #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "50463231" (lambda (bv) (bytevector-s32-native-ref bv 0))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "67305985" (lambda (bv) (bytevector-s32-native-ref bv 1))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))
(test-call "-16514302" (lambda (bv) (bytevector-s32-native-ref bv 2))
           #vu8(#xff #x01 #x02 #x03 #x04 #xff))

(test-call "511" (lambda (bv) (bytevector-u64-native-ref bv 0))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "1" (lambda (bv) (bytevector-u64-native-ref bv 1))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "72057594037927936"
;;            (lambda (bv) (bytevector-u64-native-ref bv 2))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; (test-call "18374967954648334336"
;;            (lambda (bv) (bytevector-u64-native-ref bv 3))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "511" (lambda (bv) (bytevector-s64-native-ref bv 0))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "1" (lambda (bv) (bytevector-s64-native-ref bv 1))
           #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; FIXME: Enable once we have bignums.
;; (test-call "72057594037927936"
;;            (lambda (bv) (bytevector-s64-native-ref bv 2))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
;; (test-call "-71776119061217280"
;;            (lambda (bv) (bytevector-s64-native-ref bv 3))
;;            #vu8(#xff 1 0 0 0 0 0 0 0 1 #xff))
(test-call "-65025" (lambda (bv) (bytevector-s64-native-ref bv 0))
           #vu8(#xff 1 #xff #xff #xff #xff #xff #xff))

(test-call "42.69" (lambda (bv)
                     (bytevector-ieee-double-native-ref bv 0))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "42.689998626708984"
           (lambda (bv)
             (bytevector-ieee-single-native-ref bv 0))
           #vu8(143 194 42 66))

(test-call "85.38"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (+ f64 f64)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "43.69"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (+ f64 1.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "41.69"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (- f64 1.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "64.035"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (* f64 1.5)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "21.345"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (/ f64 2.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "-57.31"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (- f64 100.0)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "57.31"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (abs (- f64 100.0))))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "6.5337584895678535"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (sqrt (abs f64))))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "42.0"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (floor f64)))
           #vu8(184 30 133 235 81 88 69 64))
(test-call "43.0"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (ceiling f64)))
           #vu8(184 30 133 235 81 88 69 64))

(test-call "(-0.9614691168217643 0.2749129633138033 -3.497358237429792)"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (list (sin f64)
                     (cos f64)
                     (tan f64))))
           #vu8(184 30 133 235 81 88 69 64))

;; Not testing fasin, facos for now because apparently Guile doesn't emit those!

(test-call "(1.5473759202633208 0.7853981633974483)"
           (lambda (bv)
             (let ((f64 (bytevector-ieee-double-native-ref bv 0)))
               (list (atan f64)
                     (atan f64 f64))))
           #vu8(184 30 133 235 81 88 69 64))

(test-call "#vu8(0 0 0 0 0)"
           (lambda () (make-bytevector 5)))
(test-call "#vu8(42 42 42 42 42)"
           (lambda () (make-bytevector 5 42)))
(test-call "#vu8(1 2 3 4)"
           (lambda () (bytevector 1 2 3 4)))

(test-call "#t" (lambda (a b) (equal? a b)) #vu8() #vu8())
(test-call "#t" (lambda (a b) (equal? a b)) #vu8(1 2) #vu8(1 2))
(test-call "#f" (lambda (a b) (equal? a b)) #vu8() #vu8(1))
(test-call "#f" (lambda (a b) (equal? a b)) #vu8(1 2) #vu8(2 1))
(test-call "#f" (lambda (a b) (equal? a b)) #vu8(1 2 1) #vu8(1 2 3))

(test-end* "test-bytevectors")
