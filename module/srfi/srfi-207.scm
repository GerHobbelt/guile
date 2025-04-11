;;;; SRFI 207: String-notated bytevectors
;;;;
;;;; Copyright (C) 2025 Free Software Foundation, Inc.
;;;;
;;;; This library is free software: you can redistribute it and/or
;;;; modify it under the terms of the GNU Lesser General Public License
;;;; as published by the Free Software Foundation, either version 3 of
;;;; the License, or (at your option) any later version.
;;;;
;;;; This library is distributed in the hope that it will be useful, but
;;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;;; Lesser General Public License for more details.
;;;;
;;;; You should have received a copy of the GNU Lesser General Public
;;;; License along with this program.  If not, see
;;;; <http://www.gnu.org/licenses/>.

;;; Commentary:
;;;
;;; This is an implementation of SRFI 207: String-notated bytevectors.
;;;
;;; Code:

(define-module (srfi srfi-207)
  #:use-module ((rnrs arithmetic bitwise) #:select (bitwise-and bitwise-ior))
  #:use-module ((rnrs bytevectors)
                #:select (bytevector->u8-list string->utf8 u8-list->bytevector))
  #:use-module ((scheme base)
                #:select (binary-port?
                          bytevector
                          bytevector-copy
                          bytevector-copy!
                          bytevector-length
                          bytevector-u8-ref
                          bytevector-u8-set!
                          bytevector?
                          define-record-type
                          eof-object
                          get-output-bytevector
                          let-values
                          make-bytevector
                          open-output-bytevector
                          read-string
                          utf8->string
                          write-bytevector
                          write-string
                          write-u8))
  #:use-module ((srfi srfi-1)
                #:select (fold list-tabulate fold-right unfold unfold-right))
  #:use-module ((srfi srfi-43) #:select (vector-unfold))
  #:use-module ((srfi srfi-60) #:select (arithmetic-shift bit-field))
  #:export (base64->bytevector
            bytestring
            bytestring->list
            bytestring-break
            bytestring-error?
            bytestring-index
            bytestring-index-right
            bytestring-join
            bytestring-pad
            bytestring-pad-right
            bytestring-replace
            bytestring-span
            bytestring-split
            bytestring-trim
            bytestring-trim-both
            bytestring-trim-right
            bytestring<=?
            bytestring<?
            bytestring>=?
            bytestring>?
            bytevector->base64
            bytevector->hex-string
            hex-string->bytevector
            make-bytestring
            make-bytestring!
            make-bytestring-generator
            read-textual-bytestring
            write-binary-bytestring
            write-textual-bytestring))

(cond-expand-provide (current-module) '(srfi-207))

;; This awkwardness is because read.scm (not a module, included via
;; boot-9) also needs to be able to read bytestrings.
(define &bytestring-error
  (hashq-ref %boot-9-shared-internal-state '&bytestring-error))
(define bytestring-error
  (hashq-ref %boot-9-shared-internal-state 'bytestring-error))
(define read-bytestring-content
  (hashq-ref %boot-9-shared-internal-state 'read-bytestring-content))

(define bytestring-error? (exception-predicate &bytestring-error))

;; From the upstream 207.sld library definition
(define-syntax assume
  (syntax-rules ()
    ((_ pred) (unless pred (error "invalid assumption:" (quote pred))))
    ((_ pred msg ...) (unless pred (error msg ...)))))

(include-from-path "ice-9/read/bytestring.scm")
(include-from-path "srfi/srfi-207/upstream/base64.scm")
(include-from-path "srfi/srfi-207/upstream/bytestrings-impl.scm")

(define (make-bytestring! bvec at parts)
  (let lp ((parts parts)
           (i at))
    (unless (null? parts)
      (let ((x (car parts)))
        (cond
         ((and (exact-integer? x) (<= 0 x 255))
          (bytevector-u8-set! bvec i x)
          (lp (cdr parts) (1+ i)))
         ((and (char? x) (char<=? x #\delete))
          (bytevector-u8-set! bvec i (char->integer x))
          (lp (cdr parts) (1+ i)))
         ((bytevector? x)
          (bytevector-copy! bvec i x 0 (bytevector-length x))
          (lp (cdr parts) (+ i (bytevector-length x))))
         ((string? x)
          (let ((n (string-length x))
                (utf8 (string->utf8 x)))
            (unless (= n (bytevector-length utf8))
              (bytestring-error "bytestring string part is not ASCII" x))
            (bytevector-copy! bvec i utf8 0 n)
            (lp (cdr parts) (+ i (string-length x)))))
         (else
          (bytestring-error "invalid bytestring string part" x)))))))

(define (make-bytestring parts)
  (define (byte-len x)
    (cond
     ((and (integer? x) (<= 0 x 255)) 1)
     ((and (char? x) (char<=? #\delete)) 1)
     ((bytevector? x) (bytevector-length x))
     ((and (string? x) (string-every char-set:ascii x)) (string-length x))
     (else (bytestring-error "invalid bytestring argument" x))))
  (let* ((n (fold (λ (part total) (+ total (byte-len part)))
                  0 parts))
         (result (make-bytevector n)))
    (make-bytestring! result 0 parts)
    result))

(define read-textual-bytestring
  (case-lambda
   ((prefix) (read-textual-bytestring prefix (current-input-port)))
   ((prefix in)
    (unless (boolean? prefix)
      (scm-error 'wrong-type-arg "read-textual-bytestring"
                 "Non-boolean prefix argument: ~s" (list prefix) (list prefix)))
    (when prefix
      (let ((s (read-string 3 in)))
        (cond ((eof-object? s)
               (bytestring-error "end of input within bytestring content"))
              ((string=? s "#u8") #t)
              (else (bytestring-error "invalid bytestring prefix" s)))))
    (read-bytestring-content in))))
