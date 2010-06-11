;; test-types.lisp
;;
;; Copyrigth (C) 2010 David Vázquez
;;
;; This file is part of cl-icalendar.
;;
;; cl-icalendar is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; cl-icalendar is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with cl-icalendar.  If not, see <http://www.gnu.org/licenses/>.

(in-package :cl-icalendar-tests)

(in-suite icalendar-types)

;;; Boolean data type

(test booleanp-001
  "Check membership to boolean data type."
  (is (booleanp t))
  (is (booleanp nil))
  (is (not (booleanp 3)))
  (is (not (booleanp "true"))))

(test parse-value-boolean-001
  "Parse some boolean values."
  (is (parse-value "true" 'boolean))
  (is (parse-value "TRUE" 'boolean))
  (is (parse-value "tRUE" 'boolean))
  (is (not (parse-value "false" 'boolean)))
  (is (not (parse-value "FALSE" 'boolean)))
  (is (not (parse-value "fAlse" 'boolean))))

(test parse-value-boolean-002
  "Parse some non-boolean values."
  (signals error (parse-value "23" 'boolean))
  (signals error (parse-value "t" 'boolean))
  (signals error (parse-value "true2" 'boolean))
  (signals error (parse-value "falses" 'boolean)))

(test format-value-boolean-001
  "Format some boolean values."
  (is (string= (format-value t)   "TRUE"))
  (is (string= (format-value nil) "FALSE")))


;;; Integer data type

(test parse-value-integer-001
  "Parse some integer values."
  (is (= 23   (parse-value "23" 'integer)))
  (is (= -123 (parse-value "-123" 'integer)))
  (is (= 0    (parse-value "0" 'integer))))

(test parse-value-integer-002
  "Parse some non-integer values."
  (signals error (parse-value "23.1" 'integer))
  (signals error (parse-value "2x" 'integer))
  (signals error (parse-value "++3" 'integer))
  (signals error (parse-value "--2" 'integer)))

(test format-value-integer-001
  "Format some integer values."
  (is (string= (format-value 2)  "2"))
  (is (string= (format-value 3)  "3"))
  (is (string= (format-value -3) "-3"))
  (is (string= (format-value 0)  "0")))


;;; Float data type

(test parse-value-float-001
  "Parse some float values."
  (is (= 23     (parse-value "23" 'float)))
  (is (= 3.1415 (parse-value "3.1415" 'float)))
  (is (= -0.001 (parse-value "-0.001" 'float))))

(test parse-value-float-002
  "Parse some non-integer values."
  (signals error (parse-value "23." 'float))
  (signals error (parse-value ".1"  'float))
  (signals error (parse-value "-.1" 'float)))

(test format-value-float-001
  "Format some integer values."
  (is (string= (format-value 2.0) "2.0"))
  (is (string= (format-value 3.1) "3.1"))
  (is (string= (format-value -3)  "-3")))

;;; Binary data type

(test read-binary-from-stream-001
  "Read somes binary from a stream."
  (finishes (read-binary-from-file "tests/test-types.001")))

(test parse-value-binary-001
  "Parse binary values."
  (is (= 446 (binary-length (parse-value "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=" 'binary)))))

(test format-value-binary-001
  "Format binary values."
  (is (string= (format-value (read-binary-from-file "tests/test-types.001"))
               "TG9yZW0gaXBzdW0gZG9sb3Igc2l0IGFtZXQsIGNvbnNlY3RldHVyIGFkaXBpc2ljaW5nIGVsaXQsIHNlZCBkbyBlaXVzbW9kIHRlbXBvciBpbmNpZGlkdW50IHV0IGxhYm9yZSBldCBkb2xvcmUgbWFnbmEgYWxpcXVhLiBVdCBlbmltIGFkIG1pbmltIHZlbmlhbSwgcXVpcyBub3N0cnVkIGV4ZXJjaXRhdGlvbiB1bGxhbWNvIGxhYm9yaXMgbmlzaSB1dCBhbGlxdWlwIGV4IGVhIGNvbW1vZG8gY29uc2VxdWF0LiBEdWlzIGF1dGUgaXJ1cmUgZG9sb3IgaW4gcmVwcmVoZW5kZXJpdCBpbiB2b2x1cHRhdGUgdmVsaXQgZXNzZSBjaWxsdW0gZG9sb3JlIGV1IGZ1Z2lhdCBudWxsYSBwYXJpYXR1ci4gRXhjZXB0ZXVyIHNpbnQgb2NjYWVjYXQgY3VwaWRhdGF0IG5vbiBwcm9pZGVudCwgc3VudCBpbiBjdWxwYSBxdWkgb2ZmaWNpYSBkZXNlcnVudCBtb2xsaXQgYW5pbSBpZCBlc3QgbGFib3J1bS4=")))

;;; Period type

(test parse-value-period-001
  (is (periodp (parse-value "19970101T180000Z/PT5H30M" 'period))))

(test parse-value-period-002
  (is (periodp (parse-value "19970101T180000Z/19970102T070000Z" 'period))))


;;; test-types.lisp ends here
