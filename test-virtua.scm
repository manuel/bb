;; Tests

(define-operative (assert-true expr) env
  (unless (= #t (eval expr env))
    (error (+ "Should be true: " expr))))

(define-operative (assert-false expr) env
  (unless (= #f (eval expr env))
     (error (+ "Should be false: " expr))))

(define-operative (assert-equal expected expr2) env
  (let ((res (eval expr2 env))
        (exp (eval expected env)))
    (unless (= exp res)
      (error (+ expr2 " should be " exp " but is " res)))))

(define-operative (assert-data-equal expected expr2) env
  (let ((res (eval expr2 env))
        (exp (eval expected env)))
    (unless (data-equal exp res)
      (error (+ expr2 " should be " exp " but is " res)))))

(define-operative (assert-throws expr) env
  (label return
    (catch (eval expr env)
      (lambda (exc) (return)))
    (error (+ "Should throw: " expr))))

(define-macro (define-test name . body)
  (list* define-module name () body))

(define-test test-path-elements-must-be-strings
  (assert-throws (make-path 12))
  (assert-throws (make-path "->" "bar" 12 "quux"))
  )

(define-test test-local-store
  (define store (the Store (make-local-store)))
  (define tx (the LocalTx (make-tx store)))
  (define path (the Path (make-path "hello.txt")))
  (define content "Hello world!")
  (assert-equal #null (get-file tx path))
  (put-file tx path content)
  (assert-equal content (get-file tx path))
  (delete-file tx path)
  (assert-equal #null (get-file tx path))
  ;; only one-element paths supported
  (assert-throws (get-file tx (make-path "x" "y")))
  (assert-throws (put-file tx (make-path "a" "b") "test"))
  (commit tx)
  )

(define-test test-transit-encoding
  (assert-equal "\"foo\"" (encode-transit (->transit "foo")))
  (assert-equal "[\"foo\",\"bar\"]" (encode-transit (->transit (array "foo" "bar"))))
  (assert-equal "{\"~#path\":[\"aa\",\"bb\"]}" (encode-transit (->transit (make-path "aa" "bb"))))
  )

(define-test test-transit-data
  (define (roundtrip-data data)
    (define transit (->transit data))
    (define encoded-transit (encode-transit transit))
    (define decoded-transit (decode-transit encoded-transit))
    (define decoded-data (transit->data decoded-transit))
    (assert-equal encoded-transit (encode-transit decoded-transit))
    (assert-equal encoded-transit (encode-transit (->transit decoded-data))))
  (roundtrip-data "foo")
  (roundtrip-data (array "1" "2"))
  (roundtrip-data (array "1" "2" (array "z")))
  (roundtrip-data (make-path "a" "b"))
  (roundtrip-data (array "1" "2" (make-path "hello.txt") (array "z")))
  )

(define-test test-transit-entry
  (define (roundtrip-entry entry)
    (define transit (->transit entry))
    (define encoded-transit (encode-transit transit))
    (define decoded-transit (decode-transit encoded-transit))
    (define decoded-entry (the Entry (transit->entry decoded-transit)))
    (assert-equal encoded-transit (encode-transit decoded-transit))
    (assert-equal encoded-transit (encode-transit (->transit decoded-entry))))
  (roundtrip-entry (make-entry (array)))
  (roundtrip-entry (make-entry (array (make-link "rel" (make-end "src") (make-end "dst")))))
  (roundtrip-entry (make-entry (array (make-link (array "rel" "rev") (make-end "src") (make-end "dst")))))
  (roundtrip-entry (make-entry (array (make-link "rel" (make-end (make-path "src")) (make-end "dst")))))
  )

(define-test test-simple-space
  (define space-tx (make-tx (make-simple-space (make-local-store))))
  (assert-equal #null (get-entry space-tx (make-path "hello")))
  (define entry (make-entry (array (make-link "1" (make-end "a") (make-end "b")))))
  (put-entry space-tx (make-path "hello") entry)
  (define read-entry (get-entry space-tx (make-path "hello")))
  (assert-equal (entry->string entry) (entry->string read-entry))
  (delete-entry space-tx (make-path "hello"))
  )

(define-test test-uri-entry
  (assert-data-equal +top-path+
                     (uri-entry-path (make-uri "http://example.com")))
  (assert-data-equal (make-path "a" "b")
                     (uri-entry-path (make-uri "http://example.com?path=[\"a\",\"b\"]")))
  )

(define-test test-uri-edit
  (assert-true (uri-is-edit (make-uri "http://example.com?op=edit")))
  (assert-false (uri-is-edit (make-uri "http://example.com?path=[\"a\",\"b\"]")))
  )

(define-test data-equality
  (assert-true (data-equal "a" "a"))
  (assert-false (data-equal "a" "b"))
  (assert-true (data-equal (array "a" "b") (array "a" "b")))
  (assert-false (data-equal (array "a" "a") (array "a" "b")))
  )

(define-test test-get-out-links
  (define path (make-path "test"))
  (define tx (make-tx (make-simple-space (make-local-store))))
  (define link-1 (make-link "title" (make-end path) (make-end "Title 1")))
  (define link-2 (make-link "title" (make-end path) (make-end "Title 2")))
  (define link-3 (make-link "another rel" (make-end path) (make-end "whatevs")))
  (put-entry tx path (make-entry (array link-1 link-2 link-3)))
  (define titles-feed (get-out-links tx path "title"))
  (assert-data-equal (.entries titles-feed) (array link-1 link-2))
  (define another-feed (get-out-links tx path "another rel"))
  (assert-data-equal (.entries another-feed) (array link-3))
  (delete-entry tx path)
  )

(define-test test-get-in-links
  (define path (make-path "test"))
  (define tx (make-tx (make-simple-space (make-local-store))))
  (define link-1 (make-link "title" (make-end path) (make-end (make-path "dst"))))
  (define link-2 (make-link "title" (make-end path) (make-end (make-path "dst"))))
  (define link-3 (make-link "another rel" (make-end path) (make-end "whatevs")))
  (put-entry tx path (make-entry (array link-1 link-2 link-3)))
  (define titles-feed (get-in-links tx (make-path "dst") "title"))
  (assert-data-equal (.entries titles-feed) (array link-1 link-2))
  (delete-entry tx path)
  )
