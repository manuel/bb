;; URI/Location handling

(define +top-path+ (make-path "top"))

(define-prototype URI Object (purl))

(define (make-uri (uri String))
  (new URI ($purl uri)))

(define (uri-param (uri URI) (param String))
  (@param (.purl uri) param))

;; Gets the path the user wants to view or edit from the URI, or the
;; top path if unspecified.
(define (uri-entry-path (uri URI))
  (let ((param (uri-param uri "path")))
    (if (= #undefined param)
        +top-path+
        (apply make-path (array-to-list (decode-transit param))))))

;; Whether the user wants to edit (instead of view) the path.
(define (uri-is-edit (uri URI))
  (let ((param (uri-param uri "op")))
    (if (= #undefined param)
        #f
        (= param "edit"))))

;; View objects

(define (child-handle (child-links Array))
  (if (> (.length child-links) 0)
      "<span class='full'>&#x25b6;</span>"
      "<span class='empty'>&#x25b6;</span>"))

(define (handle (child-links Array))
  (if (> (.length child-links) 0)
      "<span class='full'>&#x25bc;</span>"
      "<span class='empty'>&#x25bc;</span>"))

(define (view-entry (session Session) (entry-path Path))
  (let* ((tx (make-tx (session-space session)))
         (child-links (.entries (get-out-links tx entry-path "child"))))
    (object
     ("handle"
      (handle child-links))
     ("path"
      (encode-transit (->transit (.elements entry-path))))
     ("titles"
      (map-array view-title-link (.entries (get-out-links tx entry-path "title"))))
     ("children"
      (map-array (lambda (link)
                   (view-child-link session link))
                 child-links))
     ("parents"
      (map-array (lambda (link)
                   (view-parent-link session link))
                 (.entries (get-in-links tx entry-path "child")))))))

(define (view-title-link (link Link))
  (object
   ("text" (.href (.dst link)))))

(define (view-child-link (session Session) (link Link))
  (let* ((tx (make-tx (session-space session))))
    (object
     ("path"
      (encode-transit (->transit (.elements (the Path (.href (.dst link)))))))
     ("handle"
      (child-handle (.entries (get-out-links tx (.href (.dst link)) "child"))))
     ("titles"
      (map-array view-title-link (.entries (get-out-links tx (.href (.dst link)) "title")))))))

(define (view-parent-link (session Session) (link Link))
  (let* ((tx (make-tx (session-space session))))
    (object
     ("path"
      (encode-transit (->transit (.elements (the Path (.href (.src link)))))))
     ("handle"
      "&uarr;")
     ("titles"
      (map-array view-title-link (.entries (get-out-links tx (.href (.src link)) "title")))))))
  
(define (edit-entry (session Session) (entry-path Path))
  (let* ((tx (make-tx (session-space session)))
         (title-links (get-out-links tx entry-path "title")))
    (object
     ("title"
      (if (= 0 (.length (.entries title-links)))
          ""
          (.href (.dst (elt (.entries title-links) 0))))))))

(define (set-title (session Session) (entry-path Path) (title String))
  (let* ((tx (make-tx (session-space session)))
         (entry (get-or-create-entry tx entry-path)))
    (set (.links entry) (array-keep (lambda (link)
                                      (not (data-equal "title" (.rel link))9))
                                    (.links entry)))
    (@push (.links entry)
           (make-link "title" (make-end entry-path) (make-end title)))
    (put-entry tx entry-path entry)))

(define (create-new-child-entry (session Session) (parent-path Path))
  (let* ((tx (make-tx (session-space session)))
         (parent (get-or-create-entry tx parent-path))
         (child-path (make-path (+ "item-" (@random $Math)))))
    (@push (.links parent)
           (make-link "child" (make-end parent-path) (make-end child-path)))
    (put-entry tx parent-path parent)
    child-path))

;; Start

(define (view (session Session) (path Path))
  (define r (new $Ractive
                 (object
                  ("el" "output")
                  ("template" "#viewTemplate")
                  ("data" (view-entry session path)))))
  (@on r "edit"
       (js-callback (lambda #ignore
                      (set (.href $location) (+ "?op=edit&path=" (encode-transit (->transit (.elements path))))))))
  (@on r "new"
       (js-callback (lambda #ignore
                      (define child-path (create-new-child-entry session path))
                      (set (.href $location) (+ "?op=edit&path=" (encode-transit (->transit (.elements child-path)))))))))

(define (edit (session Session) (path Path))
  (define r (new $Ractive
                 (object
                  ("el" "output")
                  ("template" "#editTemplate")
                  ("data" (edit-entry session path)))))
  (@on r "save"
       (js-callback (lambda #ignore
                      (set-title session path (@get r "title"))
                      (set (.href $location) (+ "?path=" (encode-transit (->transit (.elements path)))))))))

(define (start-ui)
  (define session (make-trivial-session (make-simple-space (make-local-store))))
  (define location (make-uri (.href $location)))
  (define entry-path (uri-entry-path location))
  (if (uri-is-edit location)
      (edit session entry-path)
      (view session entry-path)))

(start-ui)
