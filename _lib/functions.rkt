#lang racket

(require compatibility/defmacro)
(require odysseus)
(require tabtree/tabtree1)
(require tabtree/utils1)
(require tabtree/template-functions)
(require odysseus/api/vk)
(require "globals.rkt")

(require (for-syntax odysseus))

(provide (all-defined-out))

(define-namespace-anchor a)
(define trigger-expression-ns (namespace-anchor->namespace a))

(define Aliases (make-parameter #f))
(define Text (make-parameter #f))
(define Name-id-hash (make-parameter (hash)))
(define Debug (make-parameter #f))

(define (_cache filename)
  (format "/var/cache/projects/taganoskop/~a" filename))

(define-macro (** . xs)
  `(let* ((expanded-arguments (map
                                (λ (x)
                                  (cond
                                    ((trigger-expression? x) x)
                                    (else
                                      (get-trigger-words x #:aliases (Aliases) #:fallback-value #t))))
                                ',xs)))
      (andmap (λ (y) (cond
                        ((boolean? y) y)
                        ((trigger-expression? y) (eval y))
                        (else (match-words? (Text) y))))
              expanded-arguments)))

(define-macro (++ . xs)
  `(let* ((expanded-arguments (map
                                (λ (x)
                                  (cond
                                    ((trigger-expression? x) x)
                                    (else
                                      (get-trigger-words x #:aliases (Aliases) #:fallback-value #f))))
                                ',xs)))
    (ormap (λ (y) (cond
                      ((boolean? y) y)
                      ((trigger-expression? y) (eval y))
                      (else (match-words? (Text) y))))
            expanded-arguments)))

(define-macro (-- x)
  `(let* ((y (cond
                ((trigger-expression? ',x) ',x)
                (else
                  (get-trigger-words ',x #:aliases (Aliases) #:fallback-value #f)))))
      (not
        (cond
          ((boolean? y) y)
          ((trigger-expression? y) (eval y))
          (else (match-words? (Text) y))))))

(define (user? entity)
  (and
    ($ vk entity)
    (or ($ f entity) (equal? (get-entity-type entity) "user"))))

(define (group? entity)
  (and
    ($ vk entity)
    (or ($ u entity) ($ p entity))))
;
(define (members-number entity)
  (->number (or ($ u entity) ($ p entity) ($ f entity) 0)))

(define (get-name* item)
  (correct-geography-names (get-name item)))

(define-catch (get-entity-type entity)
  ($* type entity))

(define-catch (hashtags->a text)
  (regexp-replace*
    #rx"#([A-Za-zА-Яа-яЁё0-9_@!%$*=+&~`^]+)"
    text
    (λ (text hashtag) (format "<a href=\"https://vk.com/feed?section=search&q=%23~a\">#~a</a>" hashtag hashtag))))

(define-catch (text-href->a text #:style (style #f) #:max-length (max-length 50))
  (regexp-replace*
    #rx"(https?://)(www\\.)?([^/ <?]+)([^ <]+)?"
    text
    (λ (text protocol www domain path)
      (let* ((url (format "~a~a~a~a" protocol (or www "") domain (or path "")))
            (shortened_path (let* ((directories (string-split (or path "") "/"))
                                  (path_n (length directories)))
                                  (cond
                                    ((> path_n 0)
                                        (for/fold
                                          ((res ""))
                                          ((directory directories ))
                                          #:break (> (+ (string-length res) (string-length directory)) max-length)
                                          (str res "/" directory)))
                                    (else ""))))
            (url-text (format "~a~a" domain shortened_path))
            ; remove URL parameters:
            (url-text (first (string-split url-text "?")))
            (url-text (string-trim url-text "/")))
        (format "<a href=\"~a\"~a>~a</a>"
                  url
                  (if style
                      (format " style=\"~a\"" style)
                      "")
                  url-text)))))

(define (trigger-expression? expr)
  (cond
    ((not expr) #f)
    ((empty? expr) #f)
    ((list? expr)
      (or
        ; (equal? (car expr) 'identity)
        (equal? (car expr) '**)
        (equal? (car expr) '--)
        (equal? (car expr) '++)))
    ((string? expr)
      (re-matches? "\\((\\*{2}|\\+{2})" expr))
    (else #f)))

(define-catch (match-trigger-expression? text expr #:aliases (aliases #f))
  (parameterize ((Aliases aliases)
                (Text text))
    (eval
      (cond
        ((string? expr)
          (read (open-input-string expr)))
        ((list? expr)
          expr)
        (else
          expr))
      trigger-expression-ns)))

(define (tag? tag item)
  (and item ($ _t item) (re-matches? tag ($ _t item))))

(define-catch (ignore? item)
  (tag? "i" item))

(define-catch (vk? item)
  ($ vk item))

(define-catch (get-entity-parameter entity-id parameter-name entities)
  (let* ((entity (and entities (@id entity-id entities))))
    (and entity (hash-ref* entity parameter-name #f))))

(define (get-vk-image p)
  (let* ((img-urls ($ img-urls p))
        (video-img-urls ($ video-img-urls p))
        (img-url (or ($ 3x img-urls) ($ 3x-link img-urls) ($ 3x video-img-urls) ($ 3x_first_frame video-img-urls) ($ doc img-urls))))
    img-url))

(define (get-entities
            tree-file-path
            #:derived (derived (λ (x) (or
                                        (re-matches? "tr-[\\-a-z0-9]*" (->string x))
                                        (re-matches? "\\+[\\-a-z0-9]+" (->string x))))))
  (filter
    (λ (x) (and (vk? x) (not (ignore? x))))
    (get-leaves
      #:exclude '(i ref alt-id syn illegal keywords)
      #:derived-attrs derived
      (parse-tab-tree tree-file-path))))

(define-catch (cache-posts-by-uid
            #:uids uids
            #:cache-to h-uid-posts
            #:closed-to uid-closed-wall
            #:read-depth read-depth
            #:cache-write-frequency cache-write-frequency)
    (for/fold
      ((res_chunk (hash)) (buffer_count 0))
      ((uid uids))
      (let* ((posts (get-last-posts
                        #:user-id uid
                        #:limit read-depth
                        #:success-display "+"
                        #:on-closed-wall (λ (uid) (uid-closed-wall uid 'append))
                        ; #:on-error (λ (err uid) (uid-closed-wall uid 'append))
                        )))
        (cond
          (posts
            (let ((result (hash-union res_chunk (hash uid posts)))
                  (buffer_count (+ 1 buffer_count)))
              (cond
                ((>= buffer_count cache-write-frequency)
                  (h-uid-posts result 'append)
                  (values (hash) 0))
                (else
                  (values result buffer_count)))))
          (else
            (values res_chunk buffer_count)))))
    #t)

(define-catch (cache-posts
            #:source sources
            #:target target-persistence
            #:read-depth (read-depth #f)
            #:ignore-with-status (ignore-with-status #t)
            #:ignore-sleepy (ignore-sleepy #t)
            )
  (persistent id-sleepy)
  (define (single-file-cache-posts
            #:source source-path
            #:ignore-with-status (ignore-with-status #f)
            #:read-depth (read-depth #f))
    (let* (
          (source-name (last (string-split source-path "/")))
          (entities (get-entities source-path))
          (entities (or entities empty))
          (entitites (if ignore-with-status
                        (filter-not (λ (entity) ($* s entity)) entities)
                        entities))

          (_ (--- (format "\nОбновляем кэш. Сканируем ~a ~a из списка ~a:"
                            (length entities)
                            (if (group? (car entities)) "групп" "аккаунтов")
                            source-name)))
          (posts (for/fold
                  ((res empty))
                  ((entity entities))
                  (cond
                    ((and ignore-sleepy (indexof? (id-sleepy) (extract-pure-alias ($ vk entity))))
                      (display ".")
                      (flush-output)
                      res)
                    (else
                      (sleep 0.1)
                      (pushr res (map
                                    (λ (vk-url)
                                      (cond
                                        ((user? entity)
                                          (get-last-posts #:user vk-url #:limit (or read-depth 15) #:entity entity #:success-display "+" #:delay 0.1))
                                        (else
                                          (get-last-posts #:group vk-url #:limit (or read-depth 15) #:entity entity #:success-display "+" #:delay 0.1))))
                                    (string-split ($ vk entity) ",")))))))
          (posts (cleanmap (flatten posts))))
      posts))
  (define (get-list-of-sources sources)
    (cond
      ((list? sources) sources)
      (else (list sources))))
  (let* ((result
            (for/fold
              ((res empty))
              ((source (get-list-of-sources sources)))
              (append res
                      (single-file-cache-posts
                          #:source source
                          #:read-depth read-depth)))))
      (target-persistence result)
      #t))

(define-catch (get-last-posts
                  #:group (group-url #f)
                  #:group-id (group-id #f)
                  #:user (user-url #f)
                  #:user-id (user-id #f)
                  #:limit (limit 1)
                  #:entity (entity #f)
                  #:success-display (success-display #f)
                  #:on-closed-wall (on-closed-wall #f)
                  #:on-error (on-error-f #f)
                  #:delay (delay-time #f)
                  )
  (when delay-time (sleep delay-time))
  (let* (
        (group-id
					(or
						group-id
	          (and group-url
	            (let* (
	                  (group-name (get-vk-name-from-url group-url))
	                  (group-id (hash-ref (Name-id-hash) group-name #f))
	                  (group-id (and group-name (or group-id (get-group-id group-name)))))
	                group-id))))
        (user-id
					(or
						user-id
	          (and user-url
	            (let* ((user-name (get-vk-name-from-url user-url))
	                  (user-id (and user-name (get-user-id user-name))))
	                user-id)))))
        (cond
          ((and (not group-id) (not user-id))
            empty)
          (else
            (let* (
                  (response
                        (cond
                          (group-id
                              (get-wall-posts group-id
                                  #:limit limit
                                  #:break-if-error #f
                                  #:success-display success-display
                                  #:on-closed-wall on-closed-wall
                                  #:do-when-error (λ (err)
                                                    (display err)
                                                    (flush-output)
                                                    (if on-error-f (on-error-f err group-id) #f)
                                                    )))
                          (user-id
                             (get-wall-posts user-id
          	                      #:group? #f
          	                      #:limit limit
          	                      #:break-if-error #f
          	                      #:success-display success-display
                                  #:on-closed-wall on-closed-wall
          	                      #:do-when-error (λ (err)
          	                                        (display (format " ~a " user-id))
          	                                        (flush-output)
                                                    (if on-error-f (on-error-f err user-id) #f)
          	                                        ))))))
              (and
                response
                (let* (
                      (hours-shift (if (server?) HOURS_SHIFT 0))
                      (posts ($ items response))
                      (posts (map (λ (post)
                                    (let ((likes (->number ($ likes.count post)))
                                          (reposts (->number ($ reposts.count post)))
                                          (comments (->number ($ comments.count post)))
                                          (users (and entity ($ u entity) (->number (take-one ($ u entity)))))
                                          (place (and entity
                                                      (or
                                                        ($ place entity)
                                                        ($ hq entity)
                                                        ($ city entity)
                                                        ($ +place entity)
                                                        ($ _parent entity)
                                                        DEFAULT_PLACE))))
                                      (hash
                                        'title (lshift (string-replace ($ text post) "\t" "") 80)
                                        'gid group-id
                                        'uid user-id
                                        'entity-id (and entity ($ __id entity))
                                        'place place
                                        'url (if group-id
                                                (format "https://vk.com/wall-~a_~a" group-id ($ id post))
                                                (format "https://vk.com/wall~a_~a" user-id ($ id post)))
                                        'date (hdate->string (seconds->hdate (+ (* hours-shift 3600) ($ date post))))
                                        'date-seconds ($ date post)
                                        'img-urls (get-img-urls post)
                                        'video-img-urls (get-video-img-urls post)
                                        'likes likes
                                        'reposts reposts
                                        'comments comments
                                        'users users
                                        'repost? (repost? post)
                                        'text ($ text post))))
                                  posts)))
                  posts)))))))

(define-catch (get-trigger-words trigger #:aliases (aliases #f) #:fallback-value (fallback-value #f))
    (let* ((trigger-string (cond
                                  ((and aliases (hash? aliases)) (hash-ref* aliases trigger fallback-value))
                                  ((string? trigger) trigger)
                                  (else fallback-value)))
          (trigger-list (if (boolean? trigger-string)
                                    trigger-string
                                    (and trigger-string (string-split trigger-string "//")))))
      trigger-list))

(define-catch (user-post? post)
  ($ uid post))

(define-catch (group-post? post)
  ($ gid post))

(define-catch (contains-special-tags? text)
  (ormap
    (λ (tag) (string-contains? text tag))
    SPECIAL_TAGS))

(define-catch (mark-post-text-with post open-tag close-tag #:marked-text m)
  (let* (
        (M (titlefy m))
        (text ($ text post))
        (text (string-replace text m (format "~a~a~a" open-tag m close-tag)))
        (text (string-replace text M (format "~a~a~a" open-tag M close-tag))))
    (hash-union (hash 'text text) post)))

(define-catch (filter-posts posts #:entities entities #:type (type #f) #:trigger (trigger #f) #:trigger-expression (trigger-expression #f) #:within-days (within-days #f) #:start-n (start-n #f) #:n (n #f) #:min-symbols (min-symbols #f) #:max-symbols (max-symbols #f) #:use-special-tags (use-special-tags? #f))
  (let* (
        (trigger-aliases
          (let* (
                (triggers ($3 triggers (parse-tab-tree "../knowledge/_triggers.tree")))
                (trigger_id_values (for/hash ((trigger triggers)) (values ($ __id trigger) ($ v trigger)))))
                            trigger_id_values ))
        ; filter by publication date
        (posts (filter-not
            (λ (post)
              (let* ((curdate (current-date))
                    (postdate (take-one ($ date post) #:delimeter " "))) ; 20.08.2019 12:35 -> 20.08.2019
                (> (date-diff-abs postdate curdate) within-days)))
            posts))
        ; filter by type
        (posts (cond
                  ((not type) posts)
                  ((equal? type 'user) (filter user-post? posts))
                  ((equal? type 'group) (filter group-post? posts))
                  (else posts)))
        ; filter by keywords
        (posts (if-not (or trigger trigger-expression)
                  posts
                  ; TODO filter -> for/fold to provide an opportunity to substitute matched text
                  (for/fold
                    ((res empty))
                    ((post posts))
                    (let* (
                            (current-trigger (or trigger-expression (get-entity-parameter ($ entity-id post) trigger entities)))
                            (current-trigger-expression? (trigger-expression? current-trigger)))
                      (cond
                        ; ((not (@id ($ entity-id post) entities)) #f)
                        ((equal? "<none>" current-trigger) res) ; ignore
                        ((equal? "<f>" current-trigger) (pushr res post)) ; reset
                        ((and use-special-tags? (contains-special-tags? ($ text post)))
                          (pushr res post))
                        (current-trigger-expression?
                          (let* ((m? (match-trigger-expression? (remove-hashtags ($ text post)) current-trigger #:aliases trigger-aliases)))
                            (if m?
                                (pushr res (mark-post-text-with post "<b>" "</b>" #:marked-text m?))
                                res)))
                        (current-trigger
                          (let* ((m? (match-words? (remove-hashtags ($ text post)) (get-trigger-words current-trigger #:aliases trigger-aliases #:fallback-value #f))))
                            (if m?
                              (pushr res (mark-post-text-with post "<b>" "</b>" #:marked-text m?))
                              res)))
                        (else #t))))))
        ; exclude reposts
        (posts (filter-not (λ (x) ($ repost? x)) posts))
        ; filter by text properties
        (posts (filter-not
                  (λ (x)
                    (or
                      (equal? ($ text x) "")
                      (<= (string-length ($ text x)) (or min-symbols 0))
                      (and min-symbols (< (string-length (clean-htmlify ($ text x))) min-symbols))
                      (and max-symbols (> (string-length (clean-htmlify ($ text x))) max-symbols))
                      ))
                  posts))
        ; exclude doubles
        (posts (sort posts (λ (a b) (< (datetimestr->seconds ($ date a)) (datetimestr->seconds ($ date b))))))
        (posts (for/fold
                  ((res empty))
                  ((post posts))
                  (let* ((double? (ormap
                                    (λ (x) (similar-text? ($ text post) ($ text x)))
                                    res)))
                    (cond
                      (double? res)
                      ((not double?) (pushl res post))))))
        )
    posts))

(define-catch (find-image object-id #:option (option "<t>") #:root (root "photo/") #:downcase (downcase #t))
  (let* ((filename (case option
                    (("<t>" "0")
                      (format "~a.jpg" (if downcase (string-downcase object-id) object-id)))
                    (else
                      (format "~a_~a.jpg" (if downcase (string-downcase object-id) object-id) option))))
        (filepath (format "../www/~a~a" root filename)))
    (if (file-exists? filepath)
      filename
      #f)))

(define-catch (get-title p (default-title ""))
  (or ($ title p) default-title))

; в частности для прибавления приставки "Паркран" при именовании паркранов
(define-catch (get-source-title p c (default-title ""))
  (or
    (and
      c
      (let* ((name (get-name c))
            (name-prefix ($ +name-prefix c))
            (name-prefix (if name-prefix (namefy name-prefix) ""))
            (name-prefix-len (string-length name-prefix))
            (name-already-prefixed? (and
                                      (>= (string-length name) (string-length name-prefix))
                                      (equal? name-prefix (substring name 0 (string-length name-prefix)))))
            (name (if (or name-already-prefixed? (not name-prefix))
                      name
                      (str name-prefix name)))
            (name (or name default-title)))
        name))
    (format "public~a" ($ gid p))))

(define (get-sitemap #:only-visible-pages? (only-visible? #f))
  (let* ((pages (get-leaves (parse-tab-mtree "../knowledge/_sitemap.mtree")))
        (pages (if only-visible?
                  (filter
                      (λ (page) (or ($ n page) ($ show page)))
                      pages)
                  pages)))
    pages))

(define-catch (cur-y-m-d)
  (format "~a-~a-~a" (current-year) (current-month) (current-day)))

(define-catch (cached-alias->id alias)
  (persistent h-galias-gid)
  (persistent h-ualias-uid)
  (let* (
        (h-alias-id (hash-union (h-galias-gid) (h-ualias-uid))))
    (hash-ref* h-alias-id alias)))
