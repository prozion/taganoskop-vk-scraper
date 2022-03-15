#lang racket

(require odysseus)
(require tabtree/tabtree1)
(require tabtree/template-functions)
(require tabtree/html)
(require odysseus/api/vk)
(require compatibility/defmacro)
(require (file "~/.private/APIs.rkt"))

(require "../_lib/globals.rkt")
(require "../_lib/functions.rkt")
(require "../_lib/page_snippets.rkt")
(require "../_lib/yandex_map.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define news_cards "")
(define page-id "")

(set-access-token ($ access_token vk/postagg2_1))

(persistent h-galias-gid)

(define Updates (make-parameter (hash)))
(Updates (if (file-exists? (_cache "page_updates.txt"))
                (read-serialized-data-from-file (_cache "page_updates.txt"))
                (hash)))

(define PAGES (get-sitemap))

(define-catch (update-page page_id #:note (note "") #:template (template-name #f) #:gen-ext (gen-ext "html") #:filename (filename #f))
  (unless (empty-string? note) (--- (str "\n" note)))
  (set! page-id page_id)
  (let* ((page-id-string (string-downcase (->string page-id)))
        (file-to-write (or filename (str page-id-string "." gen-ext)))
        (template-name (or template-name page-id-string))
        (processed-template (process-html-template (format "../_templates/~a.t" template-name) #:tabtree-root "../knowledge" #:namespace ns)))
    (Updates (hash-union (hash page-id (cur-y-m-d)) (Updates)))
    (write-file (format "~a/~a" SERVER_DIR file-to-write) processed-template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(--- (format "~a: Обновляем контент сайта" (timestamp)))

(define (is-flag? flag)
  (indexof?
    (vector->list (current-command-line-arguments))
    flag))

(define-macro (generate-page page-id trigger-expr note filename to-cache?)
  (let* ((tree-file-name (string->symbol (format "~a.tree" (symbol->string page-id))))
        (all-posts-cache-var (string->symbol (format "~a-posts" (string-downcase (symbol->string page-id)))))
        (group-items-var (string->symbol (format "~a-items" (string-downcase (symbol->string page-id)))))
        (tree-file-path (format "../knowledge/~a" tree-file-name))
        (caching-code `(cache-posts
                          #:source (list ,tree-file-path)
                          #:target ,all-posts-cache-var
                          #:ignore-with-status #t
                          #:ignore-sleepy #t
                          #:read-depth 10)))
    `(begin
        (persistent ,all-posts-cache-var)
        (define ,group-items-var (get-entities ,tree-file-path))
        (parameterize ((Name-id-hash (h-galias-gid)))
          ,(when to-cache? caching-code)
          (set! news_cards (make-cards
                              (filter-posts
                                  (,all-posts-cache-var)
                                  #:entities ,group-items-var
                                  #:trigger-expression ',trigger-expr
                                  #:use-special-tags #t
                                  #:within-days WITHIN_DAYS
                                  #:min-symbols MIN_SYMBOLS)
                              #:entities ,group-items-var
                              ))
          (update-page ',page-id #:note ,note #:template "news" #:filename ,filename)))))


(--- "Компилируем страницы сайта")

(generate-page taganrog (++ event_future event_by_date) "Таганрог" "index.html" #t)
(generate-page history identity "История Таганрога" "history.html" #t)
; (generate-page it identity "IT-сообщество" "it.html" #t)

(write-data-to-file (Updates) (_cache "page_updates.txt"))

; trigger uploading the new files onto taganoskop.denis-shirshov.ru server:
; (-s (get-url "https://taganoskop.ru/updater.php"))
(get-url "https://taganoskop.ru/updater.php")

(--- (format "~a Конец компиляции~n~n" (timestamp)))
