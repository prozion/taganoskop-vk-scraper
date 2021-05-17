#lang racket

(require odysseus)
(require tabtree)
(require tabtree/template-functions)
(require tabtree/html)
(require odysseus/api/vk)
(require (file "~/.private/APIs.rkt"))

(require "../_lib/globals.rkt")
(require "../_lib/functions.rkt")
(require "../_lib/page_snippets.rkt")
(require "../_lib/yandex_map.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define news_cards "")
(define page-id "")

(set-access-token ($ access_token vk/postagg3_1))

(persistent h-galias-gid)
(persistent tgn-posts)
; (persistent rnd-posts)
; (persistent south-posts)
(persistent it-posts)

(define Updates (make-parameter (hash)))
(Updates (if (file-exists? (_cache "page_updates.txt"))
                (read-serialized-data-from-file (_cache "page_updates.txt"))
                (hash)))

(define taganrog.tree "../knowledge/taganrog.tree")
; (define rnd.tree "../knowledge/rostov.tree")
; (define south.tree "../knowledge/south.tree")
(define it.tree "../knowledge/it.tree")

(define tgn-items (get-entities taganrog.tree))
; (define rnd-items (get-entities rnd.tree))
; (define south-items (get-entities south.tree))
(define it-items (get-entities it.tree))

(define PAGES (get-sitemap))

(define-catch (update-cache)
  (parameterize ((Name-id-hash (h-galias-gid)))
    ; (cache-posts
    ;     #:source (list taganrog.tree)
    ;     #:write-to-cache (_cache "tgn_posts.txt")
    ;     #:ignore-with-status #t
    ;     #:ignore-sleepy #t
    ;     #:read-depth 10)
    ; (cache-posts
    ;     #:source (list rnd.tree)
    ;     #:write-to-cache "../_cache/rnd_posts.txt"
    ;     #:ignore-with-status #t
    ;     #:ignore-sleepy #t
    ;     #:read-depth 10)
    ; (cache-posts
    ;     #:source (list south.tree)
    ;     #:write-to-cache "../_cache/south_posts.txt"
    ;     #:ignore-with-status #t
    ;     #:ignore-sleepy #t
    ;     #:read-depth 10)
    (cache-posts
        #:source (list it.tree)
        #:write-to-cache (_cache "it_posts.txt")
        #:ignore-with-status #t
        #:ignore-sleepy #t
        #:read-depth 10)
  #t))

(define-catch (update-page page_id #:note (note "") #:template (template-name #f) #:gen-ext (gen-ext "html"))
  (unless (empty-string? note) (--- (str "\n" note)))
  (set! page-id page_id)
  (let* ((page-id-string (string-downcase (->string page-id)))
        (template-name (or template-name page-id-string))
        (processed-template (process-html-template (format "../_templates/~a.t" template-name) #:tabtree-root "../knowledge" #:namespace ns)))
    (Updates (hash-union (hash page-id (cur-y-m-d)) (Updates)))
    (write-file (format "~a/~a.~a" SERVER_DIR page-id-string gen-ext) processed-template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(--- (format "~a: Обновляем контент сайта" (timestamp)))

; (-s
;   (--- (format "Читаем новые посты, обновляем кэш"))
;   (update-cache))

(update-cache)

(--- "Компилируем страницы сайта")

(set! news_cards (make-cards
                    (filter-posts
                        (tgn-posts)
                        #:entities tgn-items
                        #:trigger-expression '(++ event_future event_by_date)
                        #:use-special-tags #t
                        #:within-days WITHIN_DAYS
                        #:min-symbols MIN_SYMBOLS)
                    #:entities tgn-items
                    ))
(update-page 'Tgn #:note "Таганрог" #:template "news")

; (set! news_cards (make-cards
;                     (filter-posts
;                         (rnd-posts)
;                         #:entities rnd-items
;                         #:trigger-expression '(++ event_future event_by_date)
;                         #:use-special-tags #t
;                         #:within-days WITHIN_DAYS
;                         #:min-symbols MIN_SYMBOLS)
;                     #:entities rnd-items
;                     ))
; (update-page 'Rnd #:note "Ростовская агломерация" #:template "news")
;
; (set! news_cards (make-cards
;                     (filter-posts
;                         (south-posts)
;                         #:entities south-items
;                         #:trigger-expression '(++ event_future event_by_date)
;                         #:use-special-tags #t
;                         #:within-days WITHIN_DAYS
;                         #:min-symbols MIN_SYMBOLS)
;                     #:entities south-items
;                     ))
; (update-page 'South #:note "Южный регион" #:template "news")

(set! news_cards (make-cards
                    (filter-posts
                        (it-posts)
                        #:entities it-items
                        #:trigger-expression '(++ event_future event_by_date)
                        #:use-special-tags #t
                        #:within-days WITHIN_DAYS
                        #:min-symbols MIN_SYMBOLS)
                    #:entities it-items
                    ))
(update-page 'It #:note "IT Сообщество" #:template "news")


(set! PAGES (get-sitemap #:only-visible-pages? #t))
(update-page 'Sitemap #:template "sitemap.xml" #:gen-ext "xml")

(write-data-to-file (Updates) (_cache "page_updates.txt"))

; trigger uploading the new files onto cpu.denis-shirshov.ru server:
; (-s (get-url "http://taganrog.online/updater.php"))
(get-url "http://taganrog.online/updater.php")

(--- (format "~a Конец компиляции~n~n" (timestamp)))
