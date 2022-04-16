#lang racket

(require "../../_lib_links/odysseus_all.rkt")
(require "../../_lib_links/odysseus_tabtree.rkt")
(require "../../_lib_links/odysseus_scrap.rkt")
(require "../../_lib_links/odysseus_report.rkt")
(require "../../_lib_links/odysseus_whereami.rkt")
(require "../../_lib_links/settings.rkt")

(require "../_lib/globals.rkt")
(require "../_lib/functions.rkt")
(require "../_lib/page_snippets.rkt")
(require "../_lib/yandex_map.rkt")

(define-namespace-anchor a)
(define ns (namespace-anchor->namespace a))

(define news_cards "")
(define page-id "")

(persistent h-galias-gid)
(persistent h-user-wall-posts)
(persistent h-user-wall-posts-new)
(persistent uid-closed-wall)
(persistent uids-selected)

(define Updates (make-parameter (hash)))
(Updates (if (file-exists? (_cache "page_updates.txt"))
                (read-serialized-data-from-file (_cache "page_updates.txt"))
                (hash)))

(define taganrog.tree "../knowledge/taganrog.tree")

(define items (get-entities taganrog.tree))

(define PAGES (get-sitemap))

(define (update-cache-2 uids-selected)
  (let* ((all-uids (uids-selected))
        (scanned-uids (hash-keys (h-user-wall-posts)))
        (closed-uids (uid-closed-wall))
        (non-scanned-uids (minus all-uids (append scanned-uids closed-uids)))
        )
    (cache-posts-by-uid
        #:uids non-scanned-uids
        #:cache-to h-user-wall-posts-new
        #:closed-to uid-closed-wall
        #:read-depth 30
        #:cache-write-frequency 50
        )))

(define-catch (update-page page_id #:note (note "") #:template (template-name #f) #:gen-ext (gen-ext "html") #:filename (filename #f))
  (unless (empty-string? note) (--- (str "\n" note)))
  (set! page-id page_id)
  (let* ((page-id-string (string-downcase (->string page-id)))
        (file-to-write (or filename (str page-id-string "." gen-ext)))
        (server-path "/home/denis/projects/taganoskop/www/")
        (template-name (or template-name page-id-string))
        (processed-template (process-html-template (format "../_templates/~a.t" template-name) #:tabtree-root "../knowledge" #:namespace ns)))
    (Updates (hash-union (hash page-id (cur-y-m-d)) (Updates)))
    (write-file (format "../www/~a" file-to-write) processed-template)
    (-s (write-file (format "~a~a" server-path file-to-write) processed-template))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(--- (format "~a: Считываем посты со стен пользователей" (timestamp)))

; (update-cache-2 uids-selected)

(--- "Компилируем ленту")

(define filtered_posts (filter-posts
                          (flatten (hash-values (h-user-wall-posts)))
                          #:entities items
                          ; #:trigger-expression '(++ event_future)
                          #:trigger-expression '(++ running volunteers)
                          #:within-days 360
                          #:min-symbols MIN_SYMBOLS))
(set! news_cards (make-cards
                    filtered_posts
                    #:entities items
                    ))
(update-page 'Search_people #:note "Посты людей по триггерам" #:template "news")

(--- (format "~a Конец компиляции~n~n" (timestamp)))
