#lang racket

; функции для вывода информации о базе

(require "../../_lib_links/odysseus_all.rkt")
(require "../../_lib_links/odysseus_tabtree.rkt")
(require "../../_lib_links/odysseus_scrap.rkt")

(require json)

(require "../_lib/functions.rkt")
(require "../_lib/scraping_vk.rkt")

(define CACHE_DIR "_cache")



; (persistent h-gid-uids)
; (h-gid-uids (hash-delete (h-gid-uids) 21920914))

(persistent uids-selected)
(--- "Количество пользователей для сканирования всего:" (length (uids-selected)))

; (---
; (for/fold
;   ((res 0))
;   ((item (get-leaves (parse-tab-tree "../data/cpu_teams.tree"))))
;   (+ res (->number (or ($ u item) 0)))))
;
; (persistent h-uid-info)
; (define users-info (h-uid-info))
; (--- "Количество уже отсканированных пользователей:" (length (hash-keys users-info)))

(persistent h-user-wall-posts)
(--- "Количество пользователей с постами:" (length (hash-keys (h-user-wall-posts))))

; (--- (string->jsexpr
;         (get-url (format "https://api.vk.com/method/wall.get?owner_id=~a&v=~a&&offset=1&&count=1&access_token=~a" "9710888" VK_API_VERSION AT))))
