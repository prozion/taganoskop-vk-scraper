#lang racket

(require "../../_lib_links/odysseus_all.rkt")
(require "../../_lib_links/odysseus_scrap.rkt")
(require "../../_lib_links/odysseus_tabtree.rkt")
(require "../../_lib_links/settings.rkt")
(require racket/serialize)
(require (rename-in racket/hash (hash-union hash-union-racket)))

(require "../_lib/functions.rkt")
(require "../_lib/scraping_vk.rkt")
(require "../_lib/globals.rkt")

(define-catch (get-h-gid-uids h-gid-uids-existed)
  (persistent h-galias-gid)
  (persistent h-gid-uids-new)
  (let* (
        ; находим id для новодобавленных групп:
        (h-galias-gid-new (for/fold
                            ((res (hash)))
                            ((vk-alias (get-vk-alias
                                      (map
                                        (curry string-append "../knowledge/")
                                        (list "taganrog.tree"))
                                      #:members-no-more-than MAX_MEMBERS_IN_SCANNED_GROUPS
                                      #:ignore-i? #t)))
                            (cond
                              ((hash-ref (h-galias-gid) vk-alias #f) res)
                              (else (hash-set res vk-alias (get-gid vk-alias #:delay 0.1 #:display? (format " [get-gid ~a] " vk-alias)))))))
        (_ (h-galias-gid h-galias-gid-new 'append))

        ; все известные id групп
        (gids-in-knowledge (opt/uniques (cleanmap (hash-values (h-galias-gid)))))

        ; все группы, из которых уже получены списки пользователей
        (gids-scraped-old (hash-keys h-gid-uids-existed))

        ; еще непросканированные группы
        (gids-to-scrap (minus gids-in-knowledge gids-scraped-old))

        ; получаем списки пользователей новых групп и сохраняем в персистент h_gid_uids
        (_ (get-gid-uids gids-to-scrap h-gid-uids gids-scraped-old #:file-write-frequency 10))
        )
    #t))

(define (get-gids-scraped h-gid-uids-all)
  (persistent gids-scraped)
  (let* (
        ; обновляем gids-scraped
        (_ (gids-scraped (map ->number (hash-keys h-gid-uids-all))))
        )
    #t))

(define-catch (get-h-ualias-uid treefile)
  (let* ((ualiases (get-vk-alias (list treefile)))
        ; (_ (--- ualiases))
        (_ (h-ualias-uid (for/fold
                            ((res (hash)))
                            ((ualias ualiases))
                            (cond
                              ((hash-ref (h-ualias-uid) ualias #f) res)
                              (else (hash-set res ualias (get-user-id ualias #:delay 0.1 #:display? (format " [get-user-id ~a] " ualias))))))))
        )
    #t))


(define-catch (get-h-uid-fids uids)
  (persistent h-uid-fids)
  (let* (
        ; находим id для новодобавленных групп:
        (_ (h-uid-fids (for/fold
                            ((res (hash)))
                            ((uid uids))
                            (hash-set res uid (get-friends-of-user uid)))))

        )
        #t))

(define (get-score h-gid-uids #:by-tags (by-tags #f) #:by-clusters (by-clusters #f))
  (define (get-score-cluster res-current gid (display-symbol "+"))
    (display display-symbol) (flush-output)
    (let* ((uids (hash-ref h-gid-uids gid empty)))
      (for/fold
          ((res2 res-current))
          ((uid uids))
          (hash-set res2 uid (+ 1 (hash-ref res2 uid 0))))))
  (persistent h-galias-gid)
  (persistent h-uid-score)
  (let* (
        ; определяем кластеры групп, по которым считать частоту вхождения
        (a-h-galias-gid (h-galias-gid))
        (teams? (groups-cluster? 'teams a-h-galias-gid))

        (tagged? (and by-tags (tagged-groups? by-tags a-h-galias-gid)))

        (gids (hash-keys h-gid-uids))

        ; определяем частоту вхождения
        (_ (when
              (not-empty? gids)
              (h-uid-score (for/fold
                              ((res1 (hash)))
                              ((gid gids))
                                (cond
                                  ((and tagged? (tagged? gid))
                                    (get-score-cluster res1 gid "t"))
                                  ((and (not tagged?) by-clusters
                                        (or
                                          (teams? gid)
                                        ))
                                    (get-score-cluster res1 gid "c"))
                                  ((and (not by-tags) (not by-clusters))
                                    (get-score-cluster res1 gid "+"))
                                  (else
                                    (display "-") (flush-output)
                                    res1)))))))
    #t))

(define (get-uids-selected h-uid-score)
  (persistent uids-selected)
  ; берем только тех пользователей, которые входят в MIN_MEMBER и более групп
  (uids-selected
    (opt/uniques
      (select-uids
        h-uid-score
        #:filter (λ (k v)
                    (>= v MIN_MEMBER)))))
  #t)

(define-catch (get-h-uid-info h-gid-uids h-uid-info-all)
  (persistent h-uid-info)
  (persistent uids-selected)
  (--- "создаем список уже отсканированных пользователей")
  (let* (
        (uids-scraped (hash-keys h-uid-info-all))
        )
    (--- "получаем данные профилей новых пользователей")
    (for/fold
      ((res (hash)) (buffer_count 0))
      ((uid (uids-selected)))
      (cond
        ; данные пользователя уже были ранее считаны:
        ((member uid uids-scraped)
          (display (format " [~a] " uid))
          (flush-output)
          (values res buffer_count))
        ; запись в файл только после считывания достаточного числа пользователей, чтобы диск не теребонькать каждый раз
        ((> buffer_count FILE_WRITE_FREQUENCY)
          (let ((res2 (hash-union
                        res
                        (hash
                          uid
                          (get-uid-info uid #:delay 0.1 #:display? (format " [get-uid-info ~a] " uid))))))
            (h-uid-info
                        res2
                        'append
                        'dup)
            (values (hash) 0)))
        (else
          (let ((res2 (hash-union
                        res
                        (hash
                          uid
                          (get-uid-info uid #:delay 0.1 #:display? (format " [get-uid-info ~a] " uid))))))
            (values res2 (+ 1 buffer_count))))))
    #t))

(define (break-uid-info-fids-and-gids h-uid-info)
  (persistent h-uid-info-wo-rels)
  (persistent h-uid-fids)
  (persistent h-uid-gids)
  (persistent h-uid-fids-scraped)
  (let* (
        (h-uid-info-full h-uid-info)
        (uids-scraped (hash-keys h-uid-info-full))
        (h-uid-info-new (hash-map
                                  (λ (k v) (values k (hash-delete-all v '(friend-ids group-ids))))
                                  h-uid-info-full))
        ; (h-uid-fids-new (for/fold
        ;                   ((res (hash)))
        ;                   (((k v) h-uid-info-full))
        ;                   (cond
        ;                     ((empty? ($ friend-ids v)) res)
        ;                     (else (hash-set res k (map ->string ($ friend-ids v)))))))
        (h-uid-gids-new (for/fold
                          ((res (hash)))
                          (((k v) h-uid-info-full))
                          (cond
                            ((empty? ($ group-ids v)) res)
                            (else (hash-set res k ($ group-ids v))))))
        ; (h-uid-fids-scraped-new (hash-map
        ;                                   (λ (k v) (let ((fids-scraped (intersect uids-scraped v)))
        ;                                               (display (format "~a " (length fids-scraped))) (flush-output)
        ;                                               (values
        ;                                                 k
        ;                                                 fids-scraped)))
        ;                                                 ; (filter (λ (fid) (member fid uids-scraped)) v)))
        ;                                   h-uid-fids-new))
        (_ (h-uid-info-wo-rels h-uid-info-new))
        ; (_ (h-uid-fids h-uid-fids-new))
        (_ (h-uid-gids h-uid-gids-new))
        ; (_ (h-uid-fids-scraped h-uid-fids-scraped-new))
        )
    #t))

; (persistent h-uid-info)
; (persistent h-gid-uids-new)
(persistent h-gid-uids)
(persistent h-uid-score)
(persistent h-ualias-uid)

(let* (
      ; (_ (--- "getting already scanned user info"))
      ; (h-uid-info-all (h-uid-info))

      ; (_ (--- "getting user ids in the groups"))
      (h-gid-uids-all (h-gid-uids))
      )

  ; (--- "Всего уникальных uid:" (length (uniques (flatten (hash-values h-gid-uids-all)))))


; (get-h-ualias-uid "../data/cpu_people.tree")

; (get-h-uid-fids (list "21936322"))

;;;;;;;;;;;;;;;;;;;;;;;;;
  ; (--- "for each group get its id and a list of user ids")
  ; (get-h-gid-uids h-gid-uids-all)
  ;
  ; (h-gid-uids (h-gid-uids-new) 'append)
  ; (set! h-gid-uids-all (h-gid-uids))
  ;
  ; (--- "update info on already scraped groups")
  ; (get-gids-scraped h-gid-uids-all)
  ;
  (--- "getting a score of users")
  (get-score h-gid-uids-all)
  ;
  (--- "getting user ids for scanning")
  (get-uids-selected (h-uid-score))


;;;;;;;;;;;;;;;;;;;;;;;;
  ; (--- "getting users info by their id")
  ; (get-h-uid-info h-gid-uids-all h-uid-info-all)

  ; (break-uid-info-fids-and-gids h-uid-info-all)

  #t)
