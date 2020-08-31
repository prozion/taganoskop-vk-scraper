#lang racket

(require "../../_lib_links/odysseus_all.rkt")
(require "../../_lib_links/odysseus_tabtree.rkt")
(require "../../_lib_links/odysseus_scrap.rkt")
(require racket/serialize)
(require compatibility/defmacro)
(require (for-syntax racket/match racket/syntax racket/format))

(require "functions.rkt")
(require "globals.rkt")

(provide (all-defined-out))

(define-catch (get-vk-alias tabtree #:result (result empty) #:members-no-more-than (members-no-more-than #f) #:ignore-i? (ignore-i? #f))
  (map
    extract-pure-alias
    (remove-duplicates
      (cond
        ((empty? tabtree) result)
        ((list? tabtree)
                          (get-vk-alias
                            (cdr tabtree)
                            #:result
                            (append
                              result
                                (filter-map
                                  (λ (x) (and
                                            (or
                                              (not members-no-more-than)
                                              (< (members-number x) members-no-more-than))
                                            ($ vk x)))
                                  (filter
                                    (λ (x) (and
                                              (if ignore-i?
                                                (not ($* i x))
                                                #t)))
                                    (get-leaves (parse-tab-tree (car tabtree))))))))
        (else (get-vk-alias (list tabtree)))))))

(define-catch (select-uids h-uid-score #:filter selection-lambda)
  (map
    car
    (sort
      (hash->list
        (for/fold
          ((res (hash)))
          (((k v) h-uid-score))
          (cond
            ((selection-lambda k v) (hash-set res k v))
            (else res))))
      (λ (a b) (> (cdr a) (cdr b))))))

; добавляем id участников групп в виде списка, привязанного к хэш-ключу со значением id соответствующей группы:
(define-catch (get-gid-uids gids (persistent-h-gid-uids #f) (gids-scraped empty) #:file-write-frequency (file-write-frequency #f))
  (let-values (((res _)
                (for/fold
                  ((res (if persistent-h-gid-uids
                            (persistent-h-gid-uids)
                            (hash)))
                    (buffer-n 0))
                  ((gid gids))
                  (cond
                    ; если для данной группы уже есть список участников, то ничего не добавляем к расширяемому хэшу::
                    ((member gid gids-scraped)
                      (display ".") ; отображаем в консоли скип считывания данных
                      (flush-output)
                      (values res buffer-n))
                    ; если группа новая, то запрашиваем список ее участников через VK API:
                    (else
                      (let ((new-res (hash-set res gid (get-group-users gid #:delay 0.1 #:display? (format " [get-gid-uids ~a] " gid) ))))
                        (cond
                          ((and persistent-h-gid-uids (> buffer-n file-write-frequency))
                            ; сохраняем новые полученные данные в файл, чтобы они пропали зря:
                            (persistent-h-gid-uids new-res)
                            (values new-res 0))
                          (else
                            (values new-res (inc buffer-n))))
                      ))))))
      res))

(define-catch (get-uid-info uid #:delay (delay-time 0) #:display? (display? #f))
  (let* (
        (_ (sleep delay-time))
        (friend-ids (get-friends-of-user uid))
        (_ (sleep delay-time))
        (group-ids (get-groups-of-user uid))
        (_ (sleep delay-time))
        (user (get-user-info uid #:display? display?))
        (job ($ career user))
        (job (first-or-false job))
        (job-position (and job ($ position job)))
        (job-gid (and job ($ group_id job)))
        (universities ($ universities user))
        (chair-names (and universities (map (λ (x) ($ chair_name x)) universities)))
        )
    (hash-map
      (λ (k v) (values
                  k
                  (if (string? v)
                    (string-trim v)
                    v)))
        (hash
          'uid uid
          'group-ids group-ids
          'friend-ids friend-ids
          'name ($ first_name user)
          'surname ($ last_name user)
          'bdate ($ bdate user)
          'sex (case ($ sex user)
                        ((1) "f")
                        ((2) "m")
                        (else "?"))
          'city ($ city.title user)
          'city-home ($ home_town user)
          'university ($ university_name user)
          'faculty ($ faculty_name user)
          'chair-names chair-names
          'job-position job-position
          'job-gid job-gid
          'photo-url ($ photo_max user)
          'contacts (hash
                      'site ($ site user)
                      'insta ($ instagram user)
                      'mobile-phone ($ mobile_phone user))
          'interests ($ interests user)
          'relation-partner-id ($ relation_partner.id user)
          'last-seen ($ last_seen.time user)
          '_error ($ error user) ; если при чтении случилась ошибка, фиксируем факт, потом отфильтруем
        ))))

(define-catch (print-uid-score
                  uid-scores
                  #:score-lower-limit (limit #f)
                  #:recorded-uids (recorded-uids #f)
                  #:file-to-write (file-to-write #f))
  (let* (
        (recorded-uids (or recorded-uids empty))
        (uid-scores (if limit
                        (filter (λ (x) (>= (cdr x) limit)) uid-scores)
                        uid-scores))
        (uid-html (for/fold
                        ((res ""))
                        ((u uid-scores))
                        (begin
                          ; (display (format "~a " (car user)))
                          (string-append
                            res
                            (format "<tr class='~a'><td><a href=\"https://vk.com/id~a\">~a</a></td><td>~a</td></tr>"
                                    (if (member (car u) recorded-uids)
                                      "recorded"
                                      "new")
                                      (car u)
                                      (car u)
                                      (cdr u))))))
        (uid-html (format "<head><link rel='stylesheet' href='styles/tmp_styles.css'><script src=\"js/sorttable.js\"></script></head><body><table class=\"sortable\"><tr><th>ID пользователя</th><th>Число вхождений в группы</th></tr>~a</table></body>" uid-html)))
    (when file-to-write (write-file file-to-write uid-html))
    uid-html))

(define-catch (groups-cluster? cluster-id galias-gid)
  (λ (gid)
    (let* ((treefile (hash-ref CLUSTERS cluster-id #f))
          (cluster-galiases (if treefile
                                (filter-map
                                      (λ (x) (extract-pure-alias ($ vk x)))
                                      (get-leaves (parse-tab-tree (str "../data/" treefile))))
                                empty))
          (cluster-gids (filter-map (λ (galias)
                                          (hash-ref galias-gid galias #f))
                                    cluster-galiases)))
      (member gid cluster-gids))))

(define-catch (tagged-groups? tags galias-gid)
  (define (get-item-by-galias galias items)
    (cond
      ((empty? items) #f)
      ((equal*? (extract-pure-alias ($ vk (first items))) galias) (first items))
      (else (get-item-by-galias galias (rest items)))))
  (λ (gid)
    (let* ((all-items (apply
                        append
                        (map
                          (λ (treefile) (get-leaves (parse-tab-tree (str "../data/" treefile))))
                          ; (list "groups_ds.tree" "groups_med.tree"))))
                          (hash-values CLUSTERS))))
          (galias (hash-ref* (hash-revert galias-gid) gid))
          (item-by-gid (get-item-by-galias galias all-items)))
      (cond
        ((not tags) #f)
        ((empty? tags) #f)
        ((not item-by-gid) #f)
        ((scalar? tags) (equal*? ($* tag item-by-gid) tags))
        ((list? tags) (ormap
                        (λ (tag) (equal*? ($* tag item-by-gid) tag))
                        tags))
        (else #f)))))

(define-catch (sn-group? item)
  (or
    ($ vk item) ($ vk-old item)
    ($ fb item) ($ insta item) ($ twi item) ($ tg item) ($ yt item) ($ youtube item)
    ))

(define-catch (group-category? item)
  (not (sn-group? item)))

(define-catch (get-item-parent item plained-tree)
  (cond
    ((not ($ _parent item)) #f)
    (else
      ($$ ($ _parent item) plained-tree))))

(define-catch (get-item-children item plained-tree)
  (cond
    ((empty? ($ _children item)) empty)
    (else
      (map (λ (child-id)
              ($$ child-id plained-tree))
            ($ _children item)))))
