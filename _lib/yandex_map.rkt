#lang racket

(require compatibility/defmacro)
(require "../../_lib_links/odysseus_all.rkt")
(require "../../_lib_links/odysseus_tabtree.rkt")
(require "../../_lib_links/odysseus_scrap.rkt")
(require "../../_lib_links/odysseus_report.rkt")
(require "../../_lib_links/odysseus_whereami.rkt")
(require "../../_lib_links/settings.rkt")

(require "globals.rkt")
(require "geography.rkt")
(require "functions.rkt")

(require (for-syntax "../../_lib_links/odysseus_all.rkt"))

(provide (all-defined-out))

(define (get-post-date post)
  (or ($ date post) ($ start post) ($ date-from post)))

(define (get-url* item)
  (or ($ url item) ($ vk item) ($ fb item) ($ insta item) ($ org-url item) ($ org-vk item) ($ org-fb item) ($ org-insta item)))

(define-macro (pn var)
  (let ((var_str (format "~a" var)))
    `(if ,var (format "~a:~a " ,var_str ,var) "")))

(define-catch (microshift-location base-location #:mode (mode 'lat) #:radius (radius 100))
; 30 m difference:
; 44.561076, 38.076836
; 44.561002, 38.077154
  (let* ((k-lat (* 0.000005 (/ radius 100.0)))
        (k-lon (* 0.00001 (/ radius 100.0)))
        (k (if (equal? mode 'lat) k-lat k-lon)))
    (+
      (* k (- 50 (random 100)))
      (->number base-location))))

(define (improve-date-str date_str)
  (let* ((res (take-one date_str #:delimeter ","))
        (res ((change-text
                (list
                  (cons " января " ".01.")
                  (cons " февраля " ".02.")
                  (cons " марта " ".03.")
                  (cons " апреля " ".04.")
                  (cons " мая " ".05.")
                  (cons " июня " ".06.")
                  (cons " июля " ".07.")
                  (cons " августа " ".08.")
                  (cons " сентября " ".09.")
                  (cons " октября " ".10.")
                  (cons " ноября " ".11.")
                  (cons " декабря " ".12."))) res))
        (res (string-split res "."))
        (day (first res))
        (day (format-number "dd" day #:filler "0"))
        (res (string-join (cons day (cdr res)) "."))
        )
    res))

(define (improve-id id)
  (let* (
        (res ((change-text
                (list
                  (cons "__" "_")
                  (cons "_—_" "_")
                  (cons "_–_" "_")
                ))
              id))
        )
    res))

(define (improve-name name)
  (let* (
        (res ((change-text
                (list
                  (cons "- " "-")
                  (cons "г. " "")
                  (cons "пос. " "")
                  (cons ";" " ")
                  (cons "," " ")
                  (cons "  " " ")
                  (cons (regexp "[A-ZА-Я]\\. ") "")
                ))
              name))
        )
    res))

(define (make-output-post-parameters post)
  (hash 'name
            (if ($ name post)
              ($ name post)
              (namefy ($ entity-id post)))
        'date
            (get-post-date post)
        'day
            (or (->int (first (string-split (get-post-date post) "."))) "?")
        'month
            (second (string-split (get-post-date post) "."))
        'month_abbr
            (hash-ref month-abbrs (second (string-split (get-post-date post) ".")) "")
        'place
            (print-place ($ place post))
        'image
            (or (get-vk-image post) "false")
        'url
            (let ((url (or ($ url post))))
              (if url (httpify url) ""))
        'url_html
            (let* ((url ($ url post)))
              (if url
                (format "<a href='~a' target='_blank'>~a</a>" (httpify url) "Перейти к посту")
                ""))
        'text (htmlify-text ($ text post))
        '_new
            (if (or ($ today post) (equal? ($ _parent post) "today"))
              "true"
              "false")
))
