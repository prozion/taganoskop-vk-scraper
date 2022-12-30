#lang racket

(require compatibility/defmacro)
(require odysseus)
(require tabtree)
;
(require "globals.rkt")
;
(require (for-syntax odysseus))

(provide (all-defined-out))

; (define PLACES_COORS (map
;                         (λ (entity)
;                             (hash-union
;                               (hash 'id (string-downcase ($ id entity)))
;                               entity))
;                         (append
;                           (get-leaves (parse-tab-tree "../knowledge/russia_places_1.tree"))
;                           (get-leaves (parse-tab-tree "../knowledge/russia_places_2.tree")))))

(define-catch (get-place post)
  (and
    ($ place post)
    (not (regexp-match #rx"^(\"+|<f>)$" ($ place post)))
    (let* ((raw_placename (namefy ($ place post)))
          (placename (correct-geography-names raw_placename))
          (placename (string-join (map titlefy (string-split placename " ")) " ")))
      placename)))

(define (print-place place)
  (namefy (string-titlecase (string-replace place "__" ", "))))

(define (get-place-parts place)
  (let* ((parts (string-split place "__"))
        (town (first parts))
        (addr (if (> (length parts) 1)
                (second parts)
                "")))
    (hash 'town town 'addr addr)))

(define-catch (get-town place)
  ($ town (get-place-parts place)))

(define-catch (get-post-place post)
  (let ((place ($ place post)))
    (and place (get-town place))))

(define (get-addr place)
  ($ addr (get-place-parts place)))

(define-catch (get-coors-of-the-place place_string places_coors)
  (let* ((place (and place_string ($2 (string-downcase place_string) places_coors)))
        (latlon? (and ($ lat place) ($ lon place)))
        (ref? ($ ref place))
        (coors_hash (or
                  (and latlon? place)
                  (and ref? (get-coors-of-the-place ($ ref place) places_coors))
                  (let ((city_name (first (string-split place_string "__"))))
                    (cond
                      ((equal? city_name place_string) #f)
                      (else (get-coors-of-the-place city_name places_coors)))))))
    coors_hash))

(define-catch (coors-in-south? lat lon)
  (let ((lat (->number lat))
        (lon (->number lon)))
    (and
      lat
      lon
      (> lat 41.0)
      (> lon 32.4)
      (< lon 49.2)
      (or
        ; Воронежский выступ
        (and
          (< lat 52.0)
          (> lon 38.5)
          (< lon 42.3))
        ; Камышинская сторона
        (and
          (< lat 50.3)
          (> lon 42.3)
          (< lon 47.3))
        ; От Крыма до Астрахани
        (and
          (< lat 48.4))))))
