#lang racket

(require "../../_lib_links/odysseus_all.rkt")
(require "../../_lib_links/odysseus_tabtree.rkt")
(require racket/serialize)
(require compatibility/defmacro)
(require (rename-in racket/hash (hash-union hash-union-racket)))

(require "../_lib/functions.rkt")
(require "../_lib/scraping_vk.rkt")
(require "../_lib/globals.rkt")

(define-macro (merge-h h1 h2 h-new)
  `(begin
      (persistent ,h1)
      (persistent ,h2)
      (persistent ,h-new)
      (void
        (,h-new
          (hash-union (,h1) (,h2))))
      #t))

; (merge-h h-gid-uids h-gid-uids-new h-gid-uids_)

(merge-h h-user-wall-posts-new h-user-wall-posts h-user-wall-posts_)

; (persistent h-uid-posts-1)
; (persistent h-uid-posts)
;
; (--- (length (intersect (hash-keys (h-uid-posts-1)) (hash-keys (h-uid-posts-2)))))
