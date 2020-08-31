#lang racket

(require "../../_lib_links/odysseus_all.rkt")
;
(provide (all-defined-out))
;
(define HOURS_SHIFT 3) ; to correct output time, regarding time of the scraping script on the remote server

(define MAX_SYMBOLS 1200)
(define MIN_SYMBOLS 200)
(define WITHIN_DAYS 7)
(define DEFAULT_PLACE #f)

(define month-abbrs
  (hash "01" "янв"
        "02" "фев"
        "03" "мар"
        "04" "апр"
        "05" "мая"
        "06" "июн"
        "07" "июл"
        "08" "авг"
        "09" "сен"
        "10" "окт"
        "11" "ноя"
        "12" "дек"))
;
(define (correct-geography-names text)
  ((change-text
    (list
      (cons "комсомольск на амуре" "Комсомольск-на-Амуре")
      (cons "калач на дону" "Калач-на-Дону")
      (cons "ростов на дону" "Ростов-на-Дону")
      (cons "петропавловск камчатский" "Петропавловск-Камчатский")
      (cons "санкт петербург" "Санкт-Петербург")
    )) (string-downcase text)))

(define LOCAL_PLACES
          (list
            "Таганрог" "Ростов_на_Дону" "Азов" "Батайск" "Шахты" "Новочеркасск" "Волгодонск"
            "Краснодар" "Сочи" "Анапа" "Новороссийск" "Туапсе" "Геленджик" "Ейск" "Тихорецк" "Кропоткин" "Кореновск" "Тимашевск" "Горячий_Ключ" "Красная_Поляна" "Адлер"
            "Ставрополь"  "Пятигорск" "Кисловодск" "Невинномысск" "Армавир" "Минеральные_Воды" "Буденновск"
            "Владикавказ" "Моздок" "Цхинвал" "Алагир" "Ардон" "Беслан"
            "Махачкала" "Грозный" "Нальчик" "Майкоп" "Сухум" "Карачаевск" "Кизляр" "Дербент"
            "Воронеж" "Волгоград" "Элиста" "Астрахань" "Миллерово" "Россошь"
            "Донецк" "Луганск"
            ))

(define CLUSTERS
  (hash
    'teams "cpu_teams.tree"
    'people "cpu_people.tree"
    'networks "networks_g.tree"
))

(define SPECIAL_TAGS (list "#agg " "#tgnevent" "#тгнанонс"))
;
(define MIN_MEMBER 1)
(define MAX_MEMBERS_IN_SCANNED_GROUPS 10000)

; how frequently to write to the file, when changing persistence
(define FILE_WRITE_FREQUENCY 500)

; cache directory for persistent data:
; (define CACHE_DIR "_cache")
(define CACHE_DIR "_cache")

; neo4j parameters
(define NEO4J_DIR "C:/Users/User/.Neo4jDesktop/neo4jDatabases/database-24588709-5b4b-4dea-b8ea-c622197a3c12/installation-4.0.3")
(define CSV_PATH (string-append NEO4J_DIR "/import"))
(define LOGIN "scipost")
(define PWD "scipost")
