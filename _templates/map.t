<!DOCTYPE html>
<html lang="ru">
  <head>
    <!-- %%(str "timestamp: " (timestamp))%% -->
    %%(define page (@id page-id PAGES))%%
    %%(standard-head-part
        #:title ($ title page)
        #:description ($ description page)
        #:keywords ($ keywords page)
    )%%

    %%(head-counters)%%

    %%(define filtered-posts (filter-posts
                            (posts)
                            #:entities items
                            #:trigger-expression '(++ urban_1 urban_1_action urban_2 urban_2_action urban_3 urban_3_action cpu local_economy new_settlements)
                            #:within-days WITHIN_DAYS
                            #:min-symbols MIN_SYMBOLS))%%


    %%(define extended-posts (for/fold
                      ((res empty))
                      ((post filtered-posts))
                      (let* ((coors (get-coors-of-the-place ($ place post) PLACES_COORS)))
                        (cond
                          ((not coors)
                            res
                            )
                          (else
                            (pushr res
                                  (hash-union
                                    (hash 'coors coors)
                                    post)))))))%%

    %%(define posts_js
      (for/fold
        ((res ""))
        ((post extended-posts))
        (let* (
              (lat ($ coors.lat post))
              (lon ($ coors.lon post))
              (p (make-output-post-parameters post))
              (only-location? (= 1 (length
                                      (filter (λ (pp) (equal? ($ place p) ($ place pp))) extended-posts)))))
          (str res
                (if (empty-string? res) "" ",\n") ; first element in js array starts without comma in front
                (format
                  "{name:\"~a\",
                    date:\"~a\",
                    day:\"~a\",
                    month:\"~a\",
                    month_abbr:\"~a\",
                    place:\"~a\",
                    image:\"~a\",
                    url:\"~a\",
                    url_html:\"~a\",
                    text:\"~a\",
                    _new:\"~a\",
                    lat:~a, lon:~a}"
                  ($ name p)
                  ($ date p)
                  ($ day p)
                  ($ month p)
                  ($ month_abbr p)
                  (get-place p)
                  ($ image p)
                  ($ url p)
                  ($ url_html p)
                  ($ text p)
                  ($ _new p)
                  (if only-location? lat (microshift-location lat #:mode 'lat))
                  (if only-location? lon (microshift-location lon #:mode 'lon))
                  )))))%%

    <!-- yandex maps -->
    <script src="https://api-maps.yandex.ru/2.1/?apikey=6a630879-a6b4-488c-afdd-fc34caf93443&lang=ru_RU" type="text/javascript"></script>
    <script>

      function get_icon_type(month) {
          switch(month) {
            case "01":  return "islands#darkBlueStretchyIcon";
            case "02":  return "islands#blueStretchyIcon";
            case "03":  return "islands#lightBlueStretchyIcon";
            case "04":  return "islands#yellowStretchyIcon";
            case "05":  return "islands#greenStretchyIcon";
            case "06":  return "islands#darkGreenStretchyIcon";
            case "07":  return "islands#redStretchyIcon";
            case "08":  return "islands#oliveStretchyIcon";
            case "09":  return "islands#darkOrangeStretchyIcon";
            case "10":  return "islands#orangeStretchyIcon";
            case "11":  return "islands#brownStretchyIcon";
            case "12":  return "islands#nightStretchyIcon";
            default: return "islands#grayStretchyIcon";
          }
      }

      function print_icon_content(e) {
        var open_tag = (e._new == "true")? "<b>" : "";
        var close_tag = (e._new == "true")? "</b>" : "";
        return open_tag + e.day + " " + e.month_abbr + close_tag;
      }

      ymaps.ready(init);

      function init() {

        var settings = {
                          zoom: 4,
                          center: [56.346941, 67.982402]
                        };

        var map = new ymaps.Map("map", settings);

        var posts = new Array(%%(str posts_js)%%);

        var placemarks = new Array();

        for (var i=0; i<posts.length; i++) {
          placemarks.push(
            new ymaps.Placemark(
              [posts[i].lat, posts[i].lon],
              {
                hintContent: posts[i].name,
                balloonContentHeader: posts[i].name,
                balloonContent: "<div class='balloon'>"
                                + "<div><b>" + posts[i].date + "</b> &mdash; " + posts[i].place + "</div>"
                                // + set_grey_bar(posts[i].image, posts[i].logo)
                                + set_header_image(posts[i].image)
                                + print_text(posts[i])
                                + "<b>" + print_link(posts[i]) + "</b>"
                                + "</div>",
                balloonContentFooter: "", // posts[i].type,
                iconContent: print_icon_content(posts[i])
              },
              {
                preset: get_icon_type(posts[i].month)
              }
            )
          );
        }

        for (var i=0; i<placemarks.length; i++) {
          map.geoObjects.add(placemarks[i]);
        }

      }
    </script>
  </head>
  <body>
    <div class="container">
      %%(print-header)%%
      %%(print-menu #:current-page "Map" #:section "меню_1")%%

      <div class="map" id="map">
      </div>
    </div>

      %%(footer #:statistics (hash "Всего постов на карте" (length extended-posts)))%%
      %%(counters)%%
  </body>
</html>
