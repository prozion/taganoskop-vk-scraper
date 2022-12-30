<!DOCTYPE html>
<html lang="ru">
  <head>
    <!-- %%(str "timestamp: " (timestamp))%% -->
    %%(define page (get-$2 (listify page-id) PAGES))%%
    %%(standard-head-part
        #:title ($ title page)
        #:description ($ description page)
        #:keywords ($ keywords page)
    )%%

    %%(head-counters)%%

  </head>
  <body>
    <div class="container">
      %%(print-header)%%
      %%(print-menu #:current-page ($ id page) #:section "меню_1")%%

      %%(str news_cards)%%

    </div>
    %%(footer)%%
    %%(counters)%%
  </body>
</html>
