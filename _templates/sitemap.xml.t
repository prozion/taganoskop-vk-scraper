<?xml version="1.0" encoding="UTF-8"?>

%%(define url-part
#<<URL-PART
  <url>
     <loc>http://cpu.denis-shirshov.ru/~a</loc>
     <lastmod>~a</lastmod>
     <changefreq>~a</changefreq>
  </url>
URL-PART
)%%

<urlset xmlns="http://www.sitemaps.org/schemas/sitemap/0.9">
%%(for/fold
    ((res ""))
    ((page PAGES))
    (string-append
      res
      (format (format "~a~a" url-part (if (equal? page (last PAGES)) "" "~n"))
              ($ path page)
              (or (hash-ref* (Updates) ($ id page)) "")
              (or ($ changefreq page) "monthly")
              )))%%
</urlset>
