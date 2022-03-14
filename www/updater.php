<?php

function load_page($filename, $writedir) {
  // $digitalocean_url = "http://188.166.159.222/pages/taganoskop/";
  // $deneb_url = "http://5.101.50.163/taganoskop/";
  $deneb_url = "http://taganoskop.denis-shirshov.ru/";
  $handle = fopen($deneb_url . $filename, "r");
  $content = stream_get_contents($handle);
  file_put_contents($writedir . $filename, $content);
  fclose($handle);
}

load_page("tgn.html", "");
// load_page("rnd.html", "");
// load_page("south.html", "");
// load_page("it.html", "");

// load_page("sitemap.xml", "");

?>
