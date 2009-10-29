<?php
  // DO NOT EDIT THE FILE blog.php DIRECTLY!

  // PHP file that does redirects to the new URLs.  This could be
  // implemented using a .htaccess redirect rule and a Haskell CGI
  // script that parsed and rewrote the URLs.  However, that script
  // would still need access to the mapping between old ids (not
  // stored in the new database) and the new URLs/ids.  So it is
  // simpler to do it in a single PHP file.

  // The file blog.php is produced from the blog.php.tpl file.  The
  // template language uses dollar substitutions, and dollar signs
  // must be escaped by doubling.

$$postMap = ${postIdsToUrls};

$$catMap = ${categoryIdsToUrls};

header ('HTTP/1.1 301 Moved Permanently');

$$id = $$_GET['id'];
$$cat = $$_GET['cat'];

if (!empty($$id)) {
   header('Location: ' . $$postMap[$$id]);
   exit();
}

if (!empty($$cat)) {
   header('Location: ' . $$catMap[$$cat]);
   exit();
}

header('Location: /blog/')
?>
