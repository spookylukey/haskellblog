<?php
  // DO NOT EDIT THE FILE syndicate.php DIRECTLY!

  // PHP file that does redirects to the new URLs.  This could be
  // implemented using a .htaccess redirect rule and a Haskell CGI
  // script that parsed and rewrote the URLs.  However, that script
  // would still need access to the mapping between old ids (not
  // stored in the new database) and the new URLs/ids.  So it is
  // simpler to do it in a single PHP file.

  // The file syndicate.php is produced from the syndicate.php.tpl file.  The
  // template language uses dollar substitutions, and dollar signs
  // must be escaped by doubling.

define('FEED_SELECTALL',1);
define('FEED_SELECTCATEGORIES',2);
define('FEED_SELECTEXCEPT',3);

define('FEED_FULLSTORY',1);
define('FEED_SHORTSTORY', 2);

$$catMap = ${categoryIdsToUrls};

@$$method = $$_GET['m']; // method of selection

@$$categories = $$_GET['c'];
$$categories = preg_replace("/[^\d,]/",'', $$categories);
$$categories = explode(',', $$categories);
if (count($$categories) > 1) {
    // don't support that any more
    header ('HTTP/1.1 404 Not Found');
    exit();
}

if ($$method == FEED_SELECTEXCEPT) {
    // don't support that any more
    header ('HTTP/1.1 404 Not Found');
    exit();
}

if ($$method == FEED_SELECTCATEGORIES) {
    header ('HTTP/1.1 301 Moved Permanently');
    header('Location: ' . $$catMap[$$categories[0]]);
    exit();
}

// default - FEED_SELECTALL

header ('HTTP/1.1 301 Moved Permanently');
header ('Location: /blog/atom/');
exit();

?>
