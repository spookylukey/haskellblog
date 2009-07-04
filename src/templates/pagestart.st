<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    $if(atomfeedurl)$
    <link rel="alternate" type="application/atom+xml"
    title="$if(atomfeedtitle)$$atomfeedtitle$$else$Atom feed$endif$"
    href="$atomfeedurl$" />
    $endif$
    <link rel="StyleSheet" href="/newblog.css" type="text/css" />
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
    <!--[if lte IE 6]><link rel="stylesheet" href="/newblog_IE6.css" type="text/css" /><![endif]-->
    <title>
      $if(pagetitle)$
        $pagetitle$ « All Unkept
      $else$
        All Unkept
      $endif$
    </title>
  </head>
  <body>
    <div id="container">
      <div id="toplinks">
        <ul>
          <li><a href="/blog/" class="first">Home</a></li>
          <li><a href="/blog/categories/">Categories</a></li>
          <li><a href="/blog/feeds/">Feeds</a></li>
          <li><a href="/blog/about/">About</a></li>
        </ul>
      </div>
      <div id="maintitle">
        <div>All Unkept</div>
      </div>
      <div id="content">
        <div id="contentinner">