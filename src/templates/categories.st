<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN" "http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
    <link rel="alternate" type="application/rss+xml" title="RSS" href="/TODO" />
    <link rel="StyleSheet" href="/newblog.css" type="text/css" />
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon" />
    <!--[if lte IE 6]><link rel="stylesheet" href="/newblog_IE6.css" type="text/css" /><![endif]-->
    <title>Categories « All Unkept</title>
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
          <h1>Categories</h1>
          $if(hasCategories)$
          $categories:{ c |
          <div class="category">
            <a href="$c.1$">$c.0.name$</a>
          </div>
          }$
          $else$
          <p>No categories defined</p>
          $endif$
        </div>
      </div>
      <div id="footer">
        <h1>Links</h1>
	<div class="bloglinks">
	  <h2>Blog links:</h2>
          <ul>
            <li><a href="/blog/">Index</a></li>
            <li><a href="/blog/feeds/">Feeds</a></li>
            <li><a href="/blog/categories/">Categories</a></li>
            <li><a href="/blog/about/">About blog</a></li>
          </ul>
	</div>
        <div class="sitelinks">
          <h2>Also on this site:</h2>
          <ul>
            <li><a href="/" >Index</a></li>
            <li><a href="/softprojects.html">Software</a></li>
            <li><a href="/bibleverses/">Bible memorisation</a></li>
            <li><a href="/personal.html">About me</a></li>
          </ul>
        </div>
      </div>
    </div>
  </body>
</html>
