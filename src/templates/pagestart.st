<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html lang="en">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    $if(atomfeedurl)$
    <link rel="alternate" type="application/atom+xml" title="$if(atomfeedtitle)$$atomfeedtitle$$else$Atom feed$endif$" href="$atomfeedurl$">
    $endif$
    <link rel="StyleSheet" href="/newblog.css" type="text/css">
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon">
    <!--[if lte IE 6]><link rel="stylesheet" href="/newblog_IE6.css" type="text/css"><![endif]-->
    <title>
      $if(pagetitle)$
        $pagetitle$ « All Unkept
      $else$
        All Unkept
      $endif$
    </title>
    <script language="javascript" src="/js/jquery-1.3.2.min.js"></script>
    <script language="javascript">
      \$(document).ready(function() {
      \$('#id_spamcheck').attr("name", "timestamp");
      });

function readCookie(name) {
    var nameEQ = name + "=";
    var ca = document.cookie.split(';');
    for(var i=0;i < ca.length; i++) {
        var c = ca[i];
	while (c.charAt(0)==' ') c = c.substring(1,c.length);
	if (c.indexOf(nameEQ) == 0) return c.substring(nameEQ.length,c.length);
    }
    return null;
}

function loggedIn() {
    return (!(readCookie("username") == null));
}


function setCommentVisible(cid, visible) {
    \$.post("/blog/admin/ajax/commentvisible/", { id: cid,
                                                  visible: (visible ? 'True' : 'False') });

}

function hideComment(cid, event) {
    setCommentVisible(cid, false);
    \$("#commentdiv" + cid).addClass("hiddenComment").removeClass("manuallyShown");
    \$("#showcomment" + cid).show();
    \$("#hidecomment" + cid).hide();
}

function showComment(cid, event) {
    setCommentVisible(cid, true);
    \$("#commentdiv" + cid).removeClass("hiddenComment");
    \$("#hidecomment" + cid).show();
    \$("#showcomment" + cid).hide();
}

function respondToComment(cid, event) {

}

function deleteComment(cid, event) {

}

function addCommentModeration() {
    \$(".comment").each(function() {
                    var cid = \$(this).find(".commentid").text();
                    \$(this).find("#hidecomment" + cid).click(function(ev) {
                                                                 hideComment(cid, ev);
                                                                 ev.preventDefault();
                                                              });
                    \$(this).find("#showcomment" + cid).click(function(ev) {
                                                                 showComment(cid, ev);
                                                                 ev.preventDefault();
                                                              });
                    \$(this).find("#respondtocomment" + cid).click(function(ev) {
                                                                 respondToComment(cid, ev);
                                                                 ev.preventDefault();
                                                              });
                    \$(this).find("#deletecomment" + cid).click(function(ev) {
                                                                 deleteComment(cid, ev);
                                                                 ev.preventDefault();
                                                              });
    });
    \$(".commentControls").show();
}

\$(document).ready(function() {
                    if (loggedIn()) {
                        addCommentModeration();
                    }
                    \$(".togglebar").toggle(function(ev) {
                                               \$(this.parentNode).addClass("manuallyShown");
                                            },
                                            function(ev) {
                                               \$(this.parentNode).removeClass("manuallyShown");
                                            });

});

    </script>
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
