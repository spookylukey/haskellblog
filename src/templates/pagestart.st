<!DOCTYPE HTML PUBLIC "-//W3C//DTD HTML 4.01//EN" "http://www.w3.org/TR/html4/strict.dtd">
<html lang="en">
  <head>
    <meta http-equiv="Content-Type" content="text/html; charset=utf-8">
    <link rel="alternate" type="application/atom+xml" title="All posts in this blog" href="$allpostsfeedurl$">
    $if(atomfeedurl)$
    <link rel="alternate" type="application/atom+xml" title="$if(atomfeedtitle)$$atomfeedtitle$$else$Atom feed$endif$" href="$atomfeedurl$">
    $endif$
    <link rel="StyleSheet" href="/newblog.css" type="text/css">
    <link rel="shortcut icon" href="/favicon.ico" type="image/x-icon">
    <title>
      $if(pagetitle)$
        $pagetitle$ &laquo; All Unkept
      $else$
        All Unkept
      $endif$
    </title>
    <script type="text/javascript" src="/js/jquery-1.3.2.min.js"></script>
    <script type="text/javascript">
<!--
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
    \$("#responsecommentid").val(cid.toString());
    var p = \$("#respondtocomment" + cid.toString()).offset();
    \$("#responseinput").val("");
    \$("#responsebox").css('left', p.left).css('top', p.top + 15).show();
}

function deleteComment(cid, event) {
    \$.post("/blog/admin/ajax/commentdelete/", { id: cid },
                    function(data) {
                      \$("#commentdiv" + cid.toString()).remove();
                    });
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
    \$("#responsebox li").each(function() {
                    var self = \$(this);
                    self.click(function(ev) {
                       \$("#responseinput").val(self.text());
                       \$("#responsesubmit").click();
                    })
    });
    \$("#responsesubmit").click(function(ev) {
                    var cid = \$("#responsecommentid").val();
                    \$.post("/blog/admin/ajax/commentresponse/",
                      { id: cid,
                        response: \$("#responseinput").val()
                      },
                      function(data) {
                         \$("#commentdiv" + cid.toString()).find(".response").html('<div class="responseinner">' + data + ' <span class="responsename">&mdash; luke<\\/span><\\/div>');
                      }
                    )
                    \$("#responsebox").hide();
                    });

    \$("#responsecancel").click(function(ev) {
                    \$("#responsebox").hide();
                    });


}

\$(document).ready(function() {
                    if (loggedIn()) {
                        addCommentModeration();
                        \$('#id_loginlink').hide();
                    } else {
                        \$('#id_logoutlink').hide();
                    }
                    \$(".togglebar").toggle(function(ev) {
                                               \$(this.parentNode).addClass("manuallyShown");
                                            },
                                            function(ev) {
                                               \$(this.parentNode).removeClass("manuallyShown");
                                            });

});
//-->
    </script>
  </head>
  <body>
<!--[if lte IE 6]>
   <div id="ie-message">
      Your browser is obsolete, and doesn't display this site correctly. Please
      upgrade to <a href="http://abetterbrowser.org/">a better browser</a>.
   </div>
<![endif]-->
    <div id="container">
      <div id="toplinks">
        <div id="toplinksinner">
          <p>Blog links</p>
          <ul>
            <li><a href="/blog/">Posts</a></li>
            <li><a href="/blog/categories/">Categories</a></li>
            <li><a href="/blog/comments/">Recent comments</a></li>
          </ul>
          <p>Site links</p>
          <ul>
            <li><a href="/">Home</a></li>
            <li><a href="/softprojects.html">Software</a></li>
            <li><a href="/personal.html">About me</a></li>
          </ul>
        </div>
      </div>
      <div id="maintitle">
        <div>All Unkept</div>
      </div>
      <div id="content">
        <div id="contentinner">
