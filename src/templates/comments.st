$pagestart(pagetitle="Recent comments")$
<h1>Recent comments</h1>
<div class="comments">
$comments,urls,titles:{ c,u,t |
<div><a href="$u$">$t$</a></div> $comment(comment=c)$ }$
</div>
$paginglinks:noescape()$
$pageend()$
