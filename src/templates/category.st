$pagestart(pagetitle=category.name)$
<h1>Category: $category.name$</h1>

$posts,categories:{ p,cs |
<div class="summarylink"><a href="$p.url$">$p.title$</a></div>
$metainfoline(divclass="metainfoindex";post=p;categories=cs)$
<div class="summary">$p.summary$</div>
}$
$paginglinks$
$pageend()$