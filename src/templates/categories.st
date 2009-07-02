$pagestart(pagetitle="Categories")$
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
$pageend()$
