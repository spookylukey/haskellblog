$pagestart()$
<h1>$pagetitle$</h1>

$if(showErrors)$
  $displayerrors(errors=errors)$
$endif$

$if(showPreview)$
<p>Preview:</p>
<div class="postpreview">
<h1 class="posttitle">$post.title$</h1>
<div class="post">
  $post.postFormatted:noescape()$
</div>
</div>
$endif$

<form action="." method="POST">
$csrffield:noescape()$
<table>
  <tr>
    <td><label for="id_title">Title:</label></td>
    <td><input type="text" id="id_title" name="title" value="$post.title$" size=40></td>
  </tr>
  <tr>
    <td><label for="id_categories">Categories:</label></td>
    <td>
      $categoriesWidget:noescape()$
    </td>
  </tr>
  <tr>
    <td colspan="2">
      <label for="id_summary">Summary</label><br />
      <textarea id="id_summaryRaw" name="summaryRaw" cols="80" rows="5">$post.summaryRaw$</textarea>
    </td>
  </tr>
    <td colspan="2">
      <label for="id_text">Full post</label><br />
      <textarea id="id_postRaw" name="postRaw" cols="80" rows="30">$post.postRaw$</textarea>
    </td>
  </tr>
  <tr>
    <td><label for="id_format">Format:</label></td>
    <td>
      $formatWidget:noescape()$
    </td>
  </tr>
  <tr>
    <td><label for="id_commentsOpen">Open for comments:</label></td>
    <td>
      <input type="checkbox" name="commentsOpen" $if(post.commentsOpen)$checked="checked"$endif$ >
    </td>
  </tr>
</table>

<br>
<input type="submit" name="submit" value="Post"> <input type="submit" name="preview" value="Preview"><br>

$if(!isNew)$
<br><br>
<input type="submit" name="delete" value="Delete post">
$endif$

</form>

$pageend()$
