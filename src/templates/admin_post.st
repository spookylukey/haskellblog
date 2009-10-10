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
  $post.post_formatted:noescape()$
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
      <textarea id="id_summary_raw" name="summary_raw" cols="80" rows="5">$post.summary_raw$</textarea>
    </td>
  </tr>
    <td colspan="2">
      <label for="id_text">Full post</label><br />
      <textarea id="id_post_raw" name="post_raw" cols="80" rows="30">$post.post_raw$</textarea>
    </td>
  </tr>
  <tr>
    <td><label for="id_format">Format:</label></td>
    <td>
      $formatWidget:noescape()$
    </td>
  </tr>
  <tr>
    <td><label for="id_comments_open">Open for comments:</label></td>
    <td>
      <input type="checkbox" name="comments_open" $if(post.comments_open)$checked="checked"$endif$ >
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
