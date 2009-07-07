$pagestart(pagetitle=post.title)$
<h1 class="posttitle">$post.title$</h1>
$metainfoline(divclass="metainfo";post=post;categories=categories)$
<div class="post">
  $post.full$
</div>

<div class="comments">
  <h1 id="comments">Comments <a class="anchor" href="#comments">&sect;</a></h1>
  $if(hasComments)$
    <div>
    $comments:{ $comment(comment=it)$ }$
    </div>
  $else$
    <p>No comments.</p>
  $endif$
</div>

$if(post.commentsOpen)$
  <div id="addcomment">
    <h1>Add comment</h1>
    $if(commentPreview)$
      <div class="commentpreview">
        $comment(comment=newComment)$
      </div>
    $endif$

    $if(commentAccepted)$
      <div class="accepted">
        Comment added, thank you.
      </div>
    $endif$

    $if(commentInvalid)$
      <div class="validationerror">
      <ul>
        $commentErrors:{ <li>$it.message$</li> }$
      </ul>
      </div>
    $endif$

    <form method="post" action="#addcomment">
      <table>
        <tr>
          <td>$nameLabel$</td>
          <td>$nameWidget$</td>
        </tr>
        <tr>
          <td>$emailLabel$</td>
          <td>$emailWidget$</td>
        </tr>
        <tr>
          <td>$formatLabel$</td>
          <td>$formatWidget$</td>
        </tr>
      </table>
      $messageWidget$
      <br>
      <input type="submit" name="submit" value="Submit" />
      <input type="submit" name="preview" value="Preview" />

    </form>
  </div>
$else$
  <hr />
  <p>Closed for comments.</p>
$endif$

$if(hasRelated)$
  <div class="related">
    <h1 id="related">Related: <a class="anchor" href="#related">&sect;</a></h1>
    <ul>
    $related:{
      <li><a href="$it.url$">$it.title$</a></li>
    }$
  </div>
$endif$
$pageend()$
