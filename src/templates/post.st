$pagestart(pagetitle=post.title)$
$metainfoline(divclass="metainfo";post=post;categories=categories)$
<h1 class="posttitle">$post.title$</h1>
<div class="post">
  $post.full:noescape()$
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
        <p>Comment preview:</p>
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
        $commentErrors:{ <li>$it.1$</li> }$
      </ul>
      </div>
    $endif$

    <form method="post" action="#addcomment">
    $csrffield:noescape()$
      <table>
        <tr>
          <td>$nameLabel:noescape()$</td>
          <td>$nameWidget:noescape()$</td>
        </tr>
        <tr>
          <td>$emailLabel:noescape()$</td>
          <td>$emailWidget:noescape()$</td>
        </tr>
        <tr>
          <td>$formatLabel:noescape()$</td>
          <td>$formatWidget:noescape()$</td>
        </tr>
      </table>
      <div>$messageWidget:noescape()$</div>
      <div>
        <input type="submit" name="submit" value="Submit" />
        <input type="submit" name="preview" value="Preview" />
      </div>
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
    </ul>
  </div>
$endif$
$pageend()$
