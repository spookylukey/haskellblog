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
          <td><label for="id_name">Name:</label></td>
          <td><input type="text" name="name" value="$commentData.name$" id="id_name" maxlength="100" size="20" /></td>
        </tr>
        <tr>
          <td><label for="id_email">Email:</label></td>
          <td><input type="text" name="email" value="$commentData.email$" id="id_email" maxlength="320" size="20" /></td>
        </tr>
        <tr>
          <td>Format:</td>
          <td>$formatWidget:noescape()$</td>
        </tr>
      </table>

      <div><textarea name="message" id="id_message" cols="80" rows="25">$commentData.text_raw$</textarea></div>
      <div><input type="hidden" id="id_spamcheck" name="spamcheck" value="$commentExtra$">
      </div>
      <div>
        <input type="submit" name="submit" value="Submit" />
        <input type="submit" name="preview" value="Preview" />
      </div>
      <div class="note">
      <ul>
        <li>Javascript has to be on to get past my spam protection, sorry for any inconvenience.</li>
        <li>By posting, you agree that I have the right to store and reproduce your comment.</li>
        <li>I reserve the right to moderate comments.</li>
      </ul>
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
