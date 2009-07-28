
    <div class="comment$if(comment.isAuthor)$ author$endif$">
      <div class="introline">
        <span>On <span class="timestamp">$comment.date$</span>, <span class="commentBy">$comment.formattedName$</span> wrote:</span>
      </div>
      <div class="commenttext">
         $comment.textFormatted:noescape()$
      </div>
    </div>
    <hr/>
