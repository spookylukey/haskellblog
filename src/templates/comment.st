
    <div class="comment$if(comment.isAuthor)$ author$endif$$if(comment.hidden)$ hiddenComment$endif$" id="commentdiv$comment.uid$">
      <div class="togglebar" $if(comment.hidden)$ title="Hidden comment" $endif$></div>
      <div class="introline">
        <a id="comment$comment.uid$" href="#comment$comment.uid$" class="anchor">&sect;</a> <span>On <span class="timestamp">$comment.date$</span>, <span class="commentBy">$comment.formattedName$</span> wrote:</span>
      </div>
      <div class="commenttext">
      <span class="commentid">$comment.uid$</span>
         $comment.textFormatted:noescape()$
      </div>
      <div class="response">
        $if(comment.response)$
        <div class="responseinner">$comment.response:noescape()$ <span class="responsename">&mdash; luke</span> </div>
        $endif$
      </div>
      <div class="commentControls">
        <a id="hidecomment$comment.uid$" href="#" $if(comment.hidden)$style="display:none;"$endif$>Hide</a>
        <a id="showcomment$comment.uid$" href="#" $if(!comment.hidden)$style="display:none;"$endif$>Show</a>
        <a id="respondtocomment$comment.uid$" href="#">Respond</a>
        <a id="deletecomment$comment.uid$" href="#">Delete</a>
      </div>
    </div>
    <hr/>
