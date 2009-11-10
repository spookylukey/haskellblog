        </div>
      </div>
      <div id="footer">
        <h1>Links</h1>
	<div class="bloglinks">
	  <h2>Blog links:</h2>
          <ul>
            <li><a href="/blog/">Index</a></li>
            <li><a href="/blog/categories/">Categories</a></li>
             $if(atomfeedurl)$
             <li><a href="$atomfeedurl$">$if(atomfeedtitle)$$atomfeedtitle$$else$Atom feed$endif$</a><li>
             $endif$
          </ul>
	</div>
        <div class="sitelinks">
          <h2>Also on this site:</h2>
          <ul>
            <li><a href="/" >Index</a></li>
            <li><a href="/softprojects.html">Software</a></li>
            <li><a href="/bibleverses/">Bible memorisation</a></li>
            <li><a href="/personal.html">About me</a></li>
          </ul>
        </div>
      </div>
      <div id="loginlinks">
        <a href="/blog/login/">Login</a><br/>
        <a href="/blog/logout/">Logout</a><br/>
        <a href="/blog/admin/">Admin</a><br/>
      </div>
      <div id="responsebox" style="display:none;">
        <ul>
          <li>Didn't read the post</li>
          <li>Troll!</li>
          <li>Off-topic</li>
          <li>Already answered in comments</li>
        </ul>
        Custom: <input type="text" id="responseinput" size=50> <input type="submit" id="responsesubmit" value="Send"> <input type="submit" id="responsecancel" value="Cancel">
        <input type="hidden" id="responsecommentid" value="">
      </div>
    </div>
  </body>
</html>

