$pagestart(pagetitle="Login")$
<h1>Login</h1>
$if(loginInvalid)$
  <div class="validationerror">
     <ul>
       $loginErrors:{
         <li>$it.1$</it>
       }$
     </ul>
  </div>
$endif$

<form method="post" action="">
  $csrffield:noescape()$
  <table>
    <tr>
      <td>$usernameLabel:noescape()$</td>
      <td>$usernameWidget:noescape()$</td>
    </tr>
    <tr>
      <td>$passwordLabel:noescape()$</td>
      <td>$passwordWidget:noescape()$</td>
    </tr>
  </table>
  <div>
    <input type="submit" name="login" value="Login" />
  </div>
</form>

$pageend()$
