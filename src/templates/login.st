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
  <table>
    <tr>
      <td>$usernameLabel$</td>
      <td>$usernameWidget$</td>
    </tr>
    <tr>
      <td>$passwordLabel$</td>
      <td>$passwordWidget$</td>
    </tr>
  </table>
  <div>
    <input type="submit" name="login" value="Login" />
  </div>
</form>

$pageend()$