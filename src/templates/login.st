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
      <td><label for="id_username">User name:</label></td>
      <td><input type="text" name="username" value="$loginData.username$" id="id_username" maxlength="100" size="20" /></td>
    </tr>
    <tr>
      <td><label for="id_password">Password: </label></td>
      <td><input type="password" name="password" value="$loginData.password$" id="id_password" maxlength="20" size="20" /></td>
    </tr>
  </table>
  <div>
    <input type="submit" name="login" value="Login" />
  </div>
</form>

$pageend()$
