$pagestart(pagetitle="Edit categories")$
<h1>Edit categories</h1>
<script language="javascript">

function showEditControls() {
    \$("#id_editcontrols").show();
    \$("#id_edit").attr("disabled", "disabled");
    \$("#id_new").attr("disabled", "disabled");
    \$("#id_delete").attr("disabled", "disabled");
    \$("#id_categories").attr("disabled", "disabled");
}

function hideEditControls() {
    \$("#id_editcontrols").hide();
    \$("#id_edit").removeAttr("disabled");
    \$("#id_new").removeAttr("disabled");
    \$("#id_delete").removeAttr("disabled");
    \$("#id_categories").removeAttr("disabled");
}

function editBtnClick(event) {
    event.preventDefault();
    var val = \$("#id_categories").val();
    if (val != null) {
        \$("#id_catid").val(val);
        \$("#id_name").val(\$("#id_categories option:selected").text());
        showEditControls();
    }
}

function cancelBtnClick(event) {
    event.preventDefault();
    hideEditControls();
}

function newBtnClick(event) {
    event.preventDefault();
    \$("#id_name").val("");
    \$("#id_catid").val("");
    showEditControls();
}

\$(document).ready(function() {
                       \$("#id_edit").bind("click", editBtnClick);
                       \$("#id_cancel").bind("click", cancelBtnClick);
                       \$("#id_new").bind("click", newBtnClick);
                     });
</script>
$if(showMessage)$
<div class="message">$message$</div>
$endif$


<form method="post" action="">
  $csrffield:noescape()$
  <div style="float: left; margin-right: 1em;">
    <select name="categories" id="id_categories" size="20">
      $categories:{
        <option value="$it.uid$">$it.name$</option>
      }$
    </select>
  </div>
  <div style="float: left; margin-right: 1em; ">
   <form method="post" action="">
   <input type="submit" id="id_new" value="New"><br><br>
   <input type="submit" id="id_edit" value="Edit"><br><br>
   <input type="submit" id="id_delete" name="delete" value="Delete"><br>
  </div>
  <div style="float: left; display:none; padding-left: 1em; border-left: 1px solid black" id="id_editcontrols">
    <label>Name: <input type="text" id="id_name" name="name" value=""><label><br><br>
    <input type="hidden" id="id_catid" name="catid" value="">
    <input type="submit" id="id_save" name="save" value="Save">&nbsp;
    <input type="submit" id="id_cancel" value="Cancel"><br>
  </div>
  <br style="clear:left;">

</form>

$pageend()$
