<!-- Comunicação php-R -->

<!DOCTYPE html>
<html>
<head>
	<title>R-PHP</title>
</head>
<body>
<?php
// Criando form básico para enviar dados ao servidor R
echo "<form action='r-php.php' method='get'>";    
echo "Valor: <input type='text' name='valor'/>";    
echo "<input type='submit' value = 'OK'/>";    
echo "</form>";     

if( isset($_GET['valor']))    
{
  $valor = $_GET['valor'];

  // executa o script R    
  // Autorização chown necessária Rscript e pasta
  exec("Rscript exemplo.R $valor");

  $nocache = rand();

  echo("<img src='figura.png?$nocache'/> ");    
}    
?>
</body>
</html>

