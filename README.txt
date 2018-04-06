FUNCIONAMIENTO:
ANTES DE ABRIR EL PROYECTO, CAMBIAR LA VARIABLE PATH DENTRO DE LA CLASE MiniPHP por la ruta que usted desee.
1. abrir el proyecto en netbeans
2. hacer click derecho en clase Analizador_lexico.java y click en correr o run
3. la clase Lexer.java sera compilada y generada.
4. correr el ejecutable MiniPHP
5. subir un archivo .php
6. el programa empezará a analizar  el archivo en busca de errores
7. en pantalla se podra ver los tokens reconocidos
8. si no hay errores, se generará un archivo .out con el archivo de PHP
9. en caso de error, se generará un archivo llamado "errores_encontrados.txt" con los respectivos errores

Creo que mi programa funciona correctamente ya que analiza todo tipo de tokens que se puedan encontrar en el lenguaje PHP, gracias a las diversas expresiones regulares utilizadas que abarcan los casos necesarios para descartar errores dentro de los archivos que se le ingresen. De igual manera, en caso de encontrar errores mi programa si los almacena y los escribe en un archivo por aparte. En el caso de ser un archivo php reconocido, mi programa corrige los nombres de campos de base de datos por mayusculas al igual que las estructuras de control.

En caso de encontrar errores, el programa los almacena uno por uno dentro de una lista, guardando el numero de linea y de columna en donde fue encontrado.