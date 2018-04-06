package analizador_lexico;
import static analizador_lexico.Token.*;
%%
%class Lexer
%line
%column
%type Token
L=[a-zA-Z_]
D=[0-9]
white=[ \t\r\n]
COMMENT = "/*" [^*] ~"*/" | "/*" "*"+ "/" | "//".*
RESERVED = "echo" | "__halt_compiler()" | "abstract" | "and" | "array()" | "as" | "break" | "callable" | "case" | "catch" | "class" | "clone" | "const" | "continue" | "declare" | "default" | "die()" | "do" | "echo" | "else" | "elseif" | "empty()" | "enddeclare" | "endfor" | "endforeach" | "endif" | "endswitch" | "endwhile"| "eval()" | "exit()" | "extends" | "final" | "finally" | "for" | "foreach" | "function" | "global" | "goto"  | "if" | "implements" | "include" | "include_once" | "instanceof" | "insteadof" | "interface" | "isset()" | "list()"| "namespace" | "new" | "or" | "prin" | "private" | "protected" | "public" | "require" | "require_once" | "return" | "static" | "switch" | "throw" | "trait" | "try" | "unset()" | "use" | "while" | "xor" | "yield" | "__HALT_COMPILER\(\)" | "ABSTRACT" | "AND" | "ARRAY\(\)" | "AS" | "BREAK" | "CALLABLE" | "CASE" | "CATCH" | "CLASS" | "CLONE" | "CONST" | "CONTINUE" | "DECLARE" | "DEFAULT" | "DIE()" | "DO" | "ECHO" | "ELSE" | "ELSEIF" | "EMPTY()" | "ENDDECLARE" | "ENDFOR" | "ENDFOREACH" | "ENDIF" | "ENDSWITCH" | "ENDWHILE"| "EVAL()" | "EXIT()" | "EXTENDS" | "FINAL" | "FINALLY" | "FOR" | "FOREACH" | "FUNCTION" | "GLOBAL" | "GOTO"  | "IF" | "IMPLEMENTS" | "INCLUDE" | "INCLUDE_ONCE" | "INSTANCEOF" | "INSTEADOF" | "INTERFACE" | "ISSET()" | "LIST()"| "NAMESPACE" | "NEW" | "OR" | "PRIN" | "PRIVATE" | "PROTECTED" | "PUBLIC" | "REQUIRE" | "REQUIRE_ONCE" | "RETURN" | "STATIC" | "SWITCH" | "THROW" | "TRAIT" | "TRY" | "UNSET()" | "USE" | "WHILE" | "XOR" | "YIELD"
TYPE = "String" | "Integer" | "Float" | "Boolean" | "Array" | "Object" | "NULL" | "Resource"
BOOLEAN = "TRUE" | "FALSE" | "true" | "false"
PREDEFINEDVARIABLES= "$GLOBALS"|"$_SERVER"|"$_GET"|"$_POST"|"$_FILES"|"$_REQUEST"|"$_SESSION"|"$_ENV"|"$_COOKIE"|"$php_errormsg"|"$HTTP_RAW_POST_DATA"|"$http_response_header"|"$argc"|"$argv"

%{
    public String lexeme;
    public int linea;
    public int columna;
%}
%%

/* PALABRAS RESERVADAS */
{RESERVED}+ {lexeme=yytext(); return RESERVED;}
/* OPERADORES ARITMETICOS */
"+" {return PLUS;}
"-" {return MINUS;}
"*" {return TIMES;}
"/" {return DIVIDED;}
/* OPERADORES LOGICOS */
"==" {return EQUALS;}
"!" {return NOT;}
"not" {return NOT;}
"&&" {return AND;}
"and" {return AND;}
"||" {return OR;}
"or" {return OR;}
"xor" {return XOR;}
">" {return GREATER;}
"<" {return LESS;}
">=" {return GREATEROREQUAL;}
"<=" {return LESSOREQUAL;}
/* TIPOS */
{TYPE}+ {lexeme=yytext(); return TYPE;}
/* TIPOS LÓGICOS */
{BOOLEAN} {lexeme=yytext(); return BOOLEAN;}
/* TIPOS ENTEROS */
[-+]?{D}+ {lexeme=yytext(); return INT;}
/* TIPOS REALES */
[-+]?{D}+"."{D}+ {lexeme=yytext(); return REAL;}
/* TIPOS CADENA */
\"({L}({L}|{D})*|{D}({L}|{D})*|\s)*\" {lexeme=yytext(); return STRING;}
/* VARIABLES */
"$" {return VAR;}
/* CONSTANTES */
"define""(""\""{L}({L}|{D})*"\""","{white}*"\""(({L}({L}|{D})*)*|([-+]?{D}+|([-+]?{D}+"."{D}+)))*"\""")"";" {lexeme=yytext(); return CONST;}
/* IDENTIFICADORES */
{L}({L}|{D})* {lexeme=yytext(); return ID;}
/* ESTRUCTURAS DE CONTROL*/
"if"{white}?"("((("$"{L}({L}|{D})*) | (\"({L}({L}|{D})*|{D}({L}|{D})*|\s)*\") | ([-+]?{D}+) | ([-+]?{D}+"."{D}+) | ({BOOLEAN}) ){white}?("=="|">"|"<"|">="|"<="|"!="){white}?(("$"{L}({L}|{D})*) | (\"({L}({L}|{D})*|{D}({L}|{D})*|\s)*\") | ([-+]?{D}+) | ([-+]?{D}+"."{D}+) | ({BOOLEAN}) ) )* ")""{""}" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;} /* IF*/
"else"{white}?"{".*"}" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* ELSE */
"elseif"{white}?"(".*")"{white}?"{".*"}" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* ELSE IF*/
"while"{white}?"(".*")"{white}?"{".*"}" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* WHILE*/
"do"{white}?"(".*")"{white}?"{".*"}"{white}?"while""("")"";" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;} /* DO WHILE*/
"for"{white}?"(".*";".*";"")"{white}?"{""}" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* FOR */
"foreach"{white}?"(".*")"{white}?"{""}" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* FOREACH */
"break"";" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* BREAK */
"switch"{white}?"("(("$"{L}({L}|{D})*) | ([-+]?{D}+))")"{white}?"{"{white}?("case"":"{white}?(([-+]?{D}+) | ("$"{L}({L}|{D})*))":"{white}?.*";""break;")+{white}?"default"":".*"break"";""}" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* SWITCH */
"include"{white}?(\"({L}({L}|{D})*|{D}({L}|{D})*|\s)*\")".""php"";" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;}/* INCLUDE*/
"continue"{white}?([-+]?{D}+)*";" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;} /*CONTINUE*/
"return"";" {lexeme=yytext(); return ESTRUCTURA_DE_CONTROL;} /*RETURN*/
/* VARIABLES PREDEFINIDAS*/
{PREDEFINEDVARIABLES} {lexeme=yytext(); return PREDEFINEDVARIABLE;}
/* FUNCIONES*/
"function"{white}+{L}({L}|{D})* "(".*")""{".*"}" {lexeme=yytext(); return FUNCTION;}
/* CAMPOS DE ACCESO*/
"recordset""[""\""{L}({L}|{D})*"\"""]" {lexeme=yytext(); return RECORDSET;}
/* COMENTARIOS */
{COMMENT} {lexeme=yytext(); return COMMENT;}
"=" {return ASSIGN;}
"," {return COMMA;}
";" {return SEMICOLON;}
":" {return COLON;}
"\"" {return DOUBLE_QUOTE;}
"'" {return SINGLE_QUOTE;}
"(" {return OPEN_PARENTHESIS;}
")" {return CLOSE_PARENTHESIS;}
"[" {return OPEN_SQUAREBRACKET;}
"]" {return CLOSE_SQUAREBRACKET;}
"{" {return OPEN_CURLYBRACKET;}
"}" {return CLOSE_CURLYBRACKET;}
{white} {/*Ignore*/}


. {linea=yyline; columna=yycolumn;return ERROR;}


