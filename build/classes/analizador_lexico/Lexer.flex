package analizador_lexico;
import static analizador_lexico.Token.*;
%%
%class Lexer
%type Token
L=[a-zA-Z_]
D=[0-9]
white=[ \t\r\n]
COMMENT = "/*" [^*] ~"*/" | "/*" "*"+ "/"
RESERVED = "__halt_compiler()" | "abstract" | "and" | "array()" | "as" | "break" | "callable" | "case" | "catch" | "class" | "clone" | "const" | "continue" | "declare" | "default" | "die()" | "do" | "echo" | "else" | "elseif" | "empty()" | "enddeclare" | "endfor" | "endforeach" | "endif" | "endswitch" | "endwhile"| "eval()" | "exit()" | "extends" | "final" | "finally" | "for" | "foreach" | "function" | "global" | "goto"  | "if" | "implements" | "include" | "include_once" | "instanceof" | "insteadof" | "interface" | "isset()" | "list()"| "namespace" | "new" | "or" | "prin" | "private" | "protected" | "public" | "require" | "require_once" | "return" | "static" | "switch" | "throw" | "trait" | "try" | "unset()" | "use" | "while" | "xor" | "yield" | "__HALT_COMPILER\(\)" | "ABSTRACT" | "AND" | "ARRAY\(\)" | "AS" | "BREAK" | "CALLABLE" | "CASE" | "CATCH" | "CLASS" | "CLONE" | "CONST" | "CONTINUE" | "DECLARE" | "DEFAULT" | "DIE()" | "DO" | "ECHO" | "ELSE" | "ELSEIF" | "EMPTY()" | "ENDDECLARE" | "ENDFOR" | "ENDFOREACH" | "ENDIF" | "ENDSWITCH" | "ENDWHILE"| "EVAL()" | "EXIT()" | "EXTENDS" | "FINAL" | "FINALLY" | "FOR" | "FOREACH" | "FUNCTION" | "GLOBAL" | "GOTO"  | "IF" | "IMPLEMENTS" | "INCLUDE" | "INCLUDE_ONCE" | "INSTANCEOF" | "INSTEADOF" | "INTERFACE" | "ISSET()" | "LIST()"| "NAMESPACE" | "NEW" | "OR" | "PRIN" | "PRIVATE" | "PROTECTED" | "PUBLIC" | "REQUIRE" | "REQUIRE_ONCE" | "RETURN" | "STATIC" | "SWITCH" | "THROW" | "TRAIT" | "TRY" | "UNSET()" | "USE" | "WHILE" | "XOR" | "YIELD"
TYPE = "String" | "Integer" | "Float" | "Boolean" | "Array" | "Object" | "NULL" | "Resource"

%{
    public String lexeme;
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
"true" | "TRUE" {lexeme=yytext(); return TYPE;}
"false" | "FALSE" {lexeme=yytext(); return TYPE;}
/* TIPOS ENTEROS */
[-+]?{D}+ {lexeme=yytext(); return INT;}
/* TIPOS REALES */
[-+]?{D}+"."{D}+ {lexeme=yytext(); return REAL;}
/* TIPOS CADENA */
\"({L}({L}|{D})*|{D}({L}|{D})*|\s)*\" {lexeme=yytext(); return STRING;}
/* VARIABLES */
"$" {return VAR;}
/* CONSTANTES */
"define""(""\"""\""",""\"""\""")"";" {lexeme=yytext(); return CONST;}
/*"define(\"" ({L}({L}|{D})*) "\",\s\"" ({L}({L}|{D})*) | [-+]?{D}+ "\"\);"*/
/* IDENTIFICADORES */
{L}({L}|{D})* {lexeme=yytext(); return ID;}



"=" {return ASSIGN;}

"," {return COMMA;}
";" {return SEMICOLON;}
"\"" {return DOUBLE_QUOTE;}
"'" {return SINGLE_QUOTE;}
"(" {return OPEN_PARENTHESIS;}
")" {return CLOSE_PARENTHESIS;}
{white} {/*Ignore*/}
"//".* {/*Ignore*/}
{COMMENT} {/*Ignore*/}

. {return ERROR;}


