/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package analizador_lexico;

/**
 *
 * @author SERGIO-PC
 */
public enum Token {
    ASSIGN, EQUALS, PLUS, TIMES, MINUS,DIVIDED, NOT, ID, RESERVED, TYPE, VAR, INT, ERROR, COMMA, SEMICOLON, SINGLE_QUOTE, DOUBLE_QUOTE, STRING, GREATEROREQUAL, LESSOREQUAL, GREATER, LESS, REAL,
    OPEN_PARENTHESIS, CLOSE_PARENTHESIS, ESTRUCTURA_DE_CONTROL,BOOLEAN, PREDEFINEDVARIABLE, OPEN_SQUAREBRACKET, CLOSE_SQUAREBRACKET, OPEN_CURLYBRACKET, CLOSE_CURLYBRACKET, RECORDSET, COMMENT, COLON,
    __HALT_COMPILER, ABSTRACT, AND, ARRAY, AS,BREAK, CALLABLE, 
    CASE, CATCH, CLASS, CLONE, CONST, CONTINUE, DECLARE, DEFAULT, 
    DIE, DO, ECHO, ELSE, ELSEIF, EMPTY, ENDDECLARE, ENDFOR, 
    ENDFOREACH, ENDIF, ENDSWITCH, ENDWHILE, EVAL, EXIT, EXTENDS, 
    FINAL, FINALLY, FOR, FOREACH, FUNCTION, GLOBAL, GOTO, IF, 
    IMPLEMENTS, INCLUDE, INCLUDE_ONCE, INSTANCEOF, INSTEADOF, 
    INTERFACE, ISSET, LIST, NAMESPACE, NEW, OR, PRIN, PRIVATE, 
    PROTECTED, PUBLIC, REQUIRE, REQUIRE_ONCE, RETURN, STATIC, 
    SWITCH, THROW, TRAIT, TRY, UNSET, USE, WHILE, XOR, YIELD; 
} 