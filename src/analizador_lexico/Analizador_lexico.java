/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package analizador_lexico;
import java.io.File;
/**
 *
 * @author SERGIO-PC
 */
public class Analizador_lexico {

    static String PATH = "C:/Users/SERGIO-PC/Documents/NetBeansProjects/Analizador_lexico/src/analizador_lexico/Lexer.flex";
    
    /**
     * @param args the command line arguments
     */
    public static void main(String[] args) {
        // TODO code application logic here
        GenerarLexer(PATH);
    }
    
    public static void GenerarLexer(String path){
        File output = new File(path);
        jflex.Main.generate(output);
    }
}
