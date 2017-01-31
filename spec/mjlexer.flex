package rs.ac.bg.etf.pp1;

import java_cup.runtime.Symbol;

%%

%{

	// ukljucivanje informacije o poziciji tokena
	private Symbol new_symbol(int type) {
		return new Symbol(type, yyline+1, yycolumn);
	}
	
	// ukljucivanje informacije o poziciji tokena
	private Symbol new_symbol(int type, Object value) {
		return new Symbol(type, yyline+1, yycolumn, value);
	}

%}

// Using CUP
%cup

// Track line
%line

// Track column
%column

// Add new state for comments
%xstate COMMENT

// Add EOF at the end of file
%eofval{
	return new_symbol(sym.EOF);
%eofval}

%%

// Ignore white characters
" " 		{ }
"\b" 		{ }
"\t" 		{ }
"\r\n" 		{ }
"\f" 		{ }

// Key words
"program"   { return new_symbol(sym.PROG, 		yytext()); }
"const"   	{ return new_symbol(sym.CONST, 		yytext()); }
"class"  	{ return new_symbol(sym.CLASS, 		yytext()); }
"extends"   { return new_symbol(sym.EXTENDS, 	yytext()); }
"return" 	{ return new_symbol(sym.RETURN, 	yytext()); }
"void" 		{ return new_symbol(sym.VOID, 		yytext()); }
"new" 		{ return new_symbol(sym.NEW, 		yytext()); }
"static" 	{ return new_symbol(sym.STATIC, 	yytext()); }

// Language constructs
"if"		{ return new_symbol(sym.IF, 		yytext()); }
"else"		{ return new_symbol(sym.ELSE, 		yytext()); }
"for"		{ return new_symbol(sym.FOR, 		yytext()); }
"break"		{ return new_symbol(sym.BREAK, 		yytext()); }
"continue"	{ return new_symbol(sym.CONTINUE,	yytext()); }

// Functions
"print" 	{ return new_symbol(sym.PRINT, 		yytext()); }
"read" 		{ return new_symbol(sym.READ, 		yytext()); }

// Addition Operators
"+" 		{ return new_symbol(sym.PLUS, 		yytext()); }
"-" 		{ return new_symbol(sym.MINUS, 		yytext()); }

// Multiplication Operators
"*" 		{ return new_symbol(sym.MUL, 		yytext()); }
"/" 		{ return new_symbol(sym.DIV, 		yytext()); }
"%" 		{ return new_symbol(sym.MOD, 		yytext()); }

// Unar Operators
"++"		{ return new_symbol(sym.INC, 		yytext()); }
"--"		{ return new_symbol(sym.DEC, 		yytext()); }

// Assigment Operators
"=" 		{ return new_symbol(sym.EQUAL, 			yytext()); }
"+=" 		{ return new_symbol(sym.PLUS_EQUAL, 	yytext()); }
"-=" 		{ return new_symbol(sym.MINUS_EQUAL,	yytext()); }
"*=" 		{ return new_symbol(sym.MUL_EQUAL, 		yytext()); }
"/=" 		{ return new_symbol(sym.DIV_EQUAL, 		yytext()); }
"%=" 		{ return new_symbol(sym.MOD_EQUAL, 		yytext()); }

// Logical Operators
"&&" 		{ return new_symbol(sym.AND, 		yytext()); }
"||" 		{ return new_symbol(sym.OR, 		yytext()); }

// Comperison Operator
"==" 		{ return new_symbol(sym.EQUALS, 		yytext()); }
"!=" 		{ return new_symbol(sym.NOT_EQUALS, 	yytext()); }
"<" 		{ return new_symbol(sym.LESS, 			yytext()); }
"<=" 		{ return new_symbol(sym.LESS_EQUALS,	yytext()); }
">" 		{ return new_symbol(sym.GREATER, 		yytext()); }
">=" 		{ return new_symbol(sym.GREATER_EQUALS, yytext()); }

// Braces
"{" 		{ return new_symbol(sym.LBRACE, 	yytext()); }
"}"			{ return new_symbol(sym.RBRACE, 	yytext()); }

// Brackets
"[" 		{ return new_symbol(sym.LBRACK,		yytext()); }
"]"			{ return new_symbol(sym.RBRACK, 	yytext()); }

// Parentes
"(" 		{ return new_symbol(sym.LPAREN, 	yytext()); }
")" 		{ return new_symbol(sym.RPAREN, 	yytext()); }

// Semicolom, comma and dot
";" 		{ return new_symbol(sym.SEMI, 		yytext()); }
"," 		{ return new_symbol(sym.COMMA, 		yytext()); }
"." 		{ return new_symbol(sym.DOT, 		yytext()); }

// Comment
"//" 		     { yybegin(COMMENT); }
<COMMENT> .      { yybegin(COMMENT); }
<COMMENT> "\n" 	 { yybegin(YYINITIAL); }

// Bool Constant
"true"|"false" { return new_symbol(sym.BOOL, new Boolean (yytext().equals("true") ? true : false)); }

// Integer Constant
[0-9]+  { return new_symbol(sym.NUMBER, new Integer (yytext())); }

// Identifier
([a-z]|[A-Z])[a-z|A-Z|0-9|_]* 	{ return new_symbol (sym.IDENT, yytext()); }

// Char Constant
"'"."'" { return new_symbol(sym.CHAR, new Character (yytext().charAt(1))); }

// Error
. { System.err.println("Leksicka greska ("+yytext()+") u liniji "+(yyline+1)); }






