/*
File name: parser.h
Compiler: MS Visual Studio 2012
Author: Alec Pinsent 040726224
		Josh Tate	 040713210
Course: CST 8152 – Compilers, Lab Section: 011
Assignment: 04
Date: 13/04/2014
Professor: Sv. Ranev
Purpose: to parse a given platypus source file
*/

#ifndef PARSER_H
#define PARSER_H

/* INCLUDES */
#include "buffer.h"
#include "token.h"
#include "stable.h"
#include <stdlib.h>

/* DEFINES */
#define NO_ATTR 0

/* KEYWORD INDICES */
#define ELSE 0
#define IF 1
#define INPUT 2
#define OUTPUT 3
#define PLATYPUS 4
#define REPEAT 5
#define THEN 6
#define USING 7

/* GLOBAL VARIABLES */
int synerrno;
static Token lookahead;
static Buffer* sc_buf;
extern Token mlwpar_next_token(Buffer*);
extern STD sym_table;
extern pBuffer str_LTBL;
extern int line;
extern char * kw_table [];

/* FUNCTION PROTOTYPES */
void parser(Buffer*);
void match(int, int);
void gen_incode(const char*);
void syn_eh(int);

void program();
void opt_statements();
void statements();
void statement();


/* statements */
void assignment_statement();
void selection_statement();
void iteration_statement();
void input_statement();
void output_statement();
void syn_printe();


/* expressions/lists */
void assignment_expression();

void opt_variable_list();
void variable_list();
void variable_list_recursive();

void arithmetic_expression();
void primary_arithmetic_expression();
void additive_arithmetic_expression();
void additive_arithmetic_expression_recursive();
void multiplicative_arithmetic_expression();
void multiplicative_arithmetic_expression_recursive();

void string_expression();
void string_expression_recursive();
void primary_string_expression();

void conditional_expression();
void logical_OR_expression();
void logical_OR_expression_recursive();
void logical_AND_expression();
void logical_AND_expression_recursive();
void relational_expression();
void primary_a_relational_expression();
void primary_s_relational_expression();


/* identifiers */
void variable_identifier();


#endif
