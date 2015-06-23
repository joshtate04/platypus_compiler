/*
File name: scanner.c
Compiler: MS Visual Studio 2012
Authors: Alec Pinsent 040726224
		 Josh Tate	  040713210
Course: CST 8152 – Compilers, Lab Section: 011
Assignment: 02
Date: 06/11/2014
Professor: Sv. Ranev
Purpose:  Functions implementing a Lexical Analyzer (Scanner)
		  scanner_init() must be called before using the scanner.
Function list: 	scanner_init()
				mlwpar_next_token()
				get_next_state()
				char_class
				aa_func02()
				aa_func03()
				aa_func05()
				aa_func08()
				aa_func10()
				aa_func12()
				atool()
				iskeyword()
*/




/* The #define _CRT_SECURE_NO_WARNINGS should be used in MS Visual Studio projects
 * to suppress the warnings about using "unsafe" functions like fopen()
 * and standard sting library functions defined in string.h.
 * The define does not have any effect in Borland compiler projects.
 */
#define _CRT_SECURE_NO_WARNINGS

#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* string functions */
#include <limits.h>  /* integer types constants */
#include <float.h>   /* floating-point types constants */

/*#define NDEBUG        /*to suppress assert() call */
#include <assert.h>  /* assert() prototype */

/* project header files */
#include "buffer.h"
#include "token.h"
#include "table.h"
#include "stable.h"


#define DEBUG  /* for conditional processing */
#undef  DEBUG

#define ALLOC_FAIL 401
#define NULL_BUFF 402
#define ADD_C_FAIL 403

/* Global objects - variables */
/* This buffer is used as a repository for string literals.
   It is defined in platy_st.c */
extern Buffer * str_LTBL; /*String literal table */
int line; /* current line number of the source code */
extern int scerrnum;     /* defined in platy_st.c - run-time error number */
/* Local(file) global objects - variables */
static Buffer *lex_buf;/*pointer to temporary lexeme buffer*/
/* No other global variable declarations/definitiond are allowed */
extern STD sym_table;
/* scanner.c static(local) function  prototypes */ 
static int char_class(char c); /* character class function */
static int get_next_state(int, char, int *); /* state machine function */
static int iskeyword(char * kw_lexeme); /*keywords lookup functuion */
static long atool(char * lexeme); /* converts octal string to decimal value */

/*
Purpose: to initalize the scanner
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: b_isempty(), b_setmark(), b_retract_to_mark(), b_reset()
Parameters:  Buffer *
Return value: EXIT_SUCCESS
Algorithm: initalizes the buffer to be used by the scanner
*/



int scanner_init(Buffer * sc_buf) {
  	if(b_isempty(sc_buf)) return EXIT_FAILURE;/*1*/
	/* in case the buffer has been read previously  */
	b_setmark(sc_buf, 0);
	b_retract_to_mark(sc_buf);
	b_reset(str_LTBL);
	line = 1;
	return EXIT_SUCCESS;/*0*/
}

/*
Purpose: gets the next token
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strcpy(), b_getc(), isspace(), b_setmark(), b_getc_offset(), b_retract_to_mark()  
Parameters: Buffer *
Return value: Token
Algorithm: analizes the characters in order to create tokens
*/
Token mlwpar_next_token(Buffer * sc_buf) {
   Token t; /* token to return after recognition */
   unsigned char c; /* input symbol */
   int state = 0; /* initial state of the FSM */
   short lexstart;  /*start offset of a lexeme in the input buffer */
   short lexend;    /*end   offset of a lexeme in the input buffer */
   int accept = NOAS; /* type of state - initially not accepting */                                     
	/* 
	lexstart is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the first character of the current lexeme,
	which is being processed by the scanner.
	lexend is the offset from the beginning of the char buffer of the
	input buffer (sc_buf) to the last character of the current lexeme,
	which is being processed by the scanner.

	*/ 

	char next_c;
	int lex_length;
	int i;
	char* lexeme;
        
	if(sc_buf == NULL){
		t.code = ERR_T;
		strcpy(t.attribute.err_lex, "Error loading buffer");

		return t;
	}

	if(b_isempty(sc_buf)){
		t.code = SEOF_T;
		return t;
	}
        
                
	while (1){ /* endless loop broken by token returns it will generate a warning */
                
		/* GET THE NEXT SYMBOL FROM THE INPUT BUFFER */

		c = b_getc(sc_buf);

		if (isspace(c)){
			if (c == '\n' || c == '\r')
				line++;
			continue;
		}
              
		/* special cases or token driven processing */

		/*WRITE YOUR CODE FOR PROCESSING THE SPECIAL CASES HERE. 
		COMMENTS AND STRING LITERALS ARE ALSO PROCESSED HERE.*/
		
		switch (c)
		{
			/* Left brace */
			case '{':
				t.code = LBR_T;
				return t;

			/* Right brace */
			case '}':
				t.code = RBR_T;
				return t;

			/* Left parenthesis */
			case '(':
				t.code = LPR_T;
				return t;

			case ')':
			/* Right brace */
				t.code = RPR_T;
				return t;

			/* Comma brace */
			case ',':
				t.code = COM_T;
				return t;

			/* Less than or string concatenation */
			case '<':
				b_setmark(sc_buf,b_get_getc_offset(sc_buf));
				next_c = b_getc(sc_buf);
				if(next_c == '<')	/* String concatenation */{
					t.code = SCC_OP_T;
					return t;
				}
				
				/* Not-equal-to comparison */
				if (next_c == '>'){
					t.code = REL_OP_T;
					t.attribute.rel_op = NE;
					return t;
				}

				 /* Relational comparison (less than) */
				t.code = REL_OP_T;
				t.attribute.rel_op = LT;
				b_retract_to_mark(sc_buf);
				return t;

			/* Greater than */
			case '>':
				t.code = REL_OP_T;
				t.attribute.rel_op = GT;
				return t;

			/* Equal comparison or assignment */
			case '=':
				b_setmark(sc_buf,b_get_getc_offset(sc_buf));
				next_c = b_getc(sc_buf);
				if(next_c == '=') { /* Equals comparison */
					t.code = REL_OP_T;
					t.attribute.rel_op = EQ;
				}
				else {
					t.code = ASS_OP_T;
					b_retract_to_mark(sc_buf);
				}
				return t;

			/* Plus sign */
			case '+': 
				t.code = ART_OP_T;
				t.attribute.arr_op = PLUS;
				return t;

			/* Minus sign */
			case '-':
				t.code = ART_OP_T;
				t.attribute.arr_op = MINUS;
				return t;

			/* Multiplication */
			case '*':
				t.code = ART_OP_T;
				t.attribute.arr_op = MULT;
				return t;

			/* Division */
			case '/':
				t.code = ART_OP_T;
				t.attribute.arr_op = DIV;
				return t;

			/* End of statement */
			case ';':
				t.code = EOS_T;
				return t;

			/* Exclamation */
			case '!':
				b_setmark(sc_buf, b_get_getc_offset(sc_buf));
				next_c = b_getc(sc_buf);
				switch (next_c)
				{
					/* Comment */
					case '<':
						next_c = b_getc(sc_buf);

						/* Iterate to either newline or end of file */
						while(next_c != '\n' && next_c != '\r' && next_c != '\0' && next_c != SEOF){
							next_c = b_getc(sc_buf);
						}

						/* if final character is newline, no error */
						if (next_c == '\n' || next_c == '\r') {
							line++;
							break;
						}

						/* no newline, add error */
						t.code = ERR_T;
						strcpy(t.attribute.err_lex, "Invalid comment");
						return t;				

					default:
						/* Token does not match comment or not-equal-to */
						lexeme = (char*)malloc(sizeof(char)*3);
						lexeme[0] = '!';
						lexeme[1] = next_c;
						lexeme[2] = '\0';
						t = aa_table[ES](lexeme);
						free(lexeme);

						next_c = b_getc(sc_buf);
						while(next_c != '\r' && next_c != '\n'){
							if (next_c == SEOF || next_c == -1 || next_c == '\0'){
								b_setmark(sc_buf,b_get_getc_offset(sc_buf)-1);
								b_retract_to_mark(sc_buf);
								break;
							}
							next_c = b_getc(sc_buf);
						}
						line++;

						return t;
				}
				break;

			/* Logical AND / OR */
			case '.':
				lexstart = b_get_getc_offset(sc_buf);

				next_c = b_getc(sc_buf);
				/*check for and*/
				if (next_c == 'A') {
					next_c = b_getc(sc_buf);

					if (next_c == 'N') {
						next_c = b_getc(sc_buf);

						if (next_c == 'D') {
							next_c = b_getc(sc_buf);

							if (next_c == '.') {
								t.code = LOG_OP_T;
								t.attribute.log_op = AND;
								return t;
							}
						}
					}
				}
				/*check for or*/
				else if (next_c == 'O') {
					next_c = b_getc(sc_buf);

					if (next_c == 'R') {
						next_c = b_getc(sc_buf);

						if (next_c == '.'){
							t.code = LOG_OP_T;
							t.attribute.log_op = OR;
							return t;
						}
					}
				}

				
				b_setmark(sc_buf,lexstart); 
				b_retract_to_mark(sc_buf); 
				return aa_table[ES](".");
				break;

			/* String literal */
			case '"':
				t.attribute.str_offset = b_getsize(str_LTBL);
				/*set the start of the lexeme*/
				lexstart = b_get_getc_offset(sc_buf);
				/*search for the end quote*/
				next_c = b_getc(sc_buf);
				while(next_c != '"'){

					/* if SEOF is reached before the end quote
					   then build a lexeme and pass to the error function */
					if (next_c == SEOF || next_c == '\0' || next_c == -1){
						lexend = b_get_getc_offset(sc_buf);
						lex_length = lexend - lexstart-1;

						b_setmark(sc_buf, lexstart-1);
						b_retract_to_mark(sc_buf);

						lexeme = (char*)malloc(sizeof(char)*(lex_length + 1));

						for (i = 0; i <= lex_length; ++i){
							if (i < ERR_LEN - 3)
								lexeme[i] = b_getc(sc_buf);
							else
								lexeme[i] = '.';
						}
						t = aa_table[ES](lexeme);
						free(lexeme);
						b_setmark(sc_buf, lexend);
						b_retract_to_mark(sc_buf);
						return t;
					}
					if (next_c == '\n' || next_c == '\r')
						line++;

					next_c = b_getc(sc_buf);
				}
				/*set the end of the string literal*/
				lexend = b_get_getc_offset(sc_buf) - 1;
				lex_length = lexend - lexstart;

				/*set getc back to the start of the string*/
				b_setmark(sc_buf, lexstart);
				b_retract_to_mark(sc_buf);
				/*copy the string into a new buffer*/
				for (i = 0; i < lex_length; ++i){
					b_addc(str_LTBL, b_getc(sc_buf));
				}
				next_c = b_getc(sc_buf);
				b_addc(str_LTBL, '\0');
				b_pack(str_LTBL);

				t.code = STR_T;
				return t;

			case SEOF:
				t.code = SEOF_T;
				return t;
			case '\0':
				t.code = SEOF_T;
				return t;
			case -1:
				t.code = SEOF_T;
				return t;


			/* c is digit or letter */
			default:
				/* Process state transition table */
				
				lex_buf = b_create(100, 15, 'a');
				lexstart = b_get_getc_offset(sc_buf)-1;
				
				state = get_next_state(state, c, &accept);
				if (b_addc(lex_buf, c) == NULL){
					t.code = ERR_T;
					strcpy(t.attribute.err_lex, "BUFFER ERROR");
					b_destroy(lex_buf);
					return t;
				}
			
				//printf("%c",c);
				while (accept == NOAS) {
					next_c = b_getc(sc_buf);
					//printf("%c",next_c);
					if (b_addc(lex_buf, next_c) == NULL){
						t.code = ERR_T;
						strcpy(t.attribute.err_lex, "BUFFER ERROR");
						b_destroy(lex_buf);
						return t;
					}
					//printf("accept: %d\n",accept);
					state = get_next_state(state, next_c, &accept);
					//printf("accept: %d\n",accept);
					//printf(" %d\n",state);

				}
				//printf("DONE!!!\n");
				/* if the state requires retraction, retract by one */
				if (accept == ASWR){
					
					b_setmark(sc_buf,b_get_getc_offset(sc_buf)-1);
					b_retract_to_mark(sc_buf);
				}

				/* set the lexeme end offset, and calculate the lexeme length */
				lexend = b_get_getc_offset(sc_buf);
				lex_length = lexend - lexstart;

				
				lexeme = (char*)malloc(sizeof(char)*(lex_length+1));
				if(lexeme == NULL) { /* Failed to allocate */
					t.code = ERR_T;
					strcpy(t.attribute.err_lex, "MEM ALLOC");
					return t;
				}

				/* build the lexeme to pass */
				for(i = 0; i < lex_length; ++i){
					lexeme[i] = b_getc(lex_buf);
				}
				lexeme[lex_length] = '\0';

				//printf("final state: %d\n",state);
				t = aa_table[state](lexeme);
				
				
				free(lexeme);
				b_destroy(lex_buf);
				//printf("code: %d\n",t.code);
				return t;

		} //end of case     
   }//end of while()
}//end of mlwpar_next_token


/*
Purpose:  gets the next state
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: none
Parameters:  int, char, int * 
Return value: returns the int value for the next state
Algorithm: uses current state and next character in order to determine next state
			using the transition table in table.h
*/
int get_next_state(int state, char c, int *accept) {
	int col;
	int next;
	col = char_class(c);
	next = st_table[state][col];

	#ifdef DEBUG
	printf("Input symbol: %c Row: %d Column: %d Next: %d \n",c,state,col,next);
	#endif

		   assert(next != IS);

	#ifdef DEBUG
		if(next == IS){
		  printf("Scanner Error: Illegal state:\n");
		  printf("Input symbol: %c Row: %d Column: %d\n",c,state,col);
		  exit(1);
		}
	#endif
		
	*accept = as_table[next];

	return next;
}

/*
Purpose: gets the column number for the next character
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: isalpha(), isdigit()
Parameters:	 char
Return value: column id
Algorithm: checks the char agaisnt possible results to get the column id
*/
int char_class (char c) {
	if(isalpha(c)){return 0;}
	if (isdigit(c)){
		if (c == '0'){return 1;}
		
		if (c == '8' || c == '9'){return 2;}
		
		return 3;
	}
	if(c == '.'){return 4;}
	
	if(c == '#'){return 5;}

	return 6;
}



/* HERE YOU WRITE THE DEFINITIONS FOR YOUR ACCEPTING FUNCTIONS. 
************************************************************/
/*
Purpose: 
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: iskeyword(), strlen(), 
Parameters:  char[]
Return value: token
Algorithm: 
*/
Token aa_func02(char lexeme[]) {
	Token t;
	unsigned int i;
	int is_kw;

	/*check if lexem contains a keyword*/
	is_kw = iskeyword(lexeme);
	if (is_kw >= 0){
		t.code = KW_T;
		t.attribute.kwt_idx = is_kw;
		return t;
	}
	/*if it is not a keyword it is a VID*/
	t.code = AVID_T;
	if (strlen(lexeme) > VID_LEN){
		for (i = 0; i < VID_LEN; ++i){
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[VID_LEN] = '\0';
	}
	else {
		for (i = 0; i < strlen(lexeme); ++i){
			t.attribute.vid_lex[i] = lexeme[i];
		}
		t.attribute.vid_lex[i] = '\0';
	}
	if(*lexeme == 'i' || *lexeme == 'o' || *lexeme == 'w' || *lexeme == 'd')
		t.attribute.get_int = st_install(sym_table, t.attribute.vid_lex, 'I',line);
	else
		t.attribute.get_int = st_install(sym_table, t.attribute.vid_lex, 'F',line);
	return t;
}


/*
Purpose: 
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strlen()
Parameters:  char[]
Return value: token
Algorithm: 
*/
Token aa_func03(char lexeme[]){
	Token t;
	unsigned int i;

  
	t.code = SVID_T;
	for (i = 0; i < VID_LEN-1 && i < strlen(lexeme); ++i) {
		t.attribute.vid_lex[i] = lexeme[i];
	}
	if (i+1 < VID_LEN){
		t.attribute.vid_lex[i] = '\0';
		t.attribute.vid_lex[i-1] = '#';
	}
	else {
		t.attribute.vid_lex[VID_LEN] = '\0';
		t.attribute.vid_lex[VID_LEN-1] = '#';
	}

	t.attribute.get_int = st_install(sym_table, t.attribute.vid_lex, 'S',line);
	return t;
}

/*
Purpose: accepting function for IL
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strlen(), strcpy()
Parameters:  char[]
Return value: token
Algorithm: 
*/
Token aa_func05(char lexeme[]){
	Token t;
	long new_val;

	if (strlen(lexeme) > INL_LEN)
		return aa_table[ES](lexeme);

	t.code = INL_T;
	new_val = atol(lexeme);

	if (new_val < 0 || new_val > INL_MAX)
		return aa_table[ES](lexeme);
	
	t.code = INL_T;
	t.attribute.int_value = new_val;
	return t;
}
/*
Purpose: accepting function for FPL
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strcpy()
Parameters: char[]
Return value: token
Algorithm: 
*/
Token aa_func08(char lexeme[]){
	Token t;
	double new_value = atof(lexeme);

	if (new_value > FLT_MAX){
		return aa_table[ES](lexeme);
	}
	
	t.code = FPL_T;
	t.attribute.flt_value = (float)new_value;
	return t;
}

/*
Purpose: accepting function for OIL
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strlen(), strcpy()
Parameters: char[]
Return value: Token
Algorithm: 
*/
Token aa_func10(char lexeme[]){
	Token t;
	long new_val;


	if (strlen(lexeme) > INL_LEN+1)
		return aa_table[ES](lexeme);

	t.code = INL_T;
	new_val = atool(lexeme);

	if (new_val < 0 || new_val > INL_MAX)
		return aa_table[ES](lexeme);
	
	t.code = INL_T;
	t.attribute.int_value = new_val;

	return t;
}

/*
Purpose: accepting function for error tokens
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strlen()
Parameters: char[]
Return value: Token
Algorithm: 
*/
Token aa_func12(char lexeme[]){
	Token t;
	unsigned int i;
	t.code = ERR_T;
	for (i = 0; i < ERR_LEN && i < strlen(lexeme); ++i) {
		t.attribute.err_lex[i] = lexeme[i];
	}
	if (i < ERR_LEN)
		t.attribute.err_lex[i] = '\0';
	else
		t.attribute.err_lex[ERR_LEN] = '\0';
	return t;
}

/*
Purpose: convert a number to its octal equivilant
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strlen()
Parameters: char*
Return value: long
Algorithm: loops through the number, adding the bit equivalent of each digit
*/
long atool(char * lexeme){

	 int i, //loop control
		 x=1;
	 long result=0;

	 for(i=strlen(lexeme); i>0;i--, x *= 8)
		result += x*(lexeme[i-1] - '0');
	 
	 return result;

}

/*
Purpose: determine if the lexeme is a keyword
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Called functions: strlen()
Parameters: char *
Return value: int
Algorithm: compares the given string to the keywords in the kw_table,
		   returns keyword length on success
		   returns -1 on failure
*/
int iskeyword(char * kw_lexeme){
	unsigned int i,j,char_match;
	

	for (i = 0; i < KW_L; i++) {
		char_match = 1;
		for(j = 0; j < strlen(kw_lexeme) && j < strlen(kw_table[i]); ++j){
			if (!(kw_lexeme[j] == kw_table[i][j])){
				char_match = 0;
				break;
			}
		}
		
		if (char_match == 1){
			return i;
		}
	}

	return -1;
}

