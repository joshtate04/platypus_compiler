/*
File name: parser.c
Compiler: MS Visual Studio 2012
Author: Alec Pinsent 040726224
		Josh Tate	 040713210
Course: CST 8152 – Compilers, Lab Section: 011
Assignment: 04
Date: 13/04/2014
Professor: Sv. Ranev
Purpose: to parse a given platypus source file
Function list: parser()
			   match(int, int)
			   syn_eh(int)
			   gen_incode()
			   program()
			   syn_printe()
			   Parsing Functions
*/


#include "parser.h"

/*
Purpose: Initializes the parser
Author: Josh Tate		040713210
History/Versions: 1
Called functions: mlwpar_next_token(), program(), match(), gen_incode()
Parameters: Buffer *
Return value: void
Algorithm: gets the buffer, sets lookahead to the next token, and runs the parser
		   checks for end of file to complete the parse.
*/
void parser(Buffer* in_buf){
	sc_buf = in_buf;
	lookahead = mlwpar_next_token(sc_buf);
	program(); 
	match(SEOF_T,NO_ATTR);
	gen_incode("Source file parsed");
}

/*
Purpose: Matches lookahead to required token
Author: Josh Tate		040713210
		Alec Pinsent	040726224
History/Versions: 1
Called functions: mlwpar_next_token(), syn_eh(), syn_printe()
Parameters: int pr_token_code, int pr_token_attribute
Return value: void
Algorithm: matches the given token to the lookahead token
*/
void match(int pr_token_code, int pr_token_attribute){	
	if(pr_token_code == lookahead.code){
		switch(pr_token_code){
			case KW_T:
				if (pr_token_attribute == lookahead.attribute.kwt_idx){
					lookahead = mlwpar_next_token(sc_buf);
				}
				else {
					syn_eh(pr_token_code);
				}
				break;

			case LOG_OP_T:
				if (pr_token_attribute == lookahead.attribute.log_op) {
					lookahead = mlwpar_next_token(sc_buf);
				}
				else {
					syn_eh(pr_token_code);
				}
				break;

			case ART_OP_T:
				if (pr_token_attribute == lookahead.attribute.arr_op){
					lookahead = mlwpar_next_token(sc_buf);
				}
				else {
					syn_eh(pr_token_code);
				}
				break;

			case REL_OP_T:
				if (pr_token_attribute == lookahead.attribute.rel_op){
					lookahead = mlwpar_next_token(sc_buf);
				}
				else{
					syn_eh(pr_token_code);
				}
				break;

			default:
				if (lookahead.code == SEOF_T)
					return;
				lookahead = mlwpar_next_token(sc_buf);
				break;
		}
		if(lookahead.code == ERR_T){
			syn_printe();
			++synerrno;
			lookahead = mlwpar_next_token(sc_buf);
			if (lookahead.code == SEOF_T)
				return;
		}
		return;
	}
	syn_eh(pr_token_code);
}

/*
Purpose: Implements a simple panic mode error recover.
Author: Josh Tate		040713210
History/Versions: 1
Called functions: 
Parameters: Buffer *
Return value: void
Algorithm: loops until it finds a token matching the passed token
		   if SEOF is reached, quits instead.
*/
void syn_eh(int sync_token_code){
	syn_printe();
	synerrno++;


	do {
		lookahead = mlwpar_next_token(sc_buf);
		if(lookahead.code == sync_token_code){
			lookahead = mlwpar_next_token(sc_buf);
			return;
		}

		else if(lookahead.code == SEOF_T)
			exit(synerrno);
	}while(1);

}

/*
Purpose: prints out the given string
Author: Josh Tate		040713210
History/Versions: 1
Called functions: printf()
Parameters: const char* output
Return value: void
*/
void gen_incode(const char* output){
	printf("PLATY: %s\n",output);
}


/*
Purpose: Parses the program
Author: Josh Tate		040713210
History/Versions: 1
Called functions: match(), opt_statements()
Algorithm: matches the PLATYPUS keyword and braces
		   parses any statements inside the program
*/
void program(){
	match(KW_T, PLATYPUS);
	match(LBR_T, NO_ATTR);
	opt_statements();
	match(RBR_T, NO_ATTR);
	gen_incode("Program parsed");
}

/**************
   STATEMENTS
 **************/

/*		Author: Alec Pinsent		040726224
 *		<opt_statements> ->
 *			<statements>  |  ∈v
 *
 *		First Set { AVID_T, SVID_T,KW_T(except ELSE, REPEAT, THEN), E }
 */
void opt_statements(){
	switch(lookahead.code){
		case AVID_T:
	    case SVID_T: 
			statements();
			break;
	    case KW_T:
			/* check for PLATYPUS, ELSE, THEN, REPEAT here and in
			statements_p()*/
			if (lookahead.attribute.get_int != PLATYPUS
					&& lookahead.attribute.get_int != ELSE
					&& lookahead.attribute.get_int != THEN
					&& lookahead.attribute.get_int != REPEAT){
				statements();
				break; 
			}
	   default: /*empty string – optional statements*/ ;
			gen_incode("Opt_statements parsed");
	} 
}

/*
 *		<statements> ->
 *			<statement> <statements'>  |  <statement>
 *		<statements'> ->
 *			<statement> <statements'>  |  <statement>
 *
 *		First Set{ AVID_T, SVID_T, KW_T(except ELSE, REPEAT, THEN)}
 */
void statements(){
	switch(lookahead.code){
		case KW_T:
			switch(lookahead.attribute.kwt_idx) {
				case PLATYPUS:
				case ELSE:
				case THEN:
				case REPEAT:
					return;
			}
		case AVID_T:
		case SVID_T:
			statement();
			statements();
			break;
		default:
			return;
	}
}

/*		Author: Alec Pinsent		040726224
 *		<statement> ->
 *			<assignment statement>  |  <selection statement>  |  <iteration statement>  |
 *			<input statement>  |  <output statement>
 *
 *		First Set { AVID_T, SVID_T, IF, USING, INPUT, OUTPUT }
 */
void statement(){
	switch(lookahead.code){
		case AVID_T:
		case SVID_T:		
			assignment_statement();
			break;
		case KW_T:
			switch(lookahead.attribute.kwt_idx){
				case USING:
					iteration_statement();
					break;
				case IF:					
					selection_statement();
					break;
				case OUTPUT:
					output_statement();
					break;
				case INPUT:
					input_statement();
					break;
				default:
					syn_printe();
					break;
			}
			break;
		default:
			syn_printe();
			break;
	}
}

/*		Author: Alec Pinsent		040726224
 *		<assignment statement> ->
 *			<assignment expression>;
 *
 *		First Set {AVID_T, SVID_T]
 */
void assignment_statement(){
	assignment_expression();
	match(EOS_T,NO_ATTR);
	gen_incode("Assignment statement parsed");
}

/*		Author: Josh Tate		040713210
 *		<selection statement> ->
 *			IF (<conditional expression>) THEN <opt_statements> ELSE {<opt_statements>};
 *
 *		First Set {KW_T(only IF)}
 */
void selection_statement() {
	match(KW_T, IF);
	match(LPR_T,NO_ATTR);
	conditional_expression();
	match(RPR_T,NO_ATTR);
	match(KW_T,THEN);
	opt_statements();
	match(KW_T,ELSE);
	match(LBR_T,NO_ATTR);
	opt_statements();
	match(RBR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("IF statement parsed");
}

/*		Author: Alec Pinsent		040726224
 *		<iteration statement> ->
 *			USING (<assignment expression>, <conditional expression>, <assignment expression>)
 *			REPEAT { <opt_statements> };
 *
 *		First Set {KW_T(only USING)}
 */
void iteration_statement(){
	match(KW_T, USING);
	match(LPR_T,NO_ATTR);
	assignment_expression();
	match(COM_T,NO_ATTR);
	conditional_expression();
	match(COM_T,NO_ATTR);
	assignment_expression();
	match(RPR_T,NO_ATTR);
	match(KW_T,REPEAT);
	match(LBR_T,NO_ATTR);
	opt_statements();
	match(RBR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("USING statement parsed");
}

/*		Author: Alec Pinsent		040726224
 *		<input statement> ->
 *			INPUT (<variable list>);
 *
 *		First Set {KW_T(only input)}
 */
void input_statement(){
	match(KW_T,INPUT);
	match(LPR_T,NO_ATTR);
	variable_list();
	match(RPR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("INPUT statement parsed");
}


/*		Author: Josh Tate			040713210
 *		<output statement> ->
 *			OUTPUT (<opt_variable list>);  |  OUTPUT (STR_T);
 *
 *		First Set {KW_T(only output)}
 */
void output_statement(){
	match(KW_T,OUTPUT);
	match(LPR_T,NO_ATTR);
	opt_variable_list();
	match(RPR_T,NO_ATTR);
	match(EOS_T,NO_ATTR);
	gen_incode("OUTPUT statement parsed");
}

/*		Author: Josh Tate			040713210
 *		<opt_variable list> ->
 *			<variable list>,<variable identifier>  |  <variable identifier>  |  ε
 *
 *		First Set {AVID_T, SVID_T, STL_T, FPL_T, INL_T}
 */
void opt_variable_list(){
	switch(lookahead.code){
		/* List will start with variable */
		case SVID_T:
		case AVID_T:
			variable_list();
			break;

		/* String literal */
		case STR_T:
			match(STR_T, NO_ATTR);
			gen_incode("Output list (string literal) parsed");
			break;
		
		/* Empty list (still valid) */
		default:
			gen_incode("Output list (empty) parsed");
			break;
	}
}

/*		Author: Josh Tate			040713210
 *		<variable list> ->
 *			<variable list>,<variable identifier>  |  <variable identifier>
 *
 *		First Set {AVID_T, SVID_T, STL_T, FPL_T, INL_T}
 */
void variable_list(){
	variable_identifier();
	variable_list_recursive();
	gen_incode("Variable list parsed");
}


/*		Author: Josh Tate			040713210
 *		<variable list> ->
 *			<variable list>,<variable identifier>  |  <variable identifier>
 *
 *		First Set {, , E }
 */
void variable_list_recursive(){
	/* Check to make sure the list is continued */
	if(lookahead.code != COM_T)
		return;

	/* match and increment */
	match(COM_T,NO_ATTR);
	variable_identifier();
	variable_list_recursive();
}


/***************
   EXPRESSIONS
 ***************/

/*		Author: Josh Tate			040713210
 *		<assignment expression> ->
 *			AVID = <arithmetic expression>  |  SVID = <string expression>
 *
 *		First Set {FS -> AVID_T, SVID_T}
 */
void assignment_expression(){
	switch(lookahead.code){
		case AVID_T:
			match(AVID_T,NO_ATTR);
			match(ASS_OP_T, NO_ATTR);
			arithmetic_expression();
			gen_incode("Assignment expression (arithmetic) parsed");
			break;
		case SVID_T:
			match(SVID_T,NO_ATTR);
			match(ASS_OP_T,NO_ATTR);
			string_expression();
			gen_incode("Assignment expression (string) parsed");
			break;
		default:
			syn_printe();
			break;
	}
}


/*		Author: Josh Tate			040713210
 *		<arithmetic expression> ->
 *			<unary arithmetic expression>  |  <additive arithmetic expression>
 *
 *		First Set {ART_OP_T (only PLUS/MINUS), AVID_T, INL_T, FPL_T, LPR_T}
 */
void arithmetic_expression(){
	/* check for unary */
	switch(lookahead.code){
		case ART_OP_T:
			switch(lookahead.attribute.arr_op){
				case PLUS:
				case MINUS:
					match(ART_OP_T,lookahead.attribute.arr_op);
					primary_arithmetic_expression();
					gen_incode("Unary arithmetic expression parsed");
					break;
				default:
					syn_printe();
					break;
			}
			break;

		default:
			additive_arithmetic_expression();
			break;
	}
	gen_incode("Arithmetic expression parsed");
}

/*		Author: Alec Pinsent		040726224
 *		<additive arithmetic expression> ->
 *			<multiplicative arithmetic expression>  |
 *			<additive arithmetic expression> + <multiplicative expression>  |
 *			<additive arithmetic expression> – <multiplicative expression>
 *
 *		First Set { AVID_T, INL_T, FPL_T, LPR_T}
 */
void additive_arithmetic_expression(){
	multiplicative_arithmetic_expression();
	additive_arithmetic_expression_recursive();
}

/*		Author: Alec Pinsent		040726224
 *		<additive arithmetic expression> ->
 *			<multiplicative arithmetic expression>  |
 *			<additive arithmetic expression> + <multiplicative expression>  |
 *			<additive arithmetic expression> – <multiplicative expression>
 *
 *		First Set { +, -, E }
 */
void additive_arithmetic_expression_recursive(){
	if(lookahead.code == ART_OP_T && lookahead.attribute.arr_op != MULT && lookahead.attribute.arr_op != DIV){
		if(lookahead.attribute.arr_op == PLUS){
			match(ART_OP_T,PLUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_recursive();
		}
		else if (lookahead.attribute.arr_op == MINUS){
			match(ART_OP_T,MINUS);
			multiplicative_arithmetic_expression();
			additive_arithmetic_expression_recursive();
		}
		gen_incode("Additive arithmetic expression parsed");
	}
}

/*		Author: Alec Pinsent		040726224
 *		<multiplicative expression> ->
 *			<primary arithmetic expression>  |
 *			<multiplicative expression> * <primary arithmetic expression>  |
 *			<multiplicative expression> / <primary arithmetic expression>
 *
 *		First Set {ART_OP_T (only PLUS/MINUS), AVID_T, INL_T, FPL_T, LPR_T}
 */
void multiplicative_arithmetic_expression(){
	primary_arithmetic_expression();
	multiplicative_arithmetic_expression_recursive();
}

/*		Author: Alec Pinsent		040726224
 *		<additive arithmetic expression> ->
 *			<multiplicative arithmetic expression>  |
 *			<additive arithmetic expression> + <multiplicative expression>  |
 *			<additive arithmetic expression> – <multiplicative expression>
 *
 *		First Set { *, / , E }
 */
void multiplicative_arithmetic_expression_recursive(){
	if(lookahead.code == ART_OP_T && lookahead.attribute.arr_op != PLUS && lookahead.attribute.arr_op != MINUS){
		if(lookahead.attribute.arr_op == MULT){
			match(ART_OP_T,MULT);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_recursive();
		}
		else if (lookahead.attribute.arr_op == DIV){
			match(ART_OP_T,DIV);
			primary_arithmetic_expression();
			multiplicative_arithmetic_expression_recursive();
		}
		gen_incode("Multiplicative arithmetic expression parsed");
	}
}

/*		Author: Alec Pinsent		040726224
 *		<primary arithmetic expression> ->
 *			AVID_T  | FPL_T  | INL_T  |  (<arithmetic expression>)
 *
 *		First Set {ART_OP_T (only PLUS/MINUS), AVID_T, INL_T, FPL_T, LPR_T}
 */
void primary_arithmetic_expression(){
	switch(lookahead.code){
		case AVID_T:
		case FPL_T:
		case INL_T:
			match(lookahead.code,NO_ATTR);
			gen_incode("Primary arithmetic expression parsed");
			break;
		default:
			match(LPR_T,NO_ATTR);
			arithmetic_expression();
			match(RPR_T,NO_ATTR);
			gen_incode("Primary arithmetic expression parsed");
			break;
	}
}

/*		Author: Josh Tate		040713210
 *		<string expression> ->
 *			<primary string expression>  |  <string expression> << <primary string expression>
 *
 *		First Set {SVID_T, STRL_T}
 */
void string_expression(){
	primary_string_expression();
	string_expression_recursive();
	gen_incode("String expression parsed");
}

/*		Author: Josh Tate		040713210
 *		<string expression> ->
 *			<primary string expression>  |  <string expression> << <primary string expression>
 *
 *		First Set { <>, E }
 */
void string_expression_recursive(){
	if(lookahead.code == SCC_OP_T){
		match(SCC_OP_T,NO_ATTR);
		primary_string_expression();
		string_expression_recursive();
	}
}

/*		Author Josh Tate		040713210
 *		<primary string expression> ->
 *			SVID_T  |  STR_T
 *
 *		First Set { SVID_T, STR_T }
 */
void primary_string_expression(){
	if (lookahead.code == STR_T || lookahead.code == SVID_T)
		match(lookahead.code,NO_ATTR);
	else
		syn_printe();

	gen_incode("Primary string expression parsed");
}

/*		Author: Josh Tate		040713210
 *		<conditional expression> ->
 *			<logical OR  expression>
 *
 *		First set {AVID_T, SVID_T, STRL_T, FPL_T, INL_T}
 */
void conditional_expression(){
	logical_OR_expression();
	gen_incode("Conditional expression parsed");
}

/*		Author: Josh Tate		040713210
 *		<logical  OR expression> ->
 *			<logical AND expression>  |  
 *			<logical AND expression>  .OR.  <logical OR expression>
 *
 *		First Set {AVID_T, FPL_T, INL_T, SVID_T, STR_T  }
 */
void logical_OR_expression(){
	logical_AND_expression();
	logical_OR_expression_recursive();
}

/*		Author: Josh Tate		040713210
 *		<logical  OR expression> ->
 *			<logical AND expression>  |  
 *			<logical AND expression>  .OR.  <logical OR expression>
 *
 *		First Set { .OR.,  E }
 */
void logical_OR_expression_recursive(){
	if (lookahead.code != LOG_OP_T || lookahead.attribute.log_op != OR)
		return;

	match(LOG_OP_T,OR);
	logical_AND_expression();
	logical_OR_expression_recursive();
	gen_incode("Logical OR expression parsed");
}

/*		Author: Alec Pinsent		040726224
 *		<relational expression>  |  
 *			<relational expression>  .AND.  <logical AND expression>
 *
 *		First Set { AVID_T, FPL_T, INL_T, SVID_T, STR_T }
 */
void logical_AND_expression(){
	relational_expression();
	logical_AND_expression_recursive();
}

/*		Author: Alec Pinsent		040726224
 *		<relational expression>  |  
 *			<relational expression>  .AND.  <logical AND expression>
 *
 *		First Set { .AND. , E }
 */
void logical_AND_expression_recursive(){
	if (lookahead.code != LOG_OP_T || lookahead.attribute.log_op != AND)
		return;

	match(LOG_OP_T,AND);
	relational_expression();
	logical_AND_expression_recursive();
	gen_incode("Logical AND expression parsed");
}


/*		Author: Josh Tate		040713210
 *		<relational expression> ->
 *			<primary a_relational expression>  ==  <primary a_relational expression>  |
 *			<primary a_relational  expression>  <>  <primary a_relational  expression>  |
 *			<primary a_relational  expression>  >   <primary a_relational  expression>  |
 *			<primary a_relational expression>  <   <primary a_relational expression>  |
 *			<primary s_relational expression>  ==  <primary s_relational expression>  |
 *			<primary s_relational  expression>  <>  <primary s_relational  expression>  |
 *			<primary s_relational  expression>  >   <primary s_relational  expression>  |
 *			<primary s_relational expression>  <   <primary s_relational expression>
 *
 *		First Set { AVID_T, FPL_T, INL_T, SVID_T, STR_T  }
 */
 void relational_expression(){
	switch(lookahead.code){
		/* a_relational */
		case AVID_T:
		case FPL_T:
		case INL_T:
			primary_a_relational_expression();
			match(REL_OP_T,lookahead.attribute.rel_op);
			primary_a_relational_expression();
			break;

		case SVID_T:
		case STR_T:
			primary_s_relational_expression();
			match(REL_OP_T,lookahead.attribute.rel_op);
			primary_s_relational_expression();
			break;
		/* s_relational */
		default:
			syn_printe();
			break;
	}
	gen_incode("Relational expression parsed");
}

/*		Author: Alec Pinsent		040726224
 *		<primary a_relational expression> ->
 *			AVID_T  |  FPL_T  |  INL_T
 *
 *		First Set {AVID_T, FPL_T, INL_T}
 */
void primary_a_relational_expression(){
	switch(lookahead.code){
		case AVID_T:
		case FPL_T:
		case INL_T:
			match(lookahead.code,NO_ATTR);
			break;
		default:
			syn_printe();
			break;
	}
	gen_incode("Primary a_relational expression parsed");
}

/*		Author: Alec Pinsent		040726224
 *		<primary s_relational expression> ->
 *			<primary string expression>
 *
 *		First Set {SVID_T, STRL_T}
 */
void primary_s_relational_expression(){
	primary_string_expression();
	gen_incode("Primary s_relational expression parsed");
}

/**************
   IDENTIFIER  
 **************/
void variable_identifier(){
	switch(lookahead.code){
		case SVID_T:
		case AVID_T:
			match(lookahead.code,NO_ATTR);
			break;
		default:
			syn_printe();
			break;
	}
}




/* Parser error printing function, Assignmet 4, F14
 * Provided by Prof: Svillen Ranev
 */
void syn_printe(){
	Token t = lookahead;

	printf("PLATY: Syntax error:  Line:%3d\n",line);
	printf("*****  Token code:%3d Attribute: ", t.code);
	switch(t.code){
		case  ERR_T: /* ERR_T     0   Error token */
			printf("%s\n",t.attribute.err_lex);
		 break;
		case  SEOF_T: /*SEOF_T    1   Source end-of-file token */
			printf("NA\n" );
		 break;
		case  AVID_T: /* AVID_T    2   Arithmetic Variable identifier token */
		case  SVID_T :/* SVID_T    3  String Variable identifier token */
			printf("%s\n",sym_table.pstvr[t.attribute.get_int].plex);
		 break;
		case  FPL_T: /* FPL_T     4  Floating point literal token */
			printf("%5.1f\n",t.attribute.flt_value);
		 break;
		case INL_T: /* INL_T      5   Integer literal token */
				printf("%d\n",t.attribute.get_int);
		 break;
		case STR_T:/* STR_T     6   String literal token */
				printf("%s\n",b_get_chloc(str_LTBL,t.attribute.str_offset));
		break;
        
			case SCC_OP_T: /* 7   String concatenation operator token */
				printf("NA\n" );
		break;
	
		case  ASS_OP_T:/* ASS_OP_T  8   Assignment operator token */
			printf("NA\n" );
		break;
		case  ART_OP_T:/* ART_OP_T  9   Arithmetic operator token */
			printf("%d\n",t.attribute.get_int);
		break;
		case  REL_OP_T: /*REL_OP_T  10   Relational operator token */ 
			printf("%d\n",t.attribute.get_int);
		break;
		case  LOG_OP_T:/*LOG_OP_T 11  Logical operator token */
			printf("%d\n",t.attribute.get_int);
		break;
	
		case  LPR_T: /*LPR_T    12  Left parenthesis token */
			printf("NA\n" );
		break;
		case  RPR_T: /*RPR_T    13  Right parenthesis token */
				printf("NA\n" );
		break;
		case LBR_T: /*    14   Left brace token */
				printf("NA\n" );
		break;
		case RBR_T: /*    15  Right brace token */
				printf("NA\n" );
		break;
		
		case KW_T: /*     16   Keyword token */
				printf("%s\n",kw_table [t.attribute.get_int]);
		break;
	
		case COM_T: /* 17   Comma token */
				printf("NA\n");
		break;
		case EOS_T: /*    18  End of statement *(semi - colon) */
				printf("NA\n" );
		break; 		
		default:
				printf("PLATY: Scanner error: invalid token code: %d\n", t.code);
    }/*end switch*/
}/* end syn_printe()*/