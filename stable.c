/*
File name: stable.c
Compiler: MS Visual Studio 2012
Authors: Alec Pinsent 040726224
		 Josh Tate	  040713210
Course: CST 8152 – Compilers, Lab Section: 011
Assignment: 03
Date: 12/11/2014
Professor: Sv. Ranev
Purpose:  Functions implementing a symbol table.
Function list: 	st_create()
				st_install()
				st_lookup()
				st_update_type()
				st_update_value()
				st_get_type()
				st_destroy()
				st_print()
				static st_setsize()
				statc st_incoffet()
				static st_acmpch()
				static st_dcmpch()
				st_sort()
				st_store()
*/
/*******************INCLUDES*******************/
#include <stdio.h>   /* standard input / output */
#include <ctype.h>   /* conversion functions */
#include <stdlib.h>  /* standard library functions and constants */
#include <string.h>  /* standard library functions and constants */
#include "stable.h"

/*************EXTERN VARIABLES DECLARED HERE****************/
extern STD sym_table;
/********************STATIC FUNCTION DEFENITIONS************/
static void st_setsize();
static void st_incoffset();
static int st_acmpch(const void *, const void *);
static int st_dcmpch(const void *, const void *);


/**********************************************************************
Purpose: to initialize a symbol table for use
Author: Alec Pinsent	040726224
History/Versions: 1
Called functions: malloc(), b_create().
Parameters: int st_size
Return value: STD 
Algorithm: set size,
			allocate memory for the STVR array
			create the buffer using b_create
			return created STD
**********************************************************************/
STD st_create(int st_size){
	STD std;
	std.st_size = 0;
	std.pstvr = (STVR *)malloc(sizeof(STVR)*st_size);
	if(std.pstvr == NULL)
		return std;

	std.plsBD = b_create(C_INIT_CAPACITY,C_INC_FACTOR,ADDMODE);
	if(std.plsBD == NULL)
		return std;
	std.st_size=st_size;
	std.st_offset=0;
	return std;
}

/*********************************************************************
Purpose: To Install the passed lexeme into the Symbol Table
Author:Alec Pinsent	040726224
		Josh Tate	040713210
History/Versions: 1
Called functions: st_lookup(), st_store(), st_setsize(), b_addc(), b_get_r_flag(), b_get_chloc(), strlen(), st_incoffset()
Parameters: STD sym_table, char *lexeme, char type, int line
Return value:	-1 on error
				sym_table offset on success
Algorithm: if the symbol table is full, calls st_Store
			else will go through and add the lexeme and data to the symbol table
			based on type.

**********************************************************************/
int st_install(STD sym_table, char *lexeme, char type, int line){
	unsigned int i, j, offset, r_flag;
	int lookup;
	STVR new_record;

	r_flag = 0;
	
	if(sym_table.st_size <= 0)
		return FAIL;
	
	/* Call st_lookup() */
	lookup = st_lookup(sym_table, lexeme);
	if(lookup >= 0)
		return lookup;

	/* Check if table is full */
	if(sym_table.st_offset == sym_table.st_size){
		printf("\nError: The Symbol Table is full - install failed.\n\n");
		st_store(sym_table);
		st_setsize();
		return FAIL;
	}

	/* Set up new record */
	/* Set line number */
	new_record.o_line = line;

	/* Set pointer to addc_offset */
	new_record.plex = b_get_chloc(sym_table.plsBD, b_getsize(sym_table.plsBD));
	
	/* Add lexeme to CA */
	for (i = 0; i <= strlen(lexeme); i++){
		if(b_addc(sym_table.plsBD,lexeme[i]) == NULL) {
			return FAIL; /* return FAIL if an error occurred */
		}
		if(b_get_r_flag(sym_table.plsBD))
			r_flag = 1;
	}

	new_record.status_field = ZEROMK;
	new_record.status_field |= DFLTMK;


	/* Set i_value */
	if(type == 'I') {
		new_record.i_value.int_val = 0;
		new_record.status_field |= INTMK;
	}
	else if(type == 'F'){
		new_record.i_value.fpl_val = 0.0F;
		new_record.status_field |= FLPMK;
	}
	else if (type == 'S'){
		new_record.i_value.str_offset = -1;
		
		new_record.status_field |= STRMK;
		new_record.status_field |= SUPDATEMK;
	}
	else {
		return FAIL;
	}
	
	if (r_flag){
		offset = 0;
		for (i = 0; i <= sym_table.st_offset; ++i){
			sym_table.pstvr[i].plex = b_get_chloc(sym_table.plsBD, offset);
			for (j = 0; j <= strlen(sym_table.pstvr[i].plex); ++j){
				++offset;
			}
		}
		sym_table.plsBD->r_flag = 0;
	}


	/* Assign to table and increment offset */
	sym_table.pstvr[sym_table.st_offset] = new_record;
	st_incoffset();

	//st_print(sym_table);

	return sym_table.st_offset;
}

/**********************************************************************
Purpose: to get then location of the lexeme in the table
Author:	Josh Tate	040713210
History/Versions: 1
Called functions: strcmp()
Parameters:		STD sym_table char*lexeme
Return value: int: number of records compared or -1 on error
Algorithm:  loops through the table, checking for the passes lexeme
**********************************************************************/
int st_lookup(STD sym_table, char *lexeme){
	int i;			/*loop counter*/		
	
	if(sym_table.st_size <= 0)
		return FAIL;

	/* Start at end of buffer and work backwords */
	for (i = sym_table.st_offset-1; i >= 0; --i) {
		/* Compare stored plex with lexeme */
		if(!strcmp(sym_table.pstvr[i].plex, lexeme)){
			break;
		}
	}
	return i;
}

/**********************************************************************
Purpose: set the initial value for a record
Author: Alec Pinsent	040726224
		Josh Tate		040713210
History/Versions: 1
Parameters: STD sym_table, int vid_offset, initialValue i_value
Return value: int: -1 on error, vid_offset on success
**********************************************************************/
int st_update_value(STD sym_table, int vid_offset, initialValue i_value){
	if(sym_table.st_size <= 0)
		return FAIL;

	sym_table.pstvr[vid_offset].i_value=i_value;
	return vid_offset;
}

/**********************************************************************
Purpose: garbage collection
Author: Alec Pinsent	040726224
History/Versions: 1
Parameters: STD sym_table
**********************************************************************/
void st_destroy(STD sym_table){
	if(sym_table.st_size != NULL){
		b_destroy(sym_table.plsBD);
		free(sym_table.pstvr);
	}
}

/**********************************************************************
Purpose: Print out the symbol table contents
Author:	Josh Tate		040713210
History/Versions: 1
Parameters: STD sym_table
Return value: int: number of records, -1 on error
Algorithm: loop through and print all of the records,
			return number of records printed.
**********************************************************************/
int st_print(STD sym_table){
	unsigned int i;

	if(sym_table.st_size <= 0)
		return FAIL;

	printf("\nSymbol Table");
	printf("\n____________\n\n");
	printf("Line Number Variable Identifier\n");
	
	for(i = 0; i < sym_table.st_offset; ++i){
		printf("%2d          ", sym_table.pstvr[i].o_line);
		printf("%s\n", sym_table.pstvr[i].plex);
	}

	return i;
}

/**********************************************************************
Purpose: check the status feild to determine the type of the value
Author: Alec Pinsent	040726224
History/Versions: 1
Parameters: STD sym_table, int vid_offset
Return value: 'I' for integer
			  'F' for float
			  'S' for String
Algorithm: issolate bit 1 and 2 of the status field
			check them agaisnt preset identifiers
			return based on result
**********************************************************************/
char st_get_type(STD sym_table, int vid_offset){
	int mask;

	if(sym_table.st_size==0)
		return FAIL;
	if(vid_offset > 0 && vid_offset < sym_table.st_offset){

		mask = sym_table.pstvr[vid_offset].status_field & STRMK;

		return (mask == INTMK) ? 'I' : (mask == FLPMK) ? 'F' : 'S';
	}
	return FAIL;
}

/**********************************************************************
Purpose: update the status feild to the new type
Author: Alec Pinsent	040726224
History/Versions: 1
Parameters: STD sym_table, int vid_offset, char v_type
Return value: -1 on error, vid_offset on success
Algorithm:	check if the table has any records
			check if the type has been changed previously
			set the update flag (LSB)
			set the data type indicator based on passed v_type
			return vid_offset
**********************************************************************/
int st_update_type(STD sym_table, int vid_offset, char v_type){
 	int mask;

	if(sym_table.st_size <= 0)
		return FAIL;
	if(vid_offset > 0 && vid_offset < sym_table.st_offset){

		mask = sym_table.pstvr[vid_offset].status_field & LSBMK;
		if(mask == LSBMK)
			return FAIL;

		sym_table.pstvr[vid_offset].status_field |= RESETUPMK;
		sym_table.pstvr[vid_offset].status_field |= LSBMK;
		if(v_type == 'I'){
			sym_table.pstvr[vid_offset].status_field |= INTMK;
		}
		else if(v_type == 'F'){
			sym_table.pstvr[vid_offset].status_field |= FLPMK;
		}
		else if(v_type == 'S'){
			sym_table.pstvr[vid_offset].status_field |= STRMK;
		}
		else return FAIL;
	}
	return vid_offset;
}

/**********************************************************************
Purpose: to set the sum_table size to 0, only local use (Static)
Author: Alec Pinsent	040726224
History/Versions: 1
**********************************************************************/
static void st_setsize(){
	sym_table.st_size = 0;
}

/**********************************************************************
Purpose: increment the offset, only local use (Static)
Author: Alec Pinsent	040726224
History/Versions: 1
**********************************************************************/
static void st_incoffset(){
	++sym_table.st_offset;
}


/**********************************************************************
Purpose: ascending compare function
Author: Josh Tate	040713210
History/Versions: 1
Called functions: _strnicmp()
Parameters: void *p1, void *p2
Return value:	<0: p1 < p2
				0: p1 == p2
				>0: p1 > p2
Algorithm: uses _strnicmp() to compare the two passed strings, returning the result
**********************************************************************/
static int st_acmpch(const void *p1, const void *p2){
	const STVR *a = (STVR*)p1;    
	const STVR *b = (STVR*)p2;

	return (strcmp(a->plex,b->plex));
}


/**********************************************************************
Purpose: descending compare function
Author: Josh Tate	040713210
History/Versions: 1
Called functions: _strnicmp()
Parameters: void *p1, void *p2
Return value:	<0: p1 > p2
				0: p1 == p2
				>0: p1 < p2
Algorithm: uses _strnicmp() to compare the two passed strings, returning the result
**********************************************************************/
static int st_dcmpch(const void *p1, const void *p2){
	const STVR *a = (STVR*)p1;    
	const STVR *b = (STVR*)p2;

	return (strcmp(a->plex,b->plex)) * -1;
}

/**********************************************************************
Purpose: sort the lexeme's
Author: Josh Tate	040713210
History/Versions: 1
Called functions: qsort(), st_acmpch(), st_dcmpch()
Parameters: STD sym_table, char s_order
Return value: -1 on error, 1 on success
Algorithm:  for ascending order, sorts using the st_acmpch and qsort
			for descending order, sorts using the st_dcmpch and qsort
**********************************************************************/
int st_sort(STD sym_table, char s_order){
	switch (s_order) {
		case 'A':
			qsort(sym_table.pstvr,sym_table.st_offset, sizeof(STVR),st_acmpch);
			return 1;
		case 'D':
			qsort(sym_table.pstvr,sym_table.st_offset, sizeof(STVR),st_dcmpch);
			return 1;
		default:
			return FAIL;
	}
}

/**********************************************************************
Purpose: to store the symbol table in a file
Author: Alec Pinsent	040726224
History/Versions: 1
Called functions:fopen(), fprintf(), fclose()
Parameters:STD sym_table
Return value:-1 on error
			number of records stored on success
Algorithm: opens a new file with given filename and permission
			loops through the table printing records to the file
			returns -1 if an error is encountered
			closes the file, returns number of records on success
**********************************************************************/
int st_store(STD sym_table){
	int i=0;
	FILE * fp;
	fp = fopen(FILENAME,FILEW);

	fprintf(fp,"%d",sym_table.st_size);
 
	for(i;i<sym_table.st_size;i++){
		switch (st_get_type(sym_table,i)){
			case 'I':
				fprintf(fp," %04X %d %s %d %d",sym_table.pstvr[i].status_field,strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex,sym_table.pstvr[i].o_line, sym_table.pstvr[i].i_value.int_val);
				break;
			case 'F':
				fprintf(fp," %04X %d %s %d %.2f",sym_table.pstvr[i].status_field,strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex,sym_table.pstvr[i].o_line, sym_table.pstvr[i].i_value.fpl_val);
				break;
			case 'S':
				fprintf(fp," %04X %d %s %d %d",sym_table.pstvr[i].status_field,strlen(sym_table.pstvr[i].plex), sym_table.pstvr[i].plex,sym_table.pstvr[i].o_line, sym_table.pstvr[i].i_value.str_offset);
				break;
			default:
				return FAIL;
				break;
		}
		
	}
	printf("Symbol Table stored.\n");
	fclose(fp);
	return i;
}
