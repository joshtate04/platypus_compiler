/*
File name: stable.h
Compiler: MS Visual Studio 2012
Authors: Alec Pinsent 040726224
		 Josh Tate	  040713210
Course: CST 8152 – Compilers, Lab Section: 011
Assignment: 03
Date: 12/11/2014
Professor: Sv. Ranev
Purpose:  to contain all the defines, function prototypes, and UDT's for the symbol table
Function list: 	st_create()
				st_install()
				st_lookup()
				st_update_type()
				st_update_value()
				st_get_type()
				st_destroy()
				st_print()
				st_sort()
				st_store()
*/

#ifndef STABLE_H
#define STABLE_H

/* DEFINES */
#define C_INIT_CAPACITY 200	/*used for buffer creation*/
#define C_INC_FACTOR 15		/*used for buffer creation*/
#define ADDMODE 'a'			/*used for buffer creation*/

#define FAIL -1		/*used when returning from a failed state*/

#define ZEROMK 0x0000    /* 0000 0000 0000 0000 */
#define DFLTMK 0xFFF8    /* 1111 1111 1111 1000 */
#define RESETUPMK 0xFFFE /* 1111 1111 1111 1110 */
#define SUPDATEMK 0x0001 /* 0000 0000 0000 0001 */
#define RESETMK 0xFFF9   /* 1111 1111 1111 1001 */
#define STRMK 0x0006     /* 0000 0000 0000 0110 */
#define INTMK 0x0004     /* 0000 0000 0000 0100 */
#define FLPMK 0x0002     /* 0000 0000 0000 0010 */
#define LSBMK 0x0001     /* 0000 0000 0000 0001 */

#define FILENAME "$stable.ste"  /*name of the output file for st_store*/
#define FILEW "w"				/*permission type for output file in st_store*/

#include "buffer.h"


/*UDT's*/
typedef union InitialValue {
	int int_val;
	float fpl_val;
	int str_offset;
}initialValue;

typedef struct SymbolTableVidRecord {
	unsigned short status_field;
	char* plex;
	int o_line;
	initialValue i_value;
	size_t reserved;
} STVR;

typedef struct SymbolTableDescriptor {
	STVR *pstvr;
	int st_size;
	int st_offset;
	Buffer *plsBD;
} STD;


/*Function prototypes*/
STD st_create(int);
int st_install(STD,char*,char,int);
int st_lookup(STD,char*);
int st_update_type(STD,int,char);
int st_update_value(STD,int,initialValue);
char st_get_type(STD,int);
void st_destroy(STD);
int st_print(STD);
int st_store(STD);
int st_sort(STD,char);

#endif
