/*
File name: buffer.h
Compiler: MS Visual Studio 2012
Author: Alec Pinsent 040726224
Course: CST 8152 – Compilers, Lab Section: 011
Assignment: 01
Date: 23/09/2014
Professor: Sv. Ranev
Purpose: to create and manage a buffer
Function list: b_create()
			   b_addc()
			   b_reset()
			   b_destroy()
			   b_isfull()
			   b_getsize()
			   b_getcapacity()
			   b_setmark()
			   b_getmode()
			   b_load()
			   b_isempty()
			   b_eob()
			   b_getc()
			   b_printI()
			   b_pack()
			   b_get_r_flag()
			   b_retract_to_mark()
			   b_get_getc_offset()
			   b_get_chloc()
*/
#ifndef BUFFER_H_
#define BUFFER_H_

/*#pragma warning(1:4001) *//*to enforce C89 type comments  - to make //comments an warning */

/*#pragma warning(error:4001)*//* to enforce C89 comments - to make // comments an error */

/* standard header files */
#include <stdio.h>  /* standard input/output */
#include <malloc.h> /* for dynamic memory allocation*/
#include <limits.h> /* implementation-defined data type ranges and limits */

/* constant definitions */
/* You may add your own constant definitions here */
#define R_FAIL_1 -1         /* fail return value */
#define R_FAIL_2 -2         /* fail return value */
#define LOAD_FAIL -2       /* load fail error */
#define SET_R_FLAG 1       /* realloc flag set value */
#define F_MODE 0			/*used to set mode to fixed*/
#define A_MODE 1			/*used to set mode to additive*/
#define M_MODE -1			/*used to set mode to multiplicative*/


/* user data type declarations */
typedef struct BufferDescriptor {
    char *ca_head;   /* pointer to the beginning of character array (character buffer) */
    short capacity;    /* current dynamic memory size (in bytes) allocated to character buffer */
    short addc_offset;  /* the offset (in chars) to the add-character location */
    short getc_offset;  /* the offset (in chars) to the get-character location */
    short mark_offset; /* the offset (in chars) to the mark location */
    char  inc_factor; /* character array increment factor */
    char  r_flag;     /* reallocation flag */
    char  mode;       /* operational mode indicator*/
    int   eob;       /* end-of-buffer flag */
} Buffer, *pBuffer;
/*typedef Buffer *pBuffer;*/

/* function declarations */
Buffer* b_create(short,char,char);
pBuffer b_addc(pBuffer const,char);
int b_reset(Buffer * const);
void b_destroy(Buffer * const );
int b_isfull(Buffer * const );
short b_getsize(Buffer * const );
short b_getcapacity(Buffer * const );
short b_setmark(Buffer * const , short );
short b_getmark(Buffer * const );
int b_getmode(Buffer * const );
int b_load(FILE * const fi, Buffer * const );
int b_isempty(Buffer * const );
int b_eob(Buffer * const );
char b_getc(Buffer * const );
int b_print(Buffer * const );
Buffer * b_pack(Buffer * const );
char b_get_r_flag(Buffer * const );
short b_retract_to_mark(Buffer * const );
short b_get_getc_offset(Buffer * const );
char * b_get_chloc(Buffer * const , short );


#endif
