/*
File name: buffer.c
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
#include "buffer.h"
/*
Purpose: This function creates a new buffer in memory
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: init_capacity: the intial capacity of the buffer, greater than 0, less than shrt_max
			inc_factor: the increment factor of the buffer
			o_mode: the mode for the buffer to be set to
Return value: buffer *
Algorithm: allocate the memory for the buffer
		   set the mode and inc factor
		   set capacity
		   return a pointer to the newly created buffer
*/
Buffer * b_create(short init_capacity,char inc_factor, char o_mode){
	/*allocate memory space for the buffer*/
	Buffer *pbuff = (Buffer*)calloc(1,sizeof(Buffer));
	/*check for allocation error*/
	if(pbuff == NULL){
		return NULL;
	}

	/*verify parameters are valid*/
	if(init_capacity < 0){
		return NULL;
	}

	/*allocate memory for the buffer stream, based on parameters*/
	pbuff->ca_head =(char*) malloc(init_capacity);
	/*check for allocation error*/
	if(pbuff->ca_head == NULL){
		return NULL;
	}
	/*assign the mode and inc_factor based on parameters*/
	if(o_mode == 'f' || inc_factor == '0'){
		pbuff->mode = 0;
		pbuff->inc_factor = 0;
	}
	else if(o_mode=='a' && 0<inc_factor<256){
		pbuff->mode=1;
		pbuff->inc_factor=inc_factor;
	}
	else if(o_mode == 'm' && 0<inc_factor<101){
		pbuff->mode = -1;
		pbuff->inc_factor=inc_factor;
	}

	else{
		return NULL;
	}
	pbuff->capacity = init_capacity;
	return pbuff;
}

/*
Purpose: This function adds symbol to the end of the character array
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: pBuffer const pBD: pointer to the buffer to be used, cannot be null.
		    char symbol: the symbol to be added to the buffer
Return value:  returns NULL on failure.
			   returns Buffer pointer on success.
Algorithm: if buffer is full, calculates the increment size based on mode.
		   reallocates the buffer to new capacity.
		   adds the symbol, returns the buffer pointer.		   
*/
pBuffer b_addc(pBuffer const pBD, char symbol){
	/*temp pointer used for memory allocation error checking*/
	char *temp=NULL;
	/*used for calculating remaining space in M mode*/
	float tempSpace=0;
	/*used for calculating remaining space in M mode*/
	short availSpace=0;

	/*checks for valid parameters*/
	if(pBD == NULL)
		return NULL;

	pBD->r_flag =0;
	/*check if buffer is full*/
	if(pBD->addc_offset==pBD->capacity){
		/*if the buffer is in fixed mode, cannot increase size*/
		if(pBD->mode==0){
			return NULL;
		}
		/*if the buffer is in additive mode, increase capacity by inc_factor*/
		else if(pBD->mode==1){
			/*check if capacity can be increased*/
			if(pBD->capacity+pBD->inc_factor<0)
				return NULL;
			else
				pBD->capacity+=pBD->inc_factor;
		}
		/*if buffer is in multiplicative mode, calcualte increase and reallocate space*/
		else if(pBD->mode==-1){
			availSpace=SHRT_MAX-(pBD->capacity);
			tempSpace=(float)availSpace*((float)pBD->inc_factor/100);
			/*if buffer is almost full, but not quite, set capacity to short max*/
			if(tempSpace==0){
				if(pBD->capacity!=SHRT_MAX)
					pBD->capacity=SHRT_MAX;
				else{
					printf("max buffer size");
					return NULL;
				}
			}
			else 
				if(pBD->capacity==tempSpace)
					pBD->capacity=SHRT_MAX;
				else
					pBD->capacity+=(short)tempSpace;
		}
	}
		/* reallocate the buffer to the new capacity*/
		temp=(char*)realloc(pBD->ca_head,pBD->capacity);
		/*check for memory allocation error*/
		if(temp==NULL){
			printf("error allocating memory");
			return NULL;
		}

		/*check if the buffer has moved, set r_flag accordingly */ 
		if(temp != pBD->ca_head)
			pBD->r_flag=SET_R_FLAG;
		/*move the buffer*/
		pBD->ca_head=temp;
		/*add the symbol to the buffer*/
		pBD->ca_head[pBD->addc_offset]=symbol;
		if(symbol != EOF)
			pBD->addc_offset++;
		return pBD;
}

/*
Purpose: This function retains memory allocated, but re-initializes all data members
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to hte buffer to reset.
Return value: int -1 on error, 1 on success
*/
int b_reset(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	pBD->addc_offset=0;
	pBD->getc_offset=0;
	pBD->mark_offset=0;
	if(pBD->addc_offset!=0)
		return -1;
	if(pBD->getc_offset!=0)
		return -1;
	if(pBD->mark_offset!=0)
		return -1;
	return 1;
}
/*
Purpose: This function de-allocates the memory occupied by the character buffer and he Buffer struct.
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to free.
Return value: void
*/
void b_destroy(Buffer * const pBD){
	if(pBD->ca_head != NULL){
		free(pBD->ca_head);
	}
	free(pBD);
}

/*
Purpose: checks if the buffer is full
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: int: returns 1 if full, -1 on error, 0 otherwise
*/
int b_isfull(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	if(pBD->capacity==INT_MAX)
		return 1;
	else if(pBD->capacity != INT_MAX)
		return 0;
	else
		return -1;
}

/*
Purpose: returns the current size of the buffer
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: short: -1 on error
*/
short b_getsize(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	return pBD->addc_offset;
}

/*
Purpose: returns the capacity of the buffer
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: short: -1 on error, capacity on success
*/
short b_getcapacity(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	return pBD->capacity;
}

/*
Purpose: set the mark of the buffer
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
			short mark: the mark to be set
Return value: short: returns -1 on error, mark_offset on success
*/
short b_setmark(Buffer * const pBD, short mark){
	if(pBD == NULL)
		return -1;
	if(!(0<mark<pBD->addc_offset)){
		printf("mark is out of bounds");
		return -1;
	}
	if(pBD->addc_offset==0)
		pBD->mark_offset=0;
	else
		pBD->mark_offset=mark;

	return pBD->mark_offset;

}

/*
Purpose: gets the current mark
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: short: returns -1 on error, mark_offset on success
*/
short b_getmark(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	return pBD->mark_offset;
}

/*
Purpose: returns the mode
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: int: -1 on error, mode otherwise
*/
int b_getmode(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	return pBD->mode;
}

/*
Purpose: loads the given file into the buffer
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: feof()
				  b_addc()
Parameters: FILE * const fi: FILE pointer to the file to be loaded
			Buffer * const pBD: pointer to the buffer to check.
Return value: int: -1 on error, number of chacters added on success
*/
int b_load(FILE * const fi, Buffer * const pBD){
	int i=0;
	char c;
	if(fi == NULL || pBD == NULL)
		return LOAD_FAIL;
	do{
		c = (char)fgetc(fi);
		if(c == EOF || c == '\0' || c == 255)
			break;
		if(b_addc(pBD,c) == NULL)
			return -1;
		i++;
	}while(!feof(fi));
	return i;
}

/*
Purpose: checks if the buffer is empty
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns 1 if true, 0 if false, -1 on failure.
*/
int b_isempty(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	if(pBD->addc_offset == 0)
		return 1;
	else
		return 0;
}

/*
Purpose: returns eob to the calling function
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns -1 on failure, eob on success
*/
int b_eob(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	if(pBD->addc_offset==pBD->getc_offset)
		pBD->eob = 1;
	else
		pBD->eob=0;
	return pBD->eob;
}

/*
Purpose: returns the next character
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: char: returns -1 on error, 0 on eof, the next char otherwise  
*/
char b_getc(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	if(pBD->getc_offset == pBD->addc_offset){
		pBD->eob=1;
		return -1;
	}
	else
		pBD->eob=0;

	//if(pBD->ca_head[pBD->getc_offset]==EOF)
	//	return EOF;
	//else
		return pBD->ca_head[pBD->getc_offset++];

}

/*
Purpose: prints the contents of the buffer
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: b_eob()
				  b_getc()
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns 1 if true, 0 if false, -1 on failure.
*/
int b_print(Buffer * const pBD){
	int i=0;
	char c;
	if(pBD == NULL)
		return -1;
	if(pBD->addc_offset==0){
		printf("The buffer is empty\n");
		return i;
	}
	while(!b_eob(pBD)){
		printf("%c",b_getc(pBD));
	}
	pBD->getc_offset=0;
	printf("\n");
	return i;
} 

/*
Purpose: sets the capacity to currecnt size + 1
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns NULL on error, pointer to the buffer otherwise
*/
Buffer * b_pack(Buffer * const pBD){
	char * temp;
	if(pBD == NULL || pBD->ca_head == NULL){
		//printf("ERROR");
		return NULL;
	}
	if(!b_isempty(pBD)){
		//printf("%s\n", pBD->ca_head);
		temp=(char*) realloc(pBD->ca_head,pBD->addc_offset);
		if(temp==NULL){
			//printf("memory allocation failure");
			return NULL;
		}
		if(temp != pBD->ca_head){
			pBD->r_flag=SET_R_FLAG;
			pBD->ca_head=temp;
		}
	

		pBD->capacity=pBD->addc_offset+1;
		pBD->eob=0;
	}
	return pBD;
}

/*
Purpose: returns r_flag
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns -1 on error, r_flag on success
*/
char b_get_r_flag(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	return pBD->r_flag;
}

/*
Purpose: sets the next get character to the current mark
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns -1 on error, getc_offset on success
*/
short b_retract_to_mark(Buffer * const pBD){
	if(pBD == NULL)
		return -1;
	pBD->getc_offset=pBD->mark_offset;
	if(pBD->getc_offset!=pBD->mark_offset)
		return -1;
	else return pBD->getc_offset;
}

/*
Purpose: returns getc_offset
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns -1 on failure, getc_offset on success
*/
short b_get_getc_offset(Buffer * const pBD){
	if(pBD != NULL)
		return pBD->getc_offset;
	else
		return -1;
}

/*
Purpose: returns a pointer to a character in the buffer
Author: Alec Pinsent
History/Versions: 1 23/09/2014
Called functions: none
Parameters: Buffer * const pBD: pointer to the buffer to check.
Return value: returns NULL on error, a pointer to the character on success
*/
char * b_get_chloc(Buffer * const pBD, short offset){
	if(pBD == NULL)
		return NULL;
	if(0 > offset || offset > pBD->addc_offset){
		return NULL;
	}
	return pBD->ca_head + offset;
}


