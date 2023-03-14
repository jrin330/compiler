/****************************************************/
/* File: symtab.h                                   */
/* Symbol table interface for the TINY compiler     */
/* (allows only one symbol table)                   */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#ifndef _SYMTAB_H_
#define _SYMTAB_H_


#include "globals.h"

/* SIZE is the size of the hash table */
#define SIZE 211

/* SHIFT is the power of two used as multiplier
   in hash function  */
#define SHIFT 4

/* the list of line numbers of the source 
 * code in which a variable is referenced
 */
typedef struct LineListRec
   { int lineno;
     struct LineListRec * next;
   } * LineList;

/* The record in the bucket lists for
 * each variable, including name, 
 * assigned memory location, and
 * the list of line numbers in which
 * it appears in the source code
 */
typedef struct BucketListRec
   { char * name;
     LineList lines;
     int memloc ; /* memory location for variable */
     struct BucketListRec * next;
		 TreeNode *node;
   } * BucketList;


typedef struct ScopeSpecListRec
	{
		BucketList table[SIZE];
		struct ScopeSpecListRec *parent;
		char *name;
		int level;
	} * ScopeList;
/* Procedure st_insert inserts line numbers and
 * memory locations into the symbol table
 * loc = memory location is inserted only the
 * first time, otherwise ignored
 */
void st_insert( char *, int , int , TreeNode *);

/* Function st_lookup returns the memory
 * location of a variable or -1 if not found
 * modified because of type checking of function's return type
 */
/* Searching all scope current variable or function is in
 */
BucketList st_lookup ( char * );
BucketList st_lookup_top(char *);

void scope_insert(char *name);
ScopeList scope_top(void);
void scope_pop(void);
void scope_init(void);

int add_memloc(int);
void copy_memloc(void);
/* Procedure printSymTab prints a formatted 
 * listing of the symbol table contents 
 * to the listing file
 */
void printSymTab(FILE * );

#endif
