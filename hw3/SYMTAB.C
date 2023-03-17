/****************************************************/
/* File: symtab.c                                   */
/* Symbol table implementation for the TINY compiler*/
/* (allows only one symbol table)                   */
/* Symbol table is implemented as a chained         */
/* hash table                                       */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "symtab.h"

/* the hash function */
static int hash(char *key)
{
  int temp = 0;
  int i = 0;
  while (key[i] != '\0')
  {
    temp = ((temp << SHIFT) + key[i]) % SIZE;
    ++i;
  }
  return temp;
}

// For print Symbol table
static ScopeList allScope[SIZE];
static int allScope_top = 0;

// For current Symbol table with nested level
static ScopeList scopeStack[SIZE];
static int scopeStack_top = -1;

static int memlocStack[SIZE];
static int memloc_top = -1;

static char *funcname;

/* Procedure st_insert inserts line numbers and
 * memory locations into the current symbol table
 * loc = memory location is inserted only the
 * first time, otherwise ignored
 */
void st_insert(char *name, int lineno, int loc, TreeNode *node)
{
  int h = hash(name);
  ScopeList s = scopeStack[scopeStack_top];
  BucketList l;
  // first insert
  s = scopeStack[scopeStack_top];
  l = (BucketList)malloc(sizeof(struct BucketListRec));
  l->name = name;
  l->lines = (LineList)malloc(sizeof(struct LineListRec));
  l->lines->lineno = lineno;
  l->memloc = loc;
  l->lines->next = NULL;
  l->next = s->table[h];
  s->table[h] = l;
  l->node = node;
} /* st_insert */

void lineno_insert(char *name, int lineno){
  int h = hash(name);
  ScopeList s = scopeStack[scopeStack_top];
  BucketList l;
 
  while (s != NULL)
  {
    l = s->table[h];
    while ((l != NULL) && (strcmp(name, l->name) != 0))
      l = l->next;
    if (l != NULL) // exist
    {
      LineList t = l->lines;
      while (t->next != NULL)
        t = t->next;
      t->next = (LineList)malloc(sizeof(struct LineListRec));
      t->next->lineno = lineno;
      t->next->next = NULL;
      return;
    }
    s = s->parent;
  }
}

/* Function st_lookup returns the memory
 * location of a variable or -1 if not found
 * modified because of type checking of function's return type
 */
/* Searching current all scope variable or function is in
 */
BucketList st_lookup(char *name)
{
  int h = hash(name);
  ScopeList s = scopeStack[scopeStack_top];
  BucketList l = NULL;
  while (s != NULL)
  {
    l = s->table[h];
    while ((l != NULL) && (strcmp(name, l->name) != 0))
      l = l->next;
    if (l != NULL)
    {
      funcname = s->name;
      return l;
    }
    s = s->parent;
  }
  return NULL;
}

BucketList st_lookup_top(char *name)
{
  int h = hash(name);
  ScopeList s = scopeStack[scopeStack_top];
  BucketList l = NULL;
  if (s != NULL)
  {
    l = s->table[h];
    while (l != NULL && strcmp(l->name, name) != 0)
      l = l->next;
  }
  return l;
}

void scope_insert(char *name)
{
  ScopeList node = (ScopeList)malloc(sizeof(struct ScopeSpecListRec));
  node->name = name;
  node->parent = scopeStack[scopeStack_top];
  node->level = node->parent->level + 1;

  allScope[allScope_top++] = node;
  scopeStack[++scopeStack_top] = node;
  memlocStack[++memloc_top] = 0;
}

void scope_init(void)
{
  ScopeList node = (ScopeList)malloc(sizeof(struct ScopeSpecListRec));
  node->name = "Global init";
  node->parent = NULL;
  node->level = 0;

  allScope[allScope_top++] = node;
  scopeStack[++scopeStack_top] = node;
  memlocStack[++memloc_top] = 0;
}

void scope_push(ScopeList s)
{
  scopeStack[++scopeStack_top] = s;
}

void scope_pop(void)
{
  scopeStack_top--;
  memloc_top--;
}

ScopeList scope_top(void)
{
  return scopeStack[scopeStack_top];
}

int add_memloc(int size)
{
  memlocStack[memloc_top] += size;
  return memlocStack[memloc_top];
}

void copy_memloc(void)
{
  memlocStack[memloc_top] = memlocStack[memloc_top - 1];
}
/* Procedure printSymTab prints a formatted
 * listing of the symbol table contents
 * to the listing file
 */

void printSymTab_cur(FILE *listing, BucketList cur[])
{
  BucketList tmp;
  for (int i = 0; i < SIZE; ++i)
  {
    tmp = cur[i];
    if (tmp != NULL)
    {
      TreeNode *node = tmp->node;
      LineList l = tmp->lines;
      while (l != NULL)
      {
        fprintf(listing, "%-14s ", tmp->name);
        fprintf(listing, "%-12d  ", tmp->memloc);
        char *s;
        switch (node->kind.exp)
        {
        case VarK:
        case VarArrayK:
          if (node->isParam)
            s = "Parameter";
          else
            s = "Variable";
          break;
        case FuncK:
          s = "Function";
          break;
        default:
          break;
        }
        fprintf(listing, "%-18s", s);

        switch (node->type)
        {
        case Void:
          s = "Void";
          break;
        case Integer:
          if (node->kind.exp == VarArrayK)
            s = "IntegerArray";
          else
            s = "Integer";
          break;
        default:
          break;
        }
        fprintf(listing, "%-13s", s);

        while (l != NULL)
        {
          fprintf(listing, "%4d", l->lineno);
          l = l->next;
        }
        fprintf(listing, "\n");
        tmp = tmp->next;
      }
    }
  }
}
void printSymTab(FILE *listing)
{
  ScopeList cur_s;
  for (int i = 0; i < allScope_top; i++)
  {
    cur_s = allScope[i];
    fprintf(listing, "Function name: %s (nested level : %d)\n", cur_s->name, cur_s->level);
    fprintf(listing, "Symbol Name   Mem loc       Symbol Type      Data Type       Line Numbers\n");
    fprintf(listing, "----------- -----------   ---------------   --------------- ----------------\n");
    printSymTab_cur(listing, cur_s->table);
    fprintf(listing, "\n");
  }
} /* printSymTab */