/****************************************************/
/* File: analyze.c                                  */
/* Semantic analyzer implementation                 */
/* for the TINY compiler                            */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "symtab.h"
#include "analyze.h"

int FuncDecl = FALSE;

/* Procedure traverse is a generic recursive 
 * syntax tree traversal routine:
 * it applies preProc in preorder and postProc 
 * in postorder to tree pointed to by t
 */
static void traverse( TreeNode * t,
               void (* preProc) (TreeNode *),
               void (* postProc) (TreeNode *) )
{ if (t != NULL)
  { preProc(t);
    { int i;
      for (i=0; i < MAXCHILDREN; i++)
        traverse(t->child[i],preProc,postProc);
    }
    postProc(t);
    traverse(t->sibling,preProc,postProc);
  }
}

/* nullProc is a do-nothing procedure to 
 * generate preorder-only or postorder-only
 * traversals from traverse
 */
static void nullProc(TreeNode * t)
{ if (t==NULL) return;
  else return;
}

static void symbolError(int lineno, char *msg, char *var){
  if(var != NULL)
    printf("Symbol %s Error at line %d: %s\n",var, lineno, msg);
  else
    printf("Symbol Error at line %d: %s\n",lineno, msg);
}

/* Procedure insertNode inserts 
 * identifiers stored in t into 
 * the symbol table 
 */
static void insertNode( TreeNode * t)
{ 
  BucketList l;
  switch (t->nodekind)
  { 
    case ExpK:
      switch(t->kind.exp){
        case VarK:
        case VarArrayK:
          printf("var declare %s\n", t->attr.name);
          if(t->type == Void){
            if(t->isParam == FALSE)
              symbolError(t->lineno, "Void cannot be variable", t->attr.name);
            break;
          }
          if(st_lookup(t->attr.name) == NULL){//First declared
            if(t->kind.exp == VarArrayK && t->isParam == FALSE)
              st_insert(t->attr.name,t->lineno,add_memloc(t->arraySize),t);
            else
              st_insert(t->attr.name,t->lineno,add_memloc(1), t);
          }
          else{
            if(st_lookup_top(t->attr.name) == NULL){
              if(t->kind.exp == VarArrayK && t->isParam == FALSE)
                st_insert(t->attr.name, t->lineno, add_memloc(t->arraySize), t);
              else
                st_insert(t->attr.name, t->lineno, add_memloc(1), t);
            }
            else
              symbolError(t->lineno,"Multi local declaration occured", t->attr.name);
          }
          break;
        case FuncK:
          printf("function declare %s\n",t->attr.name);
          if(st_lookup(t->attr.name) == NULL){
            st_insert(t->attr.name, t->lineno, add_memloc(1), t);
            scope_insert(t->attr.name);
            FuncDecl = TRUE;
          }
          else
            symbolError(t->lineno, "Multi Function declaration occured", t->attr.name);
          break;
        case IdK:
          printf("variable call\n");
          l = st_lookup(t->attr.name);
          if(l != NULL){
            if(l->node->kind.exp == VarK && t->child[0] != NULL)
              symbolError(t->lineno, "accessing variable like array", t->attr.name);
            else
              st_insert(t->attr.name, t->lineno, 0, t);
          }
          break;
        default:
        break;
      }
    case StmtK:
      switch(t->kind.stmt){
        case CallK:
          printf("function call\n");
          printf("%s\n",t->attr.name);
          printf("0\n");
          l = st_lookup(t->attr.name);
          printf("function lookup");
          if(l == NULL){
            symbolError(t->lineno, "Undeclared Function", t->attr.name);
          }
          else{
            st_insert(t->attr.name, t->lineno, 0, t);
            printf("function insert\n");
            t->type = l->node->type;
          }
          break;
        case CompoundK:
          printf("compound\n");
          if(FuncDecl){
            FuncDecl = FALSE;
          }
          else{
            scope_insert(scope_top()->name);
            copy_memloc();
          }
          break;
        default:
          break;
      }
    default:
      break;
  }
}

/* Function buildSymtab constructs the symbol 
 * table by preorder traversal of the syntax tree
 */
static void afterInsertNode(TreeNode *t){
  if(t->nodekind == StmtK && t->kind.stmt == CompoundK)
    scope_pop();
}

void buildSymtab(TreeNode * syntaxTree)
{ 
  scope_init();
  traverse(syntaxTree,insertNode,afterInsertNode);
  if (TraceAnalyze)
  { fprintf(listing,"\nSymbol table:\n\n");
    printSymTab(listing);
  }
}

static void typeError(TreeNode * t, char * message)
{ fprintf(listing,"Type error at line %d: %s\n",t->lineno,message);
  Error = TRUE;
}

/* Procedure checkNode performs
 * type checking at a single tree node
 */
static void checkNode(TreeNode * t)
{ switch (t->nodekind)
  { case ExpK:
  
    case StmtK:

    default:
      break;

  }
}

/* Procedure typeCheck performs type checking 
 * by a postorder syntax tree traversal
 */
void typeCheck(TreeNode * syntaxTree)
{ traverse(syntaxTree,nullProc,checkNode);
}
