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
static void traverse(TreeNode *t,
                     void (*preProc)(TreeNode *),
                     void (*postProc)(TreeNode *))
{
  if (t != NULL)
  {
    preProc(t);
    {
      int i;
      for (i = 0; i < MAXCHILDREN; i++)
        traverse(t->child[i], preProc, postProc);
    }
    postProc(t);
    traverse(t->sibling, preProc, postProc);
  }
}

/* nullProc is a do-nothing procedure to
 * generate preorder-only or postorder-only
 * traversals from traverse
 */
static void nullProc(TreeNode *t)
{
  if (t == NULL)
    return;
  else
    return;
}

static void symbolError(int lineno, char *msg, char *var)
{
  if (var != NULL)
    printf("Symbol %s Error at line %d: %s\n", var, lineno, msg);
  else
    printf("Symbol Error at line %d: %s\n", lineno, msg);
}

/* Procedure insertNode inserts
 * identifiers stored in t into
 * the symbol table
 */
static void insertNode(TreeNode *t)
{
  BucketList l;
  switch (t->nodekind)
  {
  case ExpK:
    switch (t->kind.exp)
    {
    case VarK:
    case VarArrayK:
      if (t->type == Void)
      {
        if (t->isParam == FALSE)
          symbolError(t->lineno, "Void cannot be variable", t->attr.name);
        break;
      }
      if (st_lookup(t->attr.name) == NULL)
      { // First declared
        if (t->kind.exp == VarArrayK && t->isParam == FALSE)
          st_insert(t->attr.name, t->lineno, add_memloc(t->arraySize), t);
        else
          st_insert(t->attr.name, t->lineno, add_memloc(1), t);
      }
      else
      {
        if (st_lookup_top(t->attr.name) == NULL)
        {
          if (t->kind.exp == VarArrayK && t->isParam == FALSE)
            st_insert(t->attr.name, t->lineno, add_memloc(t->arraySize), t);
          else
            st_insert(t->attr.name, t->lineno, add_memloc(1), t);
        }
        else
          symbolError(t->lineno, "Multi local declaration occured", t->attr.name);
      }
      break;
    case FuncK:
      if (st_lookup(t->attr.name) == NULL)
      {
        st_insert(t->attr.name, t->lineno, add_memloc(1), t);
        scope_insert(t->attr.name);
        FuncDecl = TRUE;
      }
      else
        symbolError(t->lineno, "Multi Function declaration occured", t->attr.name);
      break;
    case IdK:
      l = st_lookup(t->attr.name);
      if (l != NULL)
      {
        if (l->node->kind.exp == VarK && t->child[0] != NULL)
          symbolError(t->lineno, "accessing variable like array", t->attr.name);
        else
          lineno_insert(t->attr.name, t->lineno);
      }
      break;
    default:
      break;
    }
    break;
  case StmtK:
    switch (t->kind.stmt)
    {
    case CallK:
      l = st_lookup(t->attr.name);
      if (l == NULL)
      {
        symbolError(t->lineno, "Undeclared Function", t->attr.name);
      }
      else
      {
        lineno_insert(t->attr.name, t->lineno);
        t->type = l->node->type;
      }
      break;
    case CompoundK:
      if (FuncDecl)
      {
        FuncDecl = FALSE;
      }
      else
      {
        scope_insert(scope_top()->name);
        copy_memloc();
      }
      t->attr.scope = scope_top();
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
static void afterInsertNode(TreeNode *t)
{
  if (t->nodekind == StmtK && t->kind.stmt == CompoundK)
    scope_pop();
}

void buildSymtab(TreeNode *syntaxTree)
{
  scope_init();
  traverse(syntaxTree, insertNode, afterInsertNode);
  if (TraceAnalyze)
  {
    fprintf(listing, "\nSymbol table:\n\n");
    printSymTab(listing);
  }
}

static void typeError(TreeNode *t, char *message)
{
  fprintf(listing, "Type error at line %d: %s\n", t->lineno, message);
  Error = TRUE;
}

/* Procedure checkNode performs
 * type checking at a single tree node
 */
static char *curFunc; // For check return value
static void beforeCheckNode(TreeNode *t)
{
  if (t->nodekind == ExpK && t->kind.exp == FuncK)
    curFunc = t->attr.name;
  if (t->nodekind == StmtK && t->kind.stmt == CompoundK)
    scope_push(t->attr.scope);
}

static void checkNode(TreeNode *t)
{
  ScopeList s;
  BucketList l;

  TreeNode *lhs;
  TreeNode *rhs;

  TreeNode *argv, *param;
  int isArray = FALSE;
  switch (t->nodekind)
  {
  case ExpK:
    lhs = t->child[0];
    rhs = t->child[1];
    switch (t->kind.exp)
    {
    case AssignK:
      if (rhs->type == Void)
        typeError(t, "Assign with Void value");
      else
      {
        l = st_lookup(lhs->attr.name);
        if (l == NULL)
        {
          typeError(t, "LHS is not in symbol table");
          break;
        }
        else
        {
          if (lhs->child[0] == NULL && l->node->kind.exp == VarArrayK)
          {
            typeError(t, "Array cannot be assigned like variable");
            break;
          }
        }
        t->type = Integer;
      }
      break;
    case OpK:
      if (lhs->kind.exp == ConstK || lhs->kind.exp == OpK)
      {
        t->type = Integer;
        break;
      }
      l = st_lookup(lhs->attr.name);
      if (!((lhs->child[0] == NULL && l->node->kind.exp == VarK) ||
            (lhs->child[0] != NULL && l->node->kind.exp == VarArrayK)))
      {
        typeError(lhs, "Invalid LHS variable using");
        break;
      }

      if (rhs->kind.exp == ConstK || rhs->kind.exp == OpK)
      {
        t->type = Integer;
        break;
      }
      l = st_lookup(rhs->attr.name);
      if (!((rhs->child[0] == NULL && l->node->kind.exp == VarK) ||
            (rhs->child[0] != NULL && l->node->kind.exp == VarArrayK)))
      {
        typeError(rhs, "Invalid RHS variable using");
        break;
      }
      t->type = Integer;
      break;

    default:
      break;
    }
    break;
  case StmtK:
    switch (t->kind.stmt)
    {
    case CompoundK:
      scope_pop();
      break;

    case RetK:
      l = st_lookup(curFunc);
      if (l->node->type == Void && t->child[0] != NULL)
        typeError(t, "Void function cannot return value");
      else if (l->node->type == Integer && t->child[0] == NULL)
        typeError(t, "Integer function should return integer value");
      else if (l->node->type == Integer && t->child[0]->kind.exp == IdK &&
               t->child[0]->child[0] == NULL)
      { // return variable
        l = st_lookup(t->child[0]->attr.name);
        if (l != NULL && l->node->kind.exp == VarArrayK)
          typeError(t, "Interger Function cannot return array");
      }
      break;

    case CallK:
      l = st_lookup(t->attr.name);
      if (l == NULL)
      {
        typeError(t, "Function not in symbol table");
        break;
      }

      argv = t->child[0];
      param = l->node->child[0];

      if (l->node->kind.exp != FuncK)
      {
        typeError(t, "Function call only can with Function");
        break;
      }

      if (param->type == Void)
      {
        if (argv != NULL)
          typeError(t, "Function doesn't need args");
        break;
      }

      while (argv != NULL && param != NULL)
      {
        ExpKind a = argv->kind.exp;
        ExpKind p = param->kind.exp;

        if (p == VarArrayK)
        { // check Array parameter
          if (a != IdK)
            break;
          l = st_lookup(argv->attr.name);
          if (l == NULL)
            break;
          if (l->node->kind.exp != VarArrayK)
            break;

          argv = argv->sibling;
          param = param->sibling;
          continue;
        }

        if (a == OpK || a == ConstK || a == AssignK)
        { // check integer argument
          argv = argv->sibling;
          param = param->sibling;
          continue;
        }

        if (argv->type != param->type)
          break;

        argv = argv->sibling;
        param = param->sibling;
      }
      if (argv == NULL && param == NULL)
        break; // correct
      else if (argv != NULL && param == NULL)
        typeError(t, "Too many arguments");
      else if (argv == NULL && param != NULL)
        typeError(t, "Insufficient arguments");
      else{
        fprintf(listing, "%s %s\n", argv->attr.name, param->attr.name);
        typeError(t, "argument's type error");
      }
      break;

    default:
      break;
    }
  }
}

/* Procedure typeCheck performs type checking
 * by a postorder syntax tree traversal
 */
void typeCheck(TreeNode *syntaxTree)
{
  traverse(syntaxTree, beforeCheckNode, checkNode);
}
