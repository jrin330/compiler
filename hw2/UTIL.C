/****************************************************/
/* File: util.c                                     */
/* Utility function implementation                  */
/* for the TINY compiler                            */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"

/* Procedure printToken prints a token 
 * and its lexeme to the listing file
 */
void printToken( TokenType token, const char* tokenString )
{ switch (token)
  { case IF:
      fprintf(listing, "      %s    %s\n","IF",tokenString);
      break;
    case ELSE:
      fprintf(listing, "      %s    %s\n","ELSE",tokenString);
      break;
    case INT:
      fprintf(listing, "      %s    %s\n","INT",tokenString);
      break;
    case RETURN:
      fprintf(listing, "      %s    %s\n","RETURN",tokenString);
      break;
    case VOID:
      fprintf(listing, "      %s    %s\n","VOID",tokenString);
      break;
    case WHILE:
      fprintf(listing, "      %s    %s\n","WHILE",tokenString);
      break;
    case PLUS: 
      fprintf(listing, "      %s    %s\n","+",tokenString);
      break;
    case MINUS: 
      fprintf(listing, "      %s    %s\n","-",tokenString);
      break;
    case TIMES: 
      fprintf(listing, "      %s    %s\n","*",tokenString);
      break;
    case OVER: 
      fprintf(listing, "      %s    %s\n","/",tokenString);
      break;
    case LT: 
      fprintf(listing, "      %s    %s\n","<",tokenString);
      break;
    case LE: 
      fprintf(listing, "      %s    %s\n","<=",tokenString);
      break;
    case GT: 
      fprintf(listing, "      %s    %s\n",">",tokenString);
      break;
    case GE: 
      fprintf(listing, "      %s    %s\n",">=",tokenString);
      break;
    case EQ: 
      fprintf(listing, "      %s    %s\n","==",tokenString);
      break;
    case NE: 
      fprintf(listing, "      %s    %s\n","!=",tokenString);
      break;
    case ASSIGN: 
      fprintf(listing, "      %s    %s\n","=",tokenString);
      break;
    case SEMI: 
      fprintf(listing, "      %s    %s\n",";",tokenString);
      break;
    case LPAREN: 
      fprintf(listing, "      %s    %s\n","(",tokenString);
      break;
    case RPAREN: 
      fprintf(listing, "      %s    %s\n",")",tokenString);
      break;
    case ENDFILE: 
      fprintf(listing, "      %s    %s\n","EOF",tokenString);
      break;
    case COMMA: 
      fprintf(listing, "      %s    %s\n",",",tokenString);
      break;
    case LBRACE: 
      fprintf(listing, "      %s    %s\n","[",tokenString);
      break;
    case RBRACE: 
      fprintf(listing, "      %s    %s\n","]",tokenString);
      break;
    case LCURLY: 
      fprintf(listing, "      %s    %s\n","{",tokenString);
      break;
    case RCURLY: 
      fprintf(listing, "      %s    %s\n","}",tokenString);
      break;
    case NUM:
      fprintf(listing, "      %s    %s\n","NUM",tokenString);
      break;
    case ID:
      fprintf(listing, "      %s    %s\n","ID",tokenString);
      break;
    case ERROR:
      if(strlen(tokenString) == 0)
        fprintf(listing, "      %s    %s\n","ERROR","Comment Error");
      else{
        fprintf(listing, "      %s    %s<%s>\n","ERROR","Invalid usage: ", tokenString);
      }
      break;
    default: /* should never happen */
      fprintf(listing,"Unknown token: %d\n",token);
  }
}



/* Function newStmtNode creates a new statement
 * node for syntax tree construction
 */
TreeNode * newStmtNode(StmtKind kind)
{ TreeNode * t = (TreeNode *) malloc(sizeof(TreeNode));
  int i;
  if (t==NULL)
    fprintf(listing,"Out of memory error at line %d\n",lineno);
  else {
    for (i=0;i<MAXCHILDREN;i++) t->child[i] = NULL;
    t->sibling = NULL;
    t->nodekind = StmtK;
    t->kind.stmt = kind;
    t->lineno = lineno;
  }
  return t;
}

/* Function newExpNode creates a new expression 
 * node for syntax tree construction
 */
TreeNode * newExpNode(ExpKind kind)
{ TreeNode * t = (TreeNode *) malloc(sizeof(TreeNode));
  int i;
  if (t==NULL)
    fprintf(listing,"Out of memory error at line %d\n",lineno);
  else {
    for (i=0;i<MAXCHILDREN;i++) t->child[i] = NULL;
    t->sibling = NULL;
    t->nodekind = ExpK;
    t->kind.exp = kind;
    t->lineno = lineno;
    t->type = Void;
    t->isParam = FALSE;
  }
  return t;
}

/* Function copyString allocates and makes a new
 * copy of an existing string
 */
char * copyString(char * s)
{ int n;
  char * t;
  if (s==NULL) return NULL;
  n = strlen(s)+1;
  t = (char*)malloc(n);
  if (t==NULL)
    fprintf(listing,"Out of memory error at line %d\n",lineno);
  else strcpy(t,s);
  return t;
}

/* Variable indentno is used by printTree to
 * store current number of spaces to indent
 */
static int indentno = 0;

/* macros to increase/decrease indentation */
#define INDENT indentno+=2
#define UNINDENT indentno-=2

/* printSpaces indents by printing spaces */
static void printSpaces(void)
{ int i;
  for (i=0;i<indentno;i++)
    fprintf(listing," ");
}

char *typeName(ExpType type){
  static char i[] = "int";
  static char v[] = "void";
  static char inv[] = "<invalid>";

  if(type == Integer) return i;
  else if(type == Void) return v;
  else return inv;
}

/* procedure printTree prints a syntax tree to the 
 * listing file using indentation to indicate subtrees
 */
void printTree( TreeNode * tree )
{ int i;
  INDENT;
  while (tree != NULL) {
    printSpaces();
    if (tree->nodekind==StmtK)
    { switch (tree->kind.stmt) {
        case CompoundK:
          fprintf(listing, "Compound Statement :\n");
          break;
        case IfK:
          if(tree->child[2] != NULL)
            fprintf(listing, "If (condition) {body} else {body}\n");
          else
            fprintf(listing, "If (condition) {body}\n");
          break;
        case IterK:
          fprintf(listing, "While (condition) {body}\n");
          break;
        case RetK:
          if(tree->child[0] == NULL)
            fprintf(listing, "Return Statement, with NULL\n");
          else
            fprintf(listing, "Return Statement, with arg\n");
          break;
        case CallK:
          if(tree->child[0] != NULL)
            fprintf(listing, "Call %s with args\n", tree->attr.name);
          else
            fprintf(listing, "Call %s\n", tree->attr.name);
          break;
        default:
          fprintf(listing,"Unknown ExpNode kind\n");
          break;
      }
    }
    else if (tree->nodekind==ExpK)
    { switch (tree->kind.exp) {
        case VarK:
          if(tree->isParam)
            fprintf(listing, "Var parameter : %s type %s\n", typeName(tree->type), tree->attr.name);
          else
            fprintf(listing, "Var Declaration : %s type %s\n", typeName(tree->type), tree->attr.name);
          break;
        case VarArrayK:
          if(tree->isParam)
            fprintf(listing, "Array Parameter : %s type %s\n", typeName(tree->type), tree->attr.name);
          else
            fprintf(listing, "Array Declaration : %s type %s\n", typeName(tree->type), tree->attr.name);
          break;
        case FuncK:
          fprintf(listing, "Function Declaration : %s type %s\n", typeName(tree->type), tree->attr.name);
          break;
        case AssignK:
          fprintf(listing, "Assign (dest) (src)\n");
          break;
        case OpK:
          fprintf(listing, "Op : ");
          printToken(tree->attr.op, "\0");
          break;
        case IdK:
          fprintf(listing, "ID : %s\n",tree->attr.name);
          break;
        case ConstK:
          fprintf(listing, "Val : %d\n", tree->attr.val);
          break;
        default:
          fprintf(listing,"Unknown ExpNode kind\n");
          break;
      }
    }
    else fprintf(listing,"Unknown node kind\n");
    for (i=0;i<MAXCHILDREN;i++)
         printTree(tree->child[i]);
    tree = tree->sibling;
  }
  UNINDENT;
}
