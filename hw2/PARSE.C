/****************************************************/
/* File: parse.c                                    */
/* The parser implementation for the TINY compiler  */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"
#include "parse.h"

static TokenType token; /* holds current token */

/* function prototypes for recursive calls */
static TreeNode * decl_list(void);
static TreeNode * decl(void);
static TreeNode * var_decl(void);
static ExpType type_spec(void);
static TreeNode * params(void);
static TreeNode * param_list(ExpType type);
static TreeNode * param(ExpType type);
static TreeNode * compound_stmt(void);
static TreeNode * local_decl(void);
static TreeNode * stmt_list(void);
static TreeNode * stmt(void);
static TreeNode * expr_stmt(void);
static TreeNode * if_stmt(void);
static TreeNode * iter_stmt(void);
static TreeNode * ret_stmt(void);
static TreeNode * expr(void);
static TreeNode * simple_expr(TreeNode *f);
static TreeNode * add_expr(TreeNode *f);
static TreeNode * term(TreeNode *f);
static TreeNode * factor(TreeNode *f);
static TreeNode *  call(void);
static TreeNode * args(void);
static TreeNode * args_list(void);

static void syntaxError(char * message)
{ fprintf(listing,"\n>>> ");
  fprintf(listing,"Syntax error at line %d: %s",lineno,message);
  Error = TRUE;
}

static void match(TokenType expected)
{ if (token == expected) token = getToken();
  else {
    syntaxError("unexpected token (in match) -> ");
    printToken(token,tokenString);
    fprintf(listing,"      ");
  }
}

TreeNode * decl_list(void){
  TreeNode *t = decl();
  TreeNode *p = t;
  while(token != ENDFILE){
    TreeNode *q = decl();
    if(q!=NULL){
      if(t == NULL) t = p = q;
      else{
        p->sibling = q;
        p = q;
      }
    }
  }
  return t;
}
TreeNode * decl(void){
  TreeNode *t;
  ExpType type;
  char *name;
  type = type_spec();
  name = copyString(tokenString);
  match(ID);

  switch(token){
    case SEMI:
      t = newExpNode(VarK);
      if(t != NULL){
        t->attr.name = name;
        t->type = type;
      }
      match(SEMI);
      break;
    case LBRACE:
      t = newExpNode(VarArrayK);
      if(t != NULL){
        t->attr.name = name;
        t->type = type;
      }
      match(LBRACE);
      if(t != NULL) t->arraySize = atoi(tokenString);
      match(NUM);
      match(RBRACE);
      match(SEMI);
      break;
    case LPAREN:
      t = newExpNode(FuncK);
      if(t != NULL){
        t->attr.name = name;
        t->type = type;
      }
      match(LPAREN);
      if(t != NULL) t->child[0] = params();
      match(RPAREN);
      if(t != NULL) t->child[1] = compound_stmt();
      break;
    default : 
      syntaxError("unexpected token (in decl) -> ");
      printToken(token, tokenString);
      token = getToken();
      break;
  }
  return t;
}

TreeNode * var_decl(void){
  TreeNode *t;
  ExpType type;
  char *name;

  type = type_spec();
  name = copyString(tokenString);
  match(ID);
  switch(token){
    case SEMI:
      t = newExpNode(VarK);
      if(t != NULL){
        t->attr.name = name;
        t->type = type;
      }
      match(SEMI);
      break;
    case LBRACE:
      t = newExpNode(VarArrayK);
      if(t != NULL){
        t->attr.name = name;
        t->type = type;
      }
      match(LBRACE);
      if(t != NULL) t->arraySize = atoi(tokenString);
      match(NUM);
      match(RBRACE);
      match(SEMI);
      break;
    default:
      syntaxError("unexpected token (in var_decl) -> ");
      printToken(token, tokenString);
      token = getToken();
      break;
  }
  return t;
}

ExpType type_spec(void){
  if(token == INT){
    token = getToken();
    return Integer;
  }
  else if(token == VOID){
    token = getToken();
    return Void;
  }
  
  syntaxError("unexpected token (in type_spec) -> ");
  printToken(token, tokenString);
  token = getToken();
  return Void;
}

TreeNode * params(void){
  TreeNode *t;
  ExpType type = type_spec();

  if(type == Void && token == RPAREN){
    t = newExpNode(VarK);
    t->isParam = TRUE;
    t->type = Void;
  }
  else
    t = param_list(type);
  return t;
}

TreeNode * param_list(ExpType type){
  TreeNode *t = param(type);
  TreeNode *p = t;
  TreeNode *q;
  while(token == COMMA){
    match(COMMA);
    q = param(type_spec());
    if(q != NULL){
      if(t == NULL) t = p = q;
      else{
        p->sibling = q;
        p = q;
      }
    }
  }
  return t;
}

TreeNode * param(ExpType type){
  TreeNode *t;
  char *name;

  name = copyString(tokenString);
  match(ID);
  if(token == LBRACE){
    match(LBRACE);
    match(RBRACE);
    t = newExpNode(VarArrayK);
  }
  else
    t = newExpNode(VarK);

  if(t != NULL){
    t->attr.name = name;
    t->type = type;
    t->isParam = TRUE;
  }
  return t;
}

TreeNode * compound_stmt(void){
  TreeNode *t = newStmtNode(CompoundK);
  match(LCURLY);
  t->child[0] = local_decl();
  t->child[1] = stmt_list();
  match(RCURLY);
  return t;
}

TreeNode * local_decl(void){
  TreeNode *t = NULL;
  TreeNode *p;

  if(token == INT || token == VOID)
    t = var_decl();
  p = t;

  if(t != NULL){
    while(token == INT || token == VOID){
      TreeNode *q = var_decl();
      if(q != NULL){
        p->sibling = q;
        p = q;
      }
    }
  }
  return t;
}

TreeNode * stmt_list(void){
  TreeNode *t;
  TreeNode *p;

  if(token == RCURLY) return NULL;

  t = stmt();
  p = t;

  while(token != RCURLY){
    TreeNode *q = stmt();
    if(q != NULL){
      if(p == NULL) t = p = q;
      else{
        p->sibling = q;
        p = q;
      }
    }
  }
  return t;
}

TreeNode * stmt(void){
  TreeNode *t;

  switch(token){
    case LCURLY:
      t = compound_stmt();
      break;
    case IF:
      t = if_stmt();
      break;
    case WHILE:
      t = iter_stmt();
      break;
    case RETURN:
      t = ret_stmt();
      break;
    case ID:
    case LPAREN:
    case NUM:
    case SEMI:
      t = expr_stmt();
      break;
    default:
      syntaxError("unexpected token (in stmt) -> ");
      printToken(token, tokenString);
      token = getToken();
      t = NULL;
  }

  return t;
}

TreeNode * expr_stmt(void){
  TreeNode *t;

  if(token == SEMI) match(SEMI);
  else if(token != RCURLY){
    t = expr();
    match(SEMI);
  }
  return t;
}

TreeNode * if_stmt(void){
  TreeNode *t = newStmtNode(IfK);

  match(IF);
  match(LPAREN);
  if(t != NULL) t->child[0] = expr();

  match(RPAREN);
  if(t != NULL) t->child[1] = stmt();
  
  if(token == ELSE){
    match(ELSE);
    if(t != NULL) t->child[2] = stmt();
  }

  return t;
}

TreeNode * iter_stmt(void){
  TreeNode *t = newStmtNode(IterK);

  match(WHILE);
  match(LPAREN);
  if(t != NULL) t->child[0] = expr();

  match(RPAREN);
  if(t != NULL) t->child[1] = stmt();

  return t;
}
TreeNode * ret_stmt(void){
  TreeNode *t = newStmtNode(RetK);

  match(RETURN);
  if(t != NULL && token != SEMI) t->child[0] = expr();

  match(SEMI);
  return t;
}

TreeNode * expr(void){
  TreeNode *t;
  TreeNode *p = NULL;
  int is_ID = token == ID;

  if(is_ID){
    p = call();

    if(token == ASSIGN){
      if(p != NULL && p->nodekind == ExpK && p->kind.exp == IdK){
        match(ASSIGN);
        t = newExpNode(AssignK);
        if(t != NULL){
          t->child[0] = p;
          t->child[1] = expr();
        }
      }
      else{
        syntaxError("assign error (expr)");
        token = getToken();
      }
    }
    else
      t = simple_expr(p);
  }
  else
    t = simple_expr(p);

  return t;
}

TreeNode * simple_expr(TreeNode *f){
  TreeNode *t;
  TreeNode *p = add_expr(f);
  TokenType op;

  if(token == LT || token == GT || token == LE ||
     token == GE || token == EQ || token == NE){
      op = token;
      match(token);

      t = newExpNode(OpK);
      if(t != NULL){
        t->child[0] = p;
        t->child[1] = add_expr(NULL);
        t->attr.op = op;
      }
  }
  else
    t = p;

  return t;
}

TreeNode * add_expr(TreeNode *f){
  TreeNode *p = term(f);
  TreeNode *q;

  if(p != NULL){
    while(token == PLUS || token == MINUS){
      q = newExpNode(OpK);
      if(q != NULL){
        q->child[0] = p;
        q->attr.op = token;
        p = q;
        match(token);
        p->child[1] = term(NULL);
      }
    }
  }
  return p;
}

TreeNode * term(TreeNode *f){
  TreeNode *p = factor(f);
  TreeNode *q;

  if(p != NULL){
    while(token == TIMES || token == OVER){
      q = newExpNode(OpK);
      if(q != NULL){
        q->child[0] = p;
        q->attr.op = token;
        p = q;
        match(token);
        p->child[1] = factor(NULL);
      }
    }
  }

  return p;
}

TreeNode * factor(TreeNode *f){
  if(f != NULL) return f;
  TreeNode *t;

  switch(token){
    case LPAREN:
      match(LPAREN);
      t = expr();
      match(RPAREN);
      break;
    case ID:
      t = call();
      break;
    case NUM:
      t = newExpNode(ConstK);
      if(t != NULL){
        t->attr.val = atoi(tokenString);
        t->type = Integer;
      }
      match(NUM);
      break;
    default:
      syntaxError("unexpected token (in factor) -> ");
      printToken(token, tokenString);
      token = getToken();
      return NULL;
  }

  return t;
}

TreeNode * call(void){
  TreeNode *t;
  char *name;

  if(token == ID) name = copyString(tokenString);
  match(ID);

  if(token == LPAREN){ // function call
    match(LPAREN);
    t = newStmtNode(CallK);
    if(t != NULL){
      t->attr.name = name;
      t->child[0] = args();
    }
    match(RPAREN);
  }
  else if(token == LBRACE){ // array call
    match(LBRACE);
    t = newExpNode(IdK);
    if(t != NULL){
      t->attr.name = name;
      t->type = Integer;
      t->child[0] = expr();
    }
    match(RBRACE);
  }
  else{ // variable call
    t = newExpNode(IdK);
    if(t != NULL){
      t->attr.name = name;
      t->type = Integer;
    }
  }
  
  return t;
}

TreeNode * args(void){
  if(token == RPAREN) return NULL;
  else return args_list();
}

TreeNode * args_list(void){
  TreeNode *t = expr();
  TreeNode *p = t;

  if(t != NULL){
    while(token == COMMA){
      match(COMMA);
      TreeNode *q = expr();
      if(q != NULL){
        p->sibling = q;
        p = q;
      }
    }
  }

  return t;
}

/****************************************/
/* the primary function of the parser   */
/****************************************/
/* Function parse returns the newly 
 * constructed syntax tree
 */
TreeNode * parse(void)
{ TreeNode * t;
  token = getToken();
  t = decl_list();
  if (token!=ENDFILE)
    syntaxError("Code ends before file\n");
  return t;
}
