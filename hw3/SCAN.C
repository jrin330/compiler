/****************************************************/
/* File: scan.c                                     */
/* The scanner implementation for the TINY compiler */
/* Compiler Construction: Principles and Practice   */
/* Kenneth C. Louden                                */
/****************************************************/

#include "globals.h"
#include "util.h"
#include "scan.h"

/* states in scanner DFA */
typedef enum
   { START,INASSIGN,INCOMMENT,INNUM,INID,DONE,INEQ,INLT,INGT,INNE,INOVER }
   StateType;

/* lexeme of identifier or reserved word */
char tokenString[MAXTOKENLEN+1];

/* BUFLEN = length of the input buffer for
   source code lines */
#define BUFLEN 256

static char lineBuf[BUFLEN]; /* holds the current line */
static int linepos = 0; /* current position in LineBuf */
static int bufsize = 0; /* current size of buffer string */
static int EOF_flag = FALSE; /* corrects ungetNextChar behavior on EOF */

/* getNextChar fetches the next non-blank character
   from lineBuf, reading in a new line if lineBuf is
   exhausted */
static int getNextChar(void)
{ if (!(linepos < bufsize))
  { lineno++;
    if (fgets(lineBuf,BUFLEN-1,source))
    { 
      bufsize = strlen(lineBuf);
      if (EchoSource) fprintf(stdout,"%4d: %s",lineno,lineBuf);
      linepos = 0;
      return lineBuf[linepos++];
    }
    else
    { EOF_flag = TRUE;
      lineno--;
      return EOF;
    }
  }
  else return lineBuf[linepos++];
}

/* ungetNextChar backtracks one character
   in lineBuf */
static void ungetNextChar(void)
{ if (!EOF_flag) linepos-- ;}

/* lookup table of reserved words */
static struct
    { char* str;
      TokenType tok;
    } reservedWords[MAXRESERVED]
   = {{"if",IF},{"else",ELSE},{"return",RETURN},
      {"int",INT},{"while",WHILE},{"void",VOID}};

/* lookup an identifier to see if it is a reserved word */
/* uses linear search */
static TokenType reservedLookup (char * s)
{ int i;
  for (i=0;i<MAXRESERVED;i++)
    if (!strcmp(s,reservedWords[i].str))
      return reservedWords[i].tok;
  return ID;
}

/****************************************/
/* the primary function of the scanner  */
/****************************************/
/* function getToken returns the 
 * next token in source file
 */
TokenType getToken(void)
{  /* index for storing into tokenString */
   int tokenStringIndex = 0;
   /* holds current token to be returned */
   TokenType currentToken;
   /* current state - always begins at START */
   StateType state = START;
   /* flag to indicate save to tokenString */
   int save;
   while (state != DONE)
   { int c = getNextChar();
     save = TRUE;
     switch (state)
     { case START:
         if (isdigit(c)){
           state = INNUM;
         }
         else if (isalpha(c)){
           state = INID;
         }
         else if ((c == ' ') || (c == '\t') || (c == '\n') || c == 13){
           save = FALSE;
         }
         else if(c == '/'){
          state = INOVER;
         }
         else if (c == '='){
          state = INEQ;
         }
         else if(c == '<'){
          state = INLT;
         }
         else if(c == '>'){
          state = INGT;
         }
         else if(c == '!'){
          state = INNE;
         }
         else
         { state = DONE;
           switch (c)
           { case EOF:
               save = FALSE;
               currentToken = ENDFILE;
               break;
             case '+':
               currentToken = PLUS;
               break;
             case '-':
               currentToken = MINUS;
               break;
             case '*':
               currentToken = TIMES;
               break;
             case '(':
               currentToken = LPAREN;
               break;
             case ')':
               currentToken = RPAREN;
               break;
             case '{':
               currentToken = LCURLY;
               break;
             case '}':
               currentToken = RCURLY;
               break;
             case '[':
               currentToken = LBRACE;
               break;
             case ']':
               currentToken = RBRACE;
               break;
             case ',':
               currentToken = COMMA;
               break;
             case ';':
               currentToken = SEMI;
               break;
             default:
               currentToken = ERROR;
               break;
           }
         }
         break;
       case INOVER:
         if(c == '*'){ // /*
           state = INCOMMENT;
           save = FALSE;
           tokenStringIndex = 0;
         }
         else{
           ungetNextChar();
           state = DONE;
           currentToken = OVER;
         }
         break;
       case INEQ:
        state = DONE;
        if(c == '=') currentToken = EQ;
        else{
          currentToken = ASSIGN;
          ungetNextChar();
        }
        break;
       case INLT:
         state = DONE;
         if(c == '=') // <=
           currentToken = LE;
         else{
           currentToken = LT;
           ungetNextChar();
         }
         break;
       case INGT:
         state = DONE;
         if(c == '=') // >=
           currentToken = GE;
         else{
           currentToken = GT;
           ungetNextChar();
         }
         break;
       case INCOMMENT:
         save = FALSE;
         if(c == '*'){
           c = getNextChar();
           if(c == '/') {// */
             state = START;
           }
           else if(c == EOF){
             state = DONE;
             currentToken = ERROR;
             tokenStringIndex = 0;
           }
           else
             ungetNextChar();
         }
         else if(c == EOF){
           state = DONE;
           currentToken = ERROR;
           tokenStringIndex = 0;
         }
         break;
       case INASSIGN:
         state = DONE;
         if (c == '=')
           currentToken = EQ;
         else{
           save = FALSE;
           currentToken = ASSIGN;
           ungetNextChar();
         }
         break;
       case INNE:
         state = DONE;
         if(c == '=')
           currentToken = NE;
         else
           currentToken = ERROR;
         
         break;
       case INNUM:
         if (!isdigit(c))
         { /* backup in the input */
           ungetNextChar();
           save = FALSE;
           state = DONE;
           currentToken = NUM;
         }
         break;
       case INID:
         if (!isalpha(c))
         { /* backup in the input */
           ungetNextChar();
           save = FALSE;
           state = DONE;
           currentToken = ID;
         }
         break;
       case DONE:
       default: /* should never happen */
         fprintf(listing,"Scanner Bug: state= %d\n",state);
         state = DONE;
         currentToken = ERROR;
         break;
     }
     if ((save) && (tokenStringIndex <= MAXTOKENLEN)){
       tokenString[tokenStringIndex++] = (char) c;
     }
     if (state == DONE)
     { tokenString[tokenStringIndex] = '\0';
       if (currentToken == ID)
         currentToken = reservedLookup(tokenString);
     }
   }
   if (TraceScan) {
     fprintf(listing,"\t%d ",lineno);
     printToken(currentToken,tokenString);
   }
   return currentToken;
} /* end getToken */

