/*
 * A n t l r  T r a n s l a t i o n  H e a d e r
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * With AHPCRC, University of Minnesota
 * ANTLR Version 1.33MR33
 *
 *   antlr -gt grammar.g
 *
 */

#define ANTLR_VERSION	13333
#include "pcctscfg.h"
#include "pccts_stdio.h"

#include <string>
#include <iostream>
#include <map>
using namespace std;

// struct to store information about tokens
typedef struct {
  string kind;
  string text;
} Attrib;

// function to fill token information (predeclaration)
void zzcr_attr(Attrib *attr, int type, char *text);

// fields for AST nodes
#define AST_FIELDS string kind; string text;
#include "ast.h"

// macro to create a new AST node (and function predeclaration)
#define zzcr_ast(as,attr,ttype,textt) as=createASTnode(attr,ttype,textt)
AST* createASTnode(Attrib* attr,int ttype, char *textt);
#define GENAST

#include "ast.h"

#define zzSET_SIZE 4
#include "antlr.h"
#include "tokens.h"
#include "dlgdef.h"
#include "mode.h"

/* MR23 In order to remove calls to PURIFY use the antlr -nopurify option */

#ifndef PCCTS_PURIFY
#define PCCTS_PURIFY(r,s) memset((char *) &(r),'\0',(s));
#endif

#include "ast.c"
zzASTgvars

ANTLR_INFO

#include <cstdlib>
#include <cmath>

//global structures
AST *root;


// function to fill token information
void zzcr_attr(Attrib *attr, int type, char *text) {
  if (type == ID) {
    attr->kind = "ID";
    attr->text = text;
  }
  else if (type == NUM){
    attr->kind = "NUM";
    attr->text = text;
  }
  else {
    attr->kind = text;
    attr->text = "";
  }
}

// function to create a new AST node
AST* createASTnode(Attrib* attr, int type, char* text) {
  AST* as = new AST;
  as->kind = attr->kind;
  as->text = attr->text;
  as->right = NULL;
  as->down = NULL;
  return as;
}


/// create a new "list" AST node with one element
AST* createASTlist(AST *child) {
  AST *as=new AST;
  as->kind="list";
  as->right=NULL;
  as->down=child;
  return as;
}

/// get nth child of a tree. Count starts at 0.
/// if no such child, returns NULL
AST* child(AST *a,int n) {
  AST *c=a->down;
  for (int i=0; c!=NULL && i<n; i++) c=c->right;
  return c;
}

void printCond(AST *a){
  if(a->kind == "NOT"){
    cout << " NOT (" ;
    printCond(child(a,0));
    cout << " )";
  }
  else if(a->kind == "NUM" || a->kind == "ID"){
    if(a->kind == "NUM") cout << " Const " << atoi(a->text.c_str());
    else cout << " Var \"" << a->text << "\"";
  }
  else{  
    if (a->kind == ">") cout << " Gt";
    else if(a->kind == "=") cout << " Eq";
    else if(a->kind == "+") cout << " Plus";
    else if(a->kind == "-") cout << " Minus";
    else if(a->kind == "*") cout << " Times";
    else cout << " " << a->kind;
    cout << " (";
    printCond(child(a,0));
    cout << " )";
    cout << " (";
    printCond(child(a,1));
    cout << " )";
  }
}


/// print AST, recursively, with indentation
void ASTPrintIndent(AST *a){
  
  if (a==NULL) return;
  string token = a->kind;
  if (token == "IF"){
    cout << " Cond (";
    printCond(child(a,0));
    cout << " ) (";
    ASTPrintIndent(child(a,1));
    cout << " ) (";
    ASTPrintIndent(child(a,2));
    cout << " )" ;
  }
  else if(token == "INPUT"){
    cout << " Input \"" << child(a,0)->text << "\"";
  }
  else if( token == "PRINT"){
    cout << " Print (Var \"" << child(a,0)->text << "\" )";
  }
  else if (token == "WHILE"){
    cout << " Loop";
    cout << " (";
    printCond(child(a,0));
    cout << " ) (";
    ASTPrintIndent(child(a,1));
    cout << " )" ;
  }
  else if(token == ":="){
    cout << " Assign \"" << child(a,0)->text << "\" (";
    printCond(child(a,1));
    cout << " )"; 
  }
  
  else if(token == "PUSH"){
    cout << " Push \"" << child(a,0)->text << "\" (";
    printCond(child(a,1));
    cout << " )"; 
  }
  
  else if(token == "POP"){
    cout << " Pop \"" << child(a,0)->text << "\" \"" << child(a,1)->text << "\"";
  }
  
  else if (token == "SIZE"){
    cout << " Size \"" << child(a,0)->text << "\" \"" << child(a,1)->text << "\"";
  }
  
  else if (token == "EMPTY"){
    cout << " Empty \"" << child(a,0)->text << "\"";
  }
  
  else if (token == "list"){
    cout << " Seq [";
    int aux = 0;
    AST *i = child(a,aux);
    if (i != NULL){
      ASTPrintIndent(i);
      ++aux;
      i = child(a,aux);
    }    
    while(i != NULL){
      cout << " , ";
      ASTPrintIndent(i);
      ++aux;
      i = child(a,aux);
    }
    cout << " ]";
  }
  
  // cout<<a->kind;
  // if (a->text!="") cout<<"("<<a->text<<")";
  // cout<<endl;
  /*
  AST *i = a->down;
  while (i!=NULL && i->right!=NULL) {
    //cout<<s+"  \\__";
    ASTPrintIndent(i);
    i=i->right;
  }
  
  if (i!=NULL) {
    //cout<<s+"  \\__";
    ASTPrintIndent(i);
    //i=i->right;
  }*/
}

    //atoi(a->kind.c_str());

/// print AST
void ASTPrint(AST *a)
{
  while (a!=NULL) {
    //cout<<"  ";
    ASTPrintIndent(a);
    a=a->right;
  }
}


bool calc_difference(AST *a1, AST *a2) {
  if(a1->kind == ";" || a1->kind == "+" || a1->kind == "#" || a1->kind == "|") {
    if(a1->kind == a2->kind) return (calc_difference(child(a1,0), child(a2,0)) || calc_difference(child(a1,1), child(a2,1)));
    //Fem "or" perque ens demanen la sortida "invertida":
    //"false" si son iguals o "true" si son diferents. Llavors
    //basta amb que una sigui different o "true" per a que el
    //resultat sigui "true"
    return true;
  }
  if(a2->kind == ";" || a2->kind == "+" || a2->kind == "#" || a2->kind == "|") {
    if(a2->kind == a1->kind) return (calc_difference(child(a1,0), child(a2,0)) || calc_difference(child(a1,1), child(a2,1)));
    //Fem "or" perque ens demanen la sortida "invertida"
    return true;
  }
  return false;
  //Si son IDs, considerem que son iguals
}

int calc_critical(AST *a) {
  if(a->kind == ";")
  return ( calc_critical(child(a,0)) + calc_critical(child(a,1)) );
  //Si es op sequencial, retorna la suma de camins
  if(a->kind == "+" || a->kind == "#" || a->kind == "|") {
    int left, right;
    left = calc_critical(child(a,0));
    right = calc_critical(child(a,1));
    if(right>left) return right;
    return left;
    //Si es op no sequencial, retorna el max
  }
  return 1;
  //Si no es op, es a dir, si es ID, retorna 1
  //(la mida d'una sola tasca)
}

int critical(string role) {
AST *a = child(child(root,0),0);
while(a!=NULL) {
//Aqui s'entra nomes si existeixen rols
if(a->kind==role) return calc_critical(child(a,0));
a = a->right;
}
return -1;
}

bool difference(string role1, string role2) {
AST *a = NULL;
AST *b = NULL;

  for(AST *it=child(child(root,0),0); it!=NULL && (a==NULL || b==NULL); it=it->right) {
//Aqui s'entra nomes si existeixen rols
if(a==NULL && it->kind==role1) a = it;
if(b==NULL && it->kind==role2) b = it;
}
if(a!=NULL && b!=NULL) return calc_difference(child(a,0), child(b,0));
if(a!=NULL || b!=NULL) return true;
return false;
}

void recorre(AST *a) {
while (a!=NULL) {
if (a->kind == "critical") {
cout << "Critical " << child(a,0)->kind << " : " << critical(child(a,0)->kind) << endl;
}
else if (a->kind == "difference") {
cout << "Difference: " << child(a,0)->kind << " and " << child(a,1)->kind<< " : " << difference(child(a,0)->kind,child(a,1)->kind) << endl;
}
a=a->right;
}
}



int main() {
root = NULL;
ANTLR(seq(&root), stdin);
ASTPrint(root);
recorre(child(child(root,1),0));
}

void
#ifdef __USE_PROTOS
seq(AST**_root)
#else
seq(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd1[LA(1)]&0x1) ) {
      command(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  (*_root)=createASTlist(_sibling);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x2);
  }
}

void
#ifdef __USE_PROTOS
command(AST**_root)
#else
command(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (setwd1[LA(1)]&0x4) ) {
    def(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==IF) ) {
      cond(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (LA(1)==WHILE) ) {
        loop(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {
        if ( (setwd1[LA(1)]&0x8) ) {
          stackop(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {
          if ( (LA(1)==PRINT) ) {
            print(zzSTR); zzlink(_root, &_sibling, &_tail);
          }
          else {zzFAIL(1,zzerr1,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
        }
      }
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x10);
  }
}

void
#ifdef __USE_PROTOS
def(AST**_root)
#else
def(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==ID) ) {
    assign(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==INPUT) ) {
      input(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {zzFAIL(1,zzerr2,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x20);
  }
}

void
#ifdef __USE_PROTOS
stackop(AST**_root)
#else
stackop(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==EMPTY) ) {
    empty(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==PUSH) ) {
      push(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {
      if ( (LA(1)==POP) ) {
        pop(zzSTR); zzlink(_root, &_sibling, &_tail);
      }
      else {
        if ( (LA(1)==SIZE) ) {
          size(zzSTR); zzlink(_root, &_sibling, &_tail);
        }
        else {zzFAIL(1,zzerr3,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
      }
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x40);
  }
}

void
#ifdef __USE_PROTOS
print(AST**_root)
#else
print(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(PRINT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd1, 0x80);
  }
}

void
#ifdef __USE_PROTOS
input(AST**_root)
#else
input(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(INPUT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x1);
  }
}

void
#ifdef __USE_PROTOS
assign(AST**_root)
#else
assign(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ASS); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  nexpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x2);
  }
}

void
#ifdef __USE_PROTOS
cond(AST**_root)
#else
cond(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(IF); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  bexpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(THEN);  zzCONSUME;
  seq(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(ELSE);  zzCONSUME;
  seq(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(END);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x4);
  }
}

void
#ifdef __USE_PROTOS
loop(AST**_root)
#else
loop(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(WHILE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  bexpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(DO);  zzCONSUME;
  seq(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzmatch(END);  zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x8);
  }
}

void
#ifdef __USE_PROTOS
bexpr(AST**_root)
#else
bexpr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  bexpr2(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd2[LA(1)]&0x10) ) {
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        if ( (LA(1)==AND) ) {
          zzmatch(AND); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
        }
        else {
          if ( (LA(1)==OR) ) {
            zzmatch(OR); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
          }
          else {zzFAIL(1,zzerr4,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
        }
        zzEXIT(zztasp3);
        }
      }
      bexpr2(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x20);
  }
}

void
#ifdef __USE_PROTOS
bexpr2(AST**_root)
#else
bexpr2(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (setwd2[LA(1)]&0x40) ) {
    bexpr3(zzSTR); zzlink(_root, &_sibling, &_tail);
  }
  else {
    if ( (LA(1)==NOT) ) {
      zzmatch(NOT); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      bexpr3(zzSTR); zzlink(_root, &_sibling, &_tail);
    }
    else {zzFAIL(1,zzerr5,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd2, 0x80);
  }
}

void
#ifdef __USE_PROTOS
bexpr3(AST**_root)
#else
bexpr3(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  nexpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    if ( (LA(1)==EQ) ) {
      zzmatch(EQ); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {
      if ( (LA(1)==GTHAN) ) {
        zzmatch(GTHAN); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      }
      else {zzFAIL(1,zzerr6,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
    }
    zzEXIT(zztasp2);
    }
  }
  nexpr(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x1);
  }
}

void
#ifdef __USE_PROTOS
nexpr(AST**_root)
#else
nexpr(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  term1(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (LA(1)==TIMES) ) {
      zzmatch(TIMES); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
      term1(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x2);
  }
}

void
#ifdef __USE_PROTOS
term1(AST**_root)
#else
term1(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  term2(zzSTR); zzlink(_root, &_sibling, &_tail);
  {
    zzBLOCK(zztasp2);
    zzMake0;
    {
    while ( (setwd3[LA(1)]&0x4) ) {
      {
        zzBLOCK(zztasp3);
        zzMake0;
        {
        if ( (LA(1)==PLUS) ) {
          zzmatch(PLUS); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
        }
        else {
          if ( (LA(1)==MINUS) ) {
            zzmatch(MINUS); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
          }
          else {zzFAIL(1,zzerr7,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
        }
        zzEXIT(zztasp3);
        }
      }
      term2(zzSTR); zzlink(_root, &_sibling, &_tail);
      zzLOOP(zztasp2);
    }
    zzEXIT(zztasp2);
    }
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x8);
  }
}

void
#ifdef __USE_PROTOS
term2(AST**_root)
#else
term2(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  if ( (LA(1)==ID) ) {
    zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  }
  else {
    if ( (LA(1)==NUM) ) {
      zzmatch(NUM); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
    }
    else {zzFAIL(1,zzerr8,&zzMissSet,&zzMissText,&zzBadTok,&zzBadText,&zzErrk); goto fail;}
  }
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x10);
  }
}

void
#ifdef __USE_PROTOS
push(AST**_root)
#else
push(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(PUSH); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  term2(zzSTR); zzlink(_root, &_sibling, &_tail);
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x20);
  }
}

void
#ifdef __USE_PROTOS
pop(AST**_root)
#else
pop(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(POP); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x40);
  }
}

void
#ifdef __USE_PROTOS
empty(AST**_root)
#else
empty(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(EMPTY); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd3, 0x80);
  }
}

void
#ifdef __USE_PROTOS
size(AST**_root)
#else
size(_root)
AST **_root;
#endif
{
  zzRULE;
  zzBLOCK(zztasp1);
  zzMake0;
  {
  zzmatch(SIZE); zzsubroot(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzmatch(ID); zzsubchild(_root, &_sibling, &_tail); zzCONSUME;
  zzEXIT(zztasp1);
  return;
fail:
  zzEXIT(zztasp1);
  zzsyn(zzMissText, zzBadTok, (ANTLRChar *)"", zzMissSet, zzMissTok, zzErrk, zzBadText);
  zzresynch(setwd4, 0x1);
  }
}
