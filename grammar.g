#header
<<
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
>>

<<
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
>>

#lexclass START
#token INPUT "INPUT"
#token ASS ":="
#token EMPTY "EMPTY"
#token PUSH "PUSH"
#token POP "POP"
#token SIZE "SIZE"
#token IF "IF"
#token THEN "THEN"
#token ELSE "ELSE"
#token WHILE "WHILE"
#token DO "DO"
#token END "END"
#token EQ "="
#token GTHAN ">"
#token AND "AND"
#token OR "OR"
#token NOT "NOT"
#token PLUS "\+"
#token MINUS "\-"
#token TIMES "\*"
#token PRINT "PRINT"
#token NUM "[0-9]+"
#token ID "[a-zA-Z][a-zA-Z0-9]*"
#token SPACE "[\ \n]" << zzskip();>>

seq: (command)* <<#0=createASTlist(_sibling);>>;
command: def | cond | loop | stackop | print;
def: assign | input;
stackop: empty | push | pop | size;
print: PRINT^ ID;
input: INPUT^ ID;
assign: ID ASS^ nexpr;
cond: IF^ bexpr THEN! seq ELSE! seq END!;
loop: WHILE^ bexpr DO! seq END!;
bexpr: bexpr2 ((AND^|OR^) bexpr2)*;
bexpr2: bexpr3 | NOT^ bexpr3;
bexpr3: nexpr (EQ^|GTHAN^) nexpr;
nexpr: term1 (TIMES^ term1)*;
term1: term2 ((PLUS^|MINUS^) term2)*;
term2: ID | NUM;
push: PUSH^ ID term2;
pop: POP^ ID ID;
empty: EMPTY^ ID;
size: SIZE^ ID ID;