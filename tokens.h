#ifndef tokens_h
#define tokens_h
/* tokens.h -- List of labelled tokens and stuff
 *
 * Generated from: grammar.g
 *
 * Terence Parr, Will Cohen, and Hank Dietz: 1989-2001
 * Purdue University Electrical Engineering
 * ANTLR Version 1.33MR33
 */
#define zzEOF_TOKEN 1
#define INPUT 2
#define ASS 3
#define EMPTY 4
#define PUSH 5
#define POP 6
#define SIZE 7
#define IF 8
#define THEN 9
#define ELSE 10
#define WHILE 11
#define DO 12
#define END 13
#define EQ 14
#define GTHAN 15
#define AND 16
#define OR 17
#define NOT 18
#define PLUS 19
#define MINUS 20
#define TIMES 21
#define PRINT 22
#define NUM 23
#define ID 24
#define SPACE 25

#ifdef __USE_PROTOS
void seq(AST**_root);
#else
extern void seq();
#endif

#ifdef __USE_PROTOS
void command(AST**_root);
#else
extern void command();
#endif

#ifdef __USE_PROTOS
void def(AST**_root);
#else
extern void def();
#endif

#ifdef __USE_PROTOS
void print(AST**_root);
#else
extern void print();
#endif

#ifdef __USE_PROTOS
void input(AST**_root);
#else
extern void input();
#endif

#ifdef __USE_PROTOS
void assign(AST**_root);
#else
extern void assign();
#endif

#ifdef __USE_PROTOS
void cond(AST**_root);
#else
extern void cond();
#endif

#ifdef __USE_PROTOS
void loop(AST**_root);
#else
extern void loop();
#endif

#ifdef __USE_PROTOS
void bexpr(AST**_root);
#else
extern void bexpr();
#endif

#ifdef __USE_PROTOS
void bexpr2(AST**_root);
#else
extern void bexpr2();
#endif

#ifdef __USE_PROTOS
void bexpr3(AST**_root);
#else
extern void bexpr3();
#endif

#ifdef __USE_PROTOS
void nexpr(AST**_root);
#else
extern void nexpr();
#endif

#ifdef __USE_PROTOS
void term1(AST**_root);
#else
extern void term1();
#endif

#ifdef __USE_PROTOS
void term2(AST**_root);
#else
extern void term2();
#endif

#ifdef __USE_PROTOS
void stackop(AST**_root);
#else
extern void stackop();
#endif

#ifdef __USE_PROTOS
void push(AST**_root);
#else
extern void push();
#endif

#ifdef __USE_PROTOS
void pop(AST**_root);
#else
extern void pop();
#endif

#ifdef __USE_PROTOS
void empty(AST**_root);
#else
extern void empty();
#endif

#ifdef __USE_PROTOS
void size(AST**_root);
#else
extern void size();
#endif

#endif
extern SetWordType zzerr1[];
extern SetWordType zzerr2[];
extern SetWordType setwd1[];
extern SetWordType zzerr3[];
extern SetWordType zzerr4[];
extern SetWordType zzerr5[];
extern SetWordType setwd2[];
extern SetWordType zzerr6[];
extern SetWordType zzerr7[];
extern SetWordType zzerr8[];
extern SetWordType setwd3[];
extern SetWordType setwd4[];
