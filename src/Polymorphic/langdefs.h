/*
 * Header file for langdefs.c.
 * Contains data definitions used by compiled programs.
 */

#ifndef LANGDEFS
#define LANGDEFS

typedef enum {
    false,
    true
} bool;

/*
 * Possible types of expression
 */
typedef enum {
    T_App,    // Function application
    T_Abs,    // Abstraction
    T_Var,    // Variable usage
    T_Con,    // Constant value e.g. 1, 'c', false
    T_Opn     // Operation e.g. +, head, xor
} ExpT;

/*
 * Possible expression values
 */
typedef union {
    struct App *app;
    struct Abs *abs;
    struct Var *var;
    struct Con *con;
    struct Opn *opn;
} ExpV;

/*
 * Exp represents an expression, carrying an ExpT so we know its type, and an
 * ExpV, an actual value.
 */
typedef struct Exp Exp;
struct Exp {
    ExpT type;
    ExpV *value;
};

/*
 * Function application - just pointers to the function and argument.
 */
typedef struct App App;
struct App {
    Exp *fun;
    Exp *arg;
};

/*
 * An abstraction stores its argument name, and the body expression.
 */
typedef struct Abs Abs;
struct Abs {
    Exp *body;
};


/*
 * A variable just carries its name.
 */
typedef struct Var Var;
struct Var {
    int bind;
};

typedef enum {
    C_Int,
    C_Bool,
    C_Char
} ConTy;

/*
 * A constant can be an int, a char or a bool. We check this at compile-time, so
 * at run-time, we don't need to know which it is.
 */
typedef struct Con Con;
struct Con {
    ConTy ty;
    int val;
};

/*
 * The possible types of operations.
 */
typedef enum {
    O_Cond,                             // Conditionals

    O_Add, O_Sub, O_Mul, O_Div, O_Mod,  // Arithmetic

    O_Lss, O_LsE, O_NEq, O_Gtr, O_GtE,  // Integer Comparison

    O_Equ,                              // Expression equality

    O_And, O_Or , O_Xor,                // Binary boolean operations

    O_Not, O_IsZ,                       // Unary operations

    O_Empty,                            // List operations
    O_Cons,
    O_Null,
    O_Head,
    O_Tail,

    O_Fix,                              // Fixed-point combinator

    O_InjL,                             // Sum-types
    O_InjR,
    O_RemL,
    O_RemR,

    O_Tuple,                            // Product types
    O_Fst,
    O_Snd
} OpTy;

/*
 * An operation just carries its type
 */
typedef struct Opn Opn;
struct Opn {
    OpTy type;
};

// DEFINED IN langdefs.h

/*
 * Allocate on the heap using malloc and assert that it was successful.
 */
void *ckMalloc(int size);

/*
 * Functions to allocate & deallocate new expressions on the heap.
 */
Exp *newApp(Exp *fun, Exp *arg);
Exp *newAbs(Exp *body);
Exp *newVar(int bind);
Exp *newCon(ConTy ty, int val);
Exp *newOpn(OpTy type);
void freeExp(Exp *exp);

/*
 * Functions to determine the type of an expression node.
 */
bool isApp(Exp *exp);
bool isAbs(Exp *exp);
bool isVar(Exp *exp);
bool isCon(Exp *exp);
bool isOpn(Exp *exp);
bool isBinaryOpn(Exp *exp);

/*
 * Determine whether or not two expressions are equal.
 */
bool expEqual(Exp *e1, Exp *e2);

/*
 * Copy an expression, return a pointer to the newly allocated expression.
 */
Exp *copyExp(Exp *exp);

/*
 * Print an expression to stdout, folled by a newline character.
 */
void printlnExp(Exp *exp);

/*
 * Print an expression to stdout.
 */
void printExp(Exp *exp);

/*
 * Member retrieval functions for expressions
 */
// Application
Exp *appFun(Exp *exp);
Exp *appArg(Exp *exp);

// Abstraction
Exp *absBody(Exp *exp);

// Variables
int varBind(Exp *exp);

// Constants
ConTy conTy(Exp *exp);
int conVal(Exp *exp);

// Operations
OpTy opnType(Exp *exp);

/*
 * Reduce the template referenced by the given pointer. If it was found to have
 * normal form i.e. no reduction could be made, sets the dereferenced value at
 * normalForm to false.
 */
void reduceTemplate(bool *normalForm, Exp **template);

/*
 * Perform reduction on a template until it reaches its normal form (when no
 * reduction rules are applicable).
 */
void reduceTemplateNorm(Exp **template);

/*
 * Copy an expression, but replace all occurences of a given variable with a
 * given expression.
 */
Exp *replace(Exp *body, int bind, Exp *arg);

// DEFINED IN compiled.c GENERATED AT COMPILE-TIME

/*
 * Instantiate on the heap a template for the function with the given index
 * number, and return a pointer to it.
 */
Exp *instantiate(int funcNo);

/*
 * Determine if the function with given index number exists
 */
bool hasFunc(int funcNo);

#endif // LANGDEFS
